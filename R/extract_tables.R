# RTF destination groups to skip entirely - these may contain title/footer text
# that must not bleed into cell data. Starred destinations (\*) are also skipped.
.KNOWN_DEST <- c( # nolint: object_name_linter
  "\\fonttbl", "\\colortbl", "\\stylesheet",
  "\\info", "\\listtable", "\\listoverridetable",
  "\\rsidtbl", "\\revtbl", "\\pict", "\\object",
  "\\header", "\\footer",
  "\\headerl", "\\headerr", "\\headerf",
  "\\footerl", "\\footerr", "\\footerf",
  "\\footnote", "\\comment", "\\annotation"
)

#' Extract table rows from a tokenized RTF stream
#'
#' State machine over the token vector produced by [rtf_tokenize()].
#'
#' @param tokens Character vector from [rtf_tokenize()].
#' @param hex_encoding iconv-compatible encoding used to decode `\'XX` hex
#'   escapes. Detected from `\ansicpgN` by `detect_ansicpg()`. **Default:** `"CP1252"`.
#' @return A list with three elements:
#'   * `header_rows` / `body_rows`: lists of character vectors (one per row;
#'     one element per cell) with all tables merged -- the existing single-table
#'     interface.
#'   * `all_tables`: list of per-table `list(header_rows, body_rows)` for
#'     files that contain multiple logically distinct tables.
#' @keywords internal
rtf_extract_tables <- function(tokens, hex_encoding = "CP1252") {
  n <- length(tokens)

  # --- state ---
  state <- "OUTSIDE"
  group_depth <- 0L
  skip_depth <- -1L
  prev_open_brace <- FALSE

  # --- row-level ---
  in_row_def <- FALSE
  row_is_header <- FALSE
  row_has_vertalb <- FALSE
  row_has_vertalt <- FALSE

  # --- per-cell merge queues ------------------------------------------------
  # Filled during the row definition phase (one entry per \cellx encountered).
  # Consumed in order at each \cell during the content phase.
  # h_merge_q: TRUE = \clmrg horizontal-merge continuation (emit "")
  # v_merge_q: TRUE = \clvmrg vertical-merge continuation (emit "")
  h_merge_q <- logical(0)
  v_merge_q <- logical(0)
  h_pend <- FALSE # pending flag for the cell currently being defined
  v_pend <- FALSE
  cell_out_idx <- 0L # next \cell index (1-based) within current row

  # --- accumulation ---
  cell_parts <- character(0)
  current_row <- list()
  all_rows <- list()
  row_hdr_flags <- logical(0)
  row_cell_counts <- integer(0)

  # --- multi-table detection ------------------------------------------------
  # A new table group starts when a \trowd follows visible content in OUTSIDE
  # state (plain text, \par, \pard). Paginated continuations of the same table
  # have only formatting tokens between \row and the next \trowd.
  table_group_id <- 1L
  content_since_row <- FALSE
  row_table_groups <- integer(0)

  # --- unicode handling ---
  uc_skip <- 1L
  skip_uc_n <- 0L
  hex_pending <- FALSE

  for (i in seq_len(n)) {
    t <- tokens[[i]]
    trimmed <- sub("[ \t]+$", "", t, perl = TRUE)

    # -- Group depth ----------------------------------------------------------
    if (t == "{") {
      group_depth <- group_depth + 1L
      prev_open_brace <- TRUE
      next
    }
    if (t == "}") {
      if (skip_depth >= 0L && group_depth == skip_depth) skip_depth <- -1L
      group_depth <- group_depth - 1L
      prev_open_brace <- FALSE
      next
    }

    # -- Named destination detection (retroactive group skip) -----------------
    if (prev_open_brace && trimmed %in% .KNOWN_DEST && skip_depth < 0L) {
      skip_depth <- group_depth
    }
    prev_open_brace <- FALSE

    if (trimmed == "\\*") {
      skip_depth <- group_depth + 1L
      next
    }

    if (skip_depth >= 0L && group_depth >= skip_depth) next

    # -- OUTSIDE --------------------------------------------------------------
    if (state == "OUTSIDE") {
      if (trimmed == "\\trowd") {
        if (content_since_row) {
          table_group_id <- table_group_id + 1L
          content_since_row <- FALSE
        }
        state <- "IN_ROW"
        in_row_def <- TRUE
        row_is_header <- FALSE
        row_has_vertalb <- FALSE
        row_has_vertalt <- FALSE
        h_merge_q <- logical(0)
        v_merge_q <- logical(0)
        h_pend <- FALSE
        v_pend <- FALSE
        cell_out_idx <- 0L
        cell_parts <- character(0)
        current_row <- list()
      } else if (trimmed == "\\par" || trimmed == "\\pard" || (!startsWith(t, "\\") && nchar(trimws(t)) > 0L)) {
        content_since_row <- TRUE
      }
      next
    }

    # -- IN_ROW ---------------------------------------------------------------

    if (in_row_def) {
      # Row definition phase: collect cell properties, end at \pard
      if (trimmed == "\\trhdr") {
        row_is_header <- TRUE
      } else if (trimmed == "\\clvertalb") {
        row_has_vertalb <- TRUE
      } else if (trimmed == "\\clvertalt") {
        row_has_vertalt <- TRUE
      } else if (trimmed == "\\clmgf") {
        h_pend <- FALSE
      } else if (trimmed == "\\clmrg") {
        h_pend <- TRUE
      } else if (trimmed == "\\clvmgf") {
        v_pend <- FALSE
      } else if (trimmed == "\\clvmrg") {
        v_pend <- TRUE
      } else if (grepl("^\\\\cellx", trimmed, perl = TRUE)) {
        # \cellxN marks the right boundary of the current cell definition.
        # Commit the pending merge flags and reset for the next cell.
        h_merge_q <- c(h_merge_q, h_pend)
        v_merge_q <- c(v_merge_q, v_pend)
        h_pend <- FALSE
        v_pend <- FALSE
      } else if (trimmed == "\\trowd") {
        # Repeated \trowd in row definition block -- restart row state
        row_is_header <- FALSE
        row_has_vertalb <- FALSE
        row_has_vertalt <- FALSE
        h_merge_q <- logical(0)
        v_merge_q <- logical(0)
        h_pend <- FALSE
        v_pend <- FALSE
        cell_out_idx <- 0L
        cell_parts <- character(0)
        current_row <- list()
      } else if (trimmed == "\\pard") {
        in_row_def <- FALSE
        # r2rtf heuristic: header rows use \clvertalb, body rows use \clvertalt
        if (!row_is_header && row_has_vertalb && !row_has_vertalt) {
          row_is_header <- TRUE
        }
      }
    } else {
      # Cell content phase

      if (trimmed == "\\cell") {
        cell_out_idx <- cell_out_idx + 1L
        is_cont <- (cell_out_idx <= length(h_merge_q) && h_merge_q[[cell_out_idx]]) ||
          (cell_out_idx <= length(v_merge_q) && v_merge_q[[cell_out_idx]])
        cell_text <- trimws(paste(cell_parts, collapse = ""))
        if (is_cont) {
          current_row <- c(current_row, list(""))
        } else {
          current_row <- c(current_row, list(cell_text))
        }
        cell_parts <- character(0)
        hex_pending <- FALSE
        skip_uc_n <- 0L
      } else if (trimmed == "\\row") {
        all_rows <- c(all_rows, list(current_row))
        row_hdr_flags <- c(row_hdr_flags, row_is_header)
        row_cell_counts <- c(row_cell_counts, length(current_row))
        row_table_groups <- c(row_table_groups, table_group_id)
        current_row <- list()
        cell_out_idx <- 0L
        state <- "OUTSIDE"
      } else if (trimmed == "\\trowd") {
        # New row definition encountered mid-content (multi-band row defs)
        in_row_def <- TRUE
        row_is_header <- FALSE
        row_has_vertalb <- FALSE
        row_has_vertalt <- FALSE
        h_merge_q <- logical(0)
        v_merge_q <- logical(0)
        h_pend <- FALSE
        v_pend <- FALSE
        cell_out_idx <- 0L
      } else if (trimmed == "\\par" || trimmed == "\\line") {
        cell_parts <- c(cell_parts, "\n")
      } else if (trimmed == "\\tab") {
        cell_parts <- c(cell_parts, "\t")
      } else if (trimmed == "\\endash") {
        cell_parts <- c(cell_parts, "\u2013")
      } else if (trimmed == "\\emdash") {
        cell_parts <- c(cell_parts, "\u2014")
      } else if (trimmed == "\\bullet") {
        cell_parts <- c(cell_parts, "\u2022")
      } else if (trimmed == "\\lquote") {
        cell_parts <- c(cell_parts, "\u2018")
      } else if (trimmed == "\\rquote") {
        cell_parts <- c(cell_parts, "\u2019")
      } else if (trimmed == "\\ldblquote") {
        cell_parts <- c(cell_parts, "\u201C")
      } else if (trimmed == "\\rdblquote") {
        cell_parts <- c(cell_parts, "\u201D")
      } else if (trimmed == "\\\\") {
        cell_parts <- c(cell_parts, "\\")
      } else if (trimmed == "\\{") {
        cell_parts <- c(cell_parts, "{")
      } else if (trimmed == "\\}") {
        cell_parts <- c(cell_parts, "}")
      } else if (trimmed == "\\~") {
        cell_parts <- c(cell_parts, "\u00A0") # non-breaking space
      } else if (trimmed == "\\_") {
        cell_parts <- c(cell_parts, "\u2011") # non-breaking hyphen
      } else if (trimmed == "\\-") {
        # optional hyphen - skip
      } else if (grepl("^\\\\uc[0-9]", trimmed, perl = TRUE)) {
        uc_skip <- cw_param(trimmed)
        if (is.na(uc_skip)) uc_skip <- 1L
      } else if (grepl("^\\\\u-?[0-9]", trimmed, perl = TRUE)) {
        n_val <- cw_param(trimmed)
        cell_parts <- c(cell_parts, decode_unicode(n_val))
        skip_uc_n <- uc_skip
      } else if (trimmed == "\\'" || t == "\\'") {
        hex_pending <- TRUE
      } else if (!startsWith(t, "\\")) {
        if (hex_pending) {
          if (nchar(t) >= 2L && grepl("^[0-9A-Fa-f]{2}", t, perl = TRUE)) {
            cell_parts <- c(cell_parts, decode_hex(substr(t, 1L, 2L), hex_encoding))
            rest <- substr(t, 3L, nchar(t))
            if (nchar(rest) > 0L) cell_parts <- c(cell_parts, rest)
          }
          # else: malformed/split hex escape - silently drop
          hex_pending <- FALSE
        } else if (skip_uc_n > 0L) {
          n_chars <- nchar(t)
          if (n_chars > skip_uc_n) {
            cell_parts <- c(cell_parts, substr(t, skip_uc_n + 1L, n_chars))
          }
          skip_uc_n <- max(0L, skip_uc_n - n_chars)
        } else {
          cell_parts <- c(cell_parts, t)
        }
      }
      # All other control words (formatting: \fs, \f, \b, \i, \qc, \li, \ri,
      # \sb, \sa, \fi, \cf, \intbl, \pard, \plain, \clvertalb, etc.) -> skip
    }
  }

  # -- Post-loop: classify rows per table group, then return ----------------
  if (length(all_rows) == 0L) {
    return(list(header_rows = list(), body_rows = list(), all_tables = list()))
  }

  classify_rows <- function(rows, hdr_flags, cell_counts) {
    non_hdr <- cell_counts[!hdr_flags]
    if (length(non_hdr) == 0L) {
      return(list(header_rows = rows[hdr_flags], body_rows = list()))
    }
    freq_table <- table(non_hdr)
    max_freq <- max(freq_table)
    candidates <- as.integer(names(freq_table)[freq_table == max_freq])
    body_n <- max(candidates)
    list(
      header_rows = rows[hdr_flags],
      body_rows   = rows[!hdr_flags & cell_counts == body_n]
    )
  }

  # Global result (all table groups merged) -- used when table_index is NULL
  global <- classify_rows(all_rows, row_hdr_flags, row_cell_counts)

  # Per-table breakdown for multi-table RTFs
  n_groups <- max(row_table_groups)
  all_tables <- vector("list", n_groups)
  for (g in seq_len(n_groups)) {
    mask <- row_table_groups == g
    all_tables[[g]] <- classify_rows(
      all_rows[mask], row_hdr_flags[mask], row_cell_counts[mask]
    )
  }

  list(
    header_rows = global$header_rows,
    body_rows   = global$body_rows,
    all_tables  = all_tables
  )
}
