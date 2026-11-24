#' Locate a named table block within comparedf summary text lines
#'
#' @param lines Character vector. Lines read via [readLines()] from a
#'   comparedf summary text file.
#' @param title Character scalar. Exact table title as it appears after
#'   `"Table: "` (e.g. `"Summary of overall comparison"`).
#' @return A list with `header_line`, `sep_line`, `data_lines` (character
#'   vector, possibly length 0), or `NULL` if the table title is not found
#'   or the block is malformed (missing header or separator line).
#' @keywords internal
find_table_block <- function(lines, title) {
  start_idx <- grep(paste0("^Table: ", title, "\\s*$"), lines)
  if (length(start_idx) == 0L) {
    return(NULL)
  }
  start_idx <- start_idx[1L]

  i <- start_idx + 1L
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) {
    i <- i + 1L
  }
  if (i > length(lines)) {
    return(NULL)
  }
  header_line <- lines[i]

  sep_idx <- i + 1L
  if (sep_idx > length(lines) || !grepl("^\\s*-[- ]*$", lines[sep_idx])) {
    return(NULL)
  }
  sep_line <- lines[sep_idx]

  j <- sep_idx + 1L
  data_lines <- character(0L)
  while (j <= length(lines) && nzchar(trimws(lines[j]))) {
    data_lines <- c(data_lines, lines[j])
    j <- j + 1L
  }

  list(header_line = header_line, sep_line = sep_line, data_lines = data_lines)
}

#' Parse a pandoc simple fixed-width table into a data frame
#'
#' Uses the dash runs in `sep_line` to determine exact column character
#' widths, which is robust to column values containing 2+ consecutive
#' spaces (unlike splitting on runs of whitespace).
#'
#' @param header_line Character scalar. The column-header line.
#' @param sep_line Character scalar. The dashes separator line directly
#'   below the header.
#' @param data_lines Character vector. Zero or more data row lines.
#' @return A `data.frame` of character columns, one row per `data_lines`
#'   element, with column names taken from `header_line`. Zero rows if
#'   `data_lines` has length 0.
#' @keywords internal
parse_fixed_width_table <- function(header_line, sep_line, data_lines) {
  dash_runs <- gregexpr("-+", sep_line)[[1L]]
  starts <- as.integer(dash_runs)
  widths <- attr(dash_runs, "match.length")
  ends <- starts + widths - 1L

  max_width <- max(nchar(c(header_line, sep_line, data_lines)))
  ends[length(ends)] <- max_width

  extract_cols <- function(line) {
    line <- formatC(line, width = max_width, flag = "-")
    trimws(substring(line, starts, ends))
  }

  col_names <- extract_cols(header_line)

  if (length(data_lines) == 0L) {
    df <- as.data.frame(
      matrix(character(0L), nrow = 0L, ncol = length(col_names)),
      stringsAsFactors = FALSE
    )
    names(df) <- col_names
    return(df)
  }

  rows <- lapply(data_lines, extract_cols)
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- col_names
  df
}

#' Parse an optional "detail" table that may collapse to a placeholder
#'
#' Several comparedf summary tables (e.g. "Non-identical attributes",
#' "Variables not shared") render as a real multi-column table when there is
#' something to report, but collapse to a single centered placeholder cell
#' (e.g. `"No non-identical attributes"`) bordered by its own dashes row when
#' there is nothing to report. This returns an empty `data.frame` for both
#' "placeholder" and "table not found" cases, and the real parsed table
#' otherwise.
#'
#' @param lines Character vector. Lines read via [readLines()] from a
#'   comparedf summary text file.
#' @param title Character scalar. Exact table title, as for
#'   [find_table_block()].
#' @return A `data.frame`, possibly with zero rows.
#' @keywords internal
parse_detail_table <- function(lines, title) {
  block <- find_table_block(lines, title)
  if (is.null(block) || length(block$data_lines) == 0L) {
    return(data.frame())
  }
  if (grepl("^No ", trimws(block$data_lines[1L]))) {
    return(data.frame())
  }
  # drop the placeholder's own closing border row, if present
  data_lines <- block$data_lines[!grepl("^[\\s-]*$", block$data_lines)]
  parse_fixed_width_table(block$header_line, block$sep_line, data_lines)
}

#' Parse a single comparedf summary text file into one result row
#'
#' @param path Character scalar. Path to a comparedf summary `.txt` file.
#' @param ignore_attrs Logical scalar. If `TRUE`, non-identical attributes
#'   are excluded from the `status` determination.
#' @return A named list suitable for one row of the [scan_comparedf()]
#'   result, or `NULL` if the "Summary of overall comparison" table block
#'   cannot be located.
#' @keywords internal
parse_comparedf_file <- function(path, ignore_attrs = FALSE) {
  lines <- readLines(path, warn = FALSE)

  overall_block <- find_table_block(lines, "Summary of overall comparison")
  if (is.null(overall_block)) {
    return(NULL)
  }
  overall <- parse_fixed_width_table(
    overall_block$header_line, overall_block$sep_line, overall_block$data_lines
  )
  stat_value <- function(statistic) {
    as.integer(overall$value[overall$statistic == statistic])
  }

  n_vars_compared <- stat_value("Number of variables compared")
  n_vars_x_not_y <- stat_value("Number of variables in x but not y")
  n_vars_y_not_x <- stat_value("Number of variables in y but not x")
  n_vars_some_unequal <- stat_value("Number of variables compared with some values unequal")
  n_vars_all_equal <- stat_value("Number of variables compared with all values equal")
  n_obs_common <- stat_value("Number of observations in common")
  n_obs_x_not_y <- stat_value("Number of observations in x but not y")
  n_obs_y_not_x <- stat_value("Number of observations in y but not x")
  n_obs_some_unequal <- stat_value("Number of observations with some compared variables unequal")
  n_obs_all_equal <- stat_value("Number of observations with all compared variables equal")
  n_values_unequal <- stat_value("Number of values unequal")

  vars_with_diffs <- NA_character_
  diffs_block <- find_table_block(lines, "Differences detected by variable")
  if (!is.null(diffs_block)) {
    diffs <- parse_fixed_width_table(
      diffs_block$header_line, diffs_block$sep_line, diffs_block$data_lines
    )
    if (nrow(diffs) > 0L) {
      has_diff <- as.integer(diffs$n) > 0L | as.integer(diffs$NAs) > 0L
      offending <- diffs$var.x[has_diff]
      if (length(offending) > 0L) {
        vars_with_diffs <- paste(offending, collapse = ", ")
      }
    }
  }

  attrs_with_diffs <- NA_character_
  attrs <- parse_detail_table(lines, "Non-identical attributes")
  n_attrs_diff <- nrow(attrs)
  if (n_attrs_diff > 0L) {
    attrs_with_diffs <- paste(unique(attrs$var.x), collapse = ", ")
  }

  is_match <- (
    n_vars_x_not_y == 0L && n_vars_y_not_x == 0L &&
      n_obs_x_not_y == 0L && n_obs_y_not_x == 0L &&
      n_values_unequal == 0L &&
      (ignore_attrs || n_attrs_diff == 0L)
  )
  status <- if (is_match) "Match" else "Discrepancies found"

  list(
    status = status,
    n_vars_compared = n_vars_compared,
    n_vars_x_not_y = n_vars_x_not_y,
    n_vars_y_not_x = n_vars_y_not_x,
    n_vars_some_unequal = n_vars_some_unequal,
    n_vars_all_equal = n_vars_all_equal,
    n_obs_common = n_obs_common,
    n_obs_x_not_y = n_obs_x_not_y,
    n_obs_y_not_x = n_obs_y_not_x,
    n_obs_some_unequal = n_obs_some_unequal,
    n_obs_all_equal = n_obs_all_equal,
    n_values_unequal = n_values_unequal,
    vars_with_diffs = vars_with_diffs,
    n_attrs_diff = n_attrs_diff,
    attrs_with_diffs = attrs_with_diffs
  )
}

#' Build an empty result row with NA values and a given status
#'
#' @param file Character scalar. The `file` column value for this row.
#' @param status Character scalar. The `status` column value for this row.
#' @return A named list with the same shape as [parse_comparedf_file()]'s
#'   return value, but with `file` set and every other field `NA`.
#' @keywords internal
na_comparedf_row <- function(file, status) {
  list(
    file = file,
    status = status,
    n_vars_compared = NA_integer_,
    n_vars_x_not_y = NA_integer_,
    n_vars_y_not_x = NA_integer_,
    n_vars_some_unequal = NA_integer_,
    n_vars_all_equal = NA_integer_,
    n_obs_common = NA_integer_,
    n_obs_x_not_y = NA_integer_,
    n_obs_y_not_x = NA_integer_,
    n_obs_some_unequal = NA_integer_,
    n_obs_all_equal = NA_integer_,
    n_values_unequal = NA_integer_,
    vars_with_diffs = NA_character_,
    n_attrs_diff = NA_integer_,
    attrs_with_diffs = NA_character_
  )
}

#' Build an empty, correctly-shaped scan_comparedf() result
#'
#' @return A 0-row `data.frame` with the same columns as [scan_comparedf()].
#' @keywords internal
empty_comparedf_scan <- function() {
  row <- na_comparedf_row(NA_character_, NA_character_)
  df <- as.data.frame(row, stringsAsFactors = FALSE)
  df[0L, , drop = FALSE]
}

#' Scan a folder of arsenal comparedf summaries into a data frame
#'
#' Reads every file matching `pattern` in `path` and parses it as the text
#' output of `summary(arsenal::comparedf(...))` (as produced by e.g.
#' `writeLines(capture.output(summary(cmp)), "file.txt")`), returning one
#' summary row per file with the key counts and the names of variables that
#' had detected differences.
#'
#' Files that do not contain a recognizable "Summary of overall comparison"
#' table are not fatal: `scan_comparedf()` emits a `warning()` naming the
#' offending file and still returns a row for it with `NA` in every numeric
#' column, `NA` in `vars_with_diffs`/`attrs_with_diffs`, and
#' `status = "Parse error"`.
#'
#' By default, a variable whose only discrepancy is a non-identical
#' attribute (e.g. a changed `label`, with every value still equal) is
#' reported as `"Discrepancies found"`. Set `ignore_attrs = TRUE` to exclude
#' attribute differences from the `status` determination, e.g. when only
#' value-level differences matter for a given check.
#'
#' @param path Character scalar. Path to the folder to scan. **Default:** `"."`
#' @param pattern Character scalar. Regular expression passed to
#'   [list.files()] to select which files are scanned. **Default:** `"\\.txt$"`
#' @param recursive Logical scalar. If `TRUE`, also scan matching files in
#'   subdirectories of `path`. Passed through to [list.files()]. **Default:** `FALSE`
#' @param full_names Logical scalar. If `TRUE`, the `file` column of the
#'   result contains full file paths; if `FALSE`, only the file's base name.
#'   **Default:** `FALSE`
#' @param ignore_attrs Logical scalar. If `TRUE`, non-identical attributes
#'   (from the "Non-identical attributes" table) do not count as a
#'   discrepancy when determining `status`. The `n_attrs_diff` and
#'   `attrs_with_diffs` columns are still populated either way.
#'   **Default:** `FALSE`
#'
#' After scanning, a `message()` reports how many files were compared, how
#' many matched, and how many had discrepancies (and, if any, how many
#' could not be parsed).
#'
#' @return A `data.frame` with one row per matched file, with columns:
#'   * `file`: character, file name or path (see `full_names`).
#'   * `status`: character, one of `"Match"`, `"Discrepancies found"`,
#'     `"Parse error"`.
#'   * `n_vars_compared`, `n_vars_x_not_y`, `n_vars_y_not_x`,
#'     `n_vars_some_unequal`, `n_vars_all_equal`: integer, from the
#'     "Summary of overall comparison" table.
#'   * `n_obs_common`, `n_obs_x_not_y`, `n_obs_y_not_x`,
#'     `n_obs_some_unequal`, `n_obs_all_equal`: integer.
#'   * `n_values_unequal`: integer.
#'   * `vars_with_diffs`: character, comma-separated names of variables
#'     (from "Differences detected by variable") with a detected diff;
#'     `NA` if none, or on parse error.
#'   * `n_attrs_diff`: integer, number of variables with a non-identical
#'     attribute (from "Non-identical attributes").
#'   * `attrs_with_diffs`: character, comma-separated names of variables
#'     with a non-identical attribute; `NA` if none, or on parse error.
#'
#' @examples
#' dir <- system.file("extdata", package = "otter")
#' scan_comparedf(dir)
#'
#' @export
scan_comparedf <- function(path = ".",
                           pattern = "\\.txt$",
                           recursive = FALSE,
                           full_names = FALSE,
                           ignore_attrs = FALSE) {
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a single character string.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Directory not found: ", path, call. = FALSE)
  }
  if (!is.character(pattern) || length(pattern) != 1L) {
    stop("`pattern` must be a single character string.", call. = FALSE)
  }
  if (!is.logical(recursive) || length(recursive) != 1L) {
    stop("`recursive` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(full_names) || length(full_names) != 1L) {
    stop("`full_names` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(ignore_attrs) || length(ignore_attrs) != 1L) {
    stop("`ignore_attrs` must be TRUE or FALSE.", call. = FALSE)
  }

  files <- list.files(path, pattern = pattern, recursive = recursive, full.names = TRUE)
  if (length(files) == 0L) {
    warning("No files matching pattern '", pattern, "' found in '", path, "'.", call. = FALSE)
    return(empty_comparedf_scan())
  }

  rows <- lapply(files, function(f) {
    display_name <- if (full_names) f else basename(f)
    parsed <- parse_comparedf_file(f, ignore_attrs = ignore_attrs)
    if (is.null(parsed)) {
      warning("Could not locate comparedf summary table in '", f, "'.", call. = FALSE)
      return(na_comparedf_row(display_name, "Parse error"))
    }
    c(list(file = display_name), parsed)
  })

  result <- do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE))

  n_match <- sum(result$status == "Match")
  n_diffs <- sum(result$status == "Discrepancies found")
  n_errors <- sum(result$status == "Parse error")
  summary_msg <- paste0(
    "Compared ", nrow(result), " file(s): ",
    n_match, " match", if (n_match != 1L) "es" else "", ", ",
    n_diffs, " with discrepancies"
  )
  if (n_errors > 0L) {
    summary_msg <- paste0(summary_msg, ", ", n_errors, " could not be parsed")
  }
  message(summary_msg, ".")

  result
}
