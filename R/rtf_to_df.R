#' Parse an RTF file and extract table body cells into a data frame
#'
#' Reads an RTF file and returns a `data.frame` of the body cell values.
#'
#' Header rows are detected automatically via `\trhdr` (explicit; Word/SAS) or via
#' cell vertical-alignment hints (`\clvertalb` / `\clvertalt`) used by [\{r2rtf\}](https://merck.github.io/r2rtf/).
#'
#' When `include_header = TRUE` and no header rows are auto-detected, the first
#' body row is promoted to column names and removed from the data (analogous to
#' `read.csv(header = TRUE)`).
#' Footnote and source rows are excluded by their cell count (they span the full
#' table width as a single cell).
#'
#' When an RTF file contains multiple logically distinct tables (separated by
#' paragraph text), use `table_index` to select a specific one. The default
#' (`NULL`) merges all tables into a single data frame, which is the correct
#' behaviour for paginated single-table outputs.
#'
#' @param path Character scalar. Path to the `.rtf` file.
#' @param include_header Logical scalar. If `TRUE`, column names
#'   are taken from the detected header row. When no header row is auto-detected,
#'   the first body row is used as column names and dropped from the data. **Default:** `FALSE`
#' @param header_row Integer scalar. Which header row to use as
#'   column names when `include_header = TRUE` and there are multiple detected header
#'   rows. Defaults to the last header row. Use `1L` for the first header row. **Default:** `NULL`
#' @param encoding Character scalar. Encoding passed to
#'   [readLines()] when reading the RTF file. Use `"latin1"` for older SAS RTF
#'   that is not UTF-8. **Default:** `"UTF-8"`
#' @param table_index Integer scalar. Index of the table to
#'   extract when the RTF contains multiple distinct tables. `1L` selects the
#'   first table, `2L` the second, etc. `NULL` merges all tables. **Default:** `NULL`
#'
#' @return A `data.frame` with one row per body row and one column per cell. All
#'   values are character strings. Column names are `V1`, `V2`, ... unless
#'   `include_header = TRUE`.
#'
#' @examples
#' path <- system.file("extdata", "example.rtf", package = "otter")
#' rtf_to_df(path)
#' rtf_to_df(path, include_header = TRUE)
#'
#' multi_path <- system.file("extdata", "multi_table_example.rtf", package = "otter")
#' rtf_to_df(multi_path, include_header = TRUE, table_index = 2L)
#'
#' @export
rtf_to_df <- function(path,
                      include_header = FALSE,
                      header_row = NULL,
                      encoding = "UTF-8",
                      table_index = NULL) {
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a single character string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  if (!is.logical(include_header) || length(include_header) != 1L) {
    stop("`include_header` must be TRUE or FALSE.", call. = FALSE)
  }

  rtf_text <- paste(
    readLines(path, encoding = encoding, warn = FALSE),
    collapse = "\n"
  )

  # r2rtf writes {line} as a literal group for line breaks inside column header
  # text. Convert to the standard \line control word before tokenizing.
  rtf_text <- gsub("{line}", "\\line ", rtf_text, fixed = TRUE)

  hex_encoding <- detect_ansicpg(rtf_text)
  tokens <- rtf_tokenize(rtf_text)
  extracted <- rtf_extract_tables(tokens, hex_encoding = hex_encoding)

  if (!is.null(table_index)) {
    ti <- as.integer(table_index)
    n_tables <- length(extracted$all_tables)
    if (ti < 1L || ti > n_tables) {
      stop("`table_index` ", ti, " is out of range: file contains ",
        n_tables, " table(s).",
        call. = FALSE
      )
    }
    header_rows <- extracted$all_tables[[ti]]$header_rows
    body_rows <- extracted$all_tables[[ti]]$body_rows
  } else {
    header_rows <- extracted$header_rows
    body_rows <- extracted$body_rows
  }

  # Remove body rows whose content exactly matches a detected header row.
  # This handles repeated page column-headers that were not classified via
  # alignment markers (\clvertalb / \trhdr).
  if (length(header_rows) > 0L && length(body_rows) > 0L) {
    row_key <- function(r) paste(as.character(unlist(r)), collapse = "\x01")
    hdr_keys <- vapply(header_rows, row_key, character(1L))
    body_keys <- vapply(body_rows, row_key, character(1L))
    body_rows <- body_rows[!body_keys %in% hdr_keys]
  }

  # When include_header = TRUE but no header rows were auto-detected, promote
  # the first body row to column names (like read.csv(header = TRUE)).
  promoted_header <- NULL
  if (include_header && length(header_rows) == 0L && length(body_rows) > 0L) {
    promoted_header <- body_rows[[1L]]
    body_rows <- body_rows[-1L]
  }

  if (length(body_rows) == 0L) {
    warning("No table body rows found in '", path, "'.", call. = FALSE)
    return(data.frame())
  }

  n_cols <- length(body_rows[[length(body_rows)]])

  rows_padded <- lapply(body_rows, function(r) {
    r <- as.character(unlist(r))
    length(r) <- n_cols
    r
  })
  df <- as.data.frame(
    do.call(rbind, rows_padded),
    stringsAsFactors = FALSE
  )

  default_names <- paste0("V", seq_len(n_cols))
  col_names <- default_names

  if (include_header) {
    hdr_source <- if (!is.null(promoted_header)) {
      promoted_header
    } else if (length(header_rows) > 0L) {
      use_row <- if (is.null(header_row)) {
        length(header_rows)
      } else {
        max(1L, min(as.integer(header_row), length(header_rows)))
      }
      header_rows[[use_row]]
    } else {
      NULL
    }

    if (!is.null(hdr_source)) {
      hdr_vals <- as.character(unlist(hdr_source))
      length(hdr_vals) <- n_cols
      hdr_vals[is.na(hdr_vals)] <- default_names[is.na(hdr_vals)]
      hdr_vals <- gsub("\n", " ", hdr_vals, fixed = TRUE)
      col_names <- make.unique(hdr_vals, sep = "_")
    }
  }

  names(df) <- col_names
  df
}
