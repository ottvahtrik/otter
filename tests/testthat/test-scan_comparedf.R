local_fixture_dir <- function(files) {
  dir <- tempfile("scan_comparedf_")
  dir.create(dir)
  for (f in files) {
    file.copy(test_path("fixtures", f), file.path(dir, f))
  }
  dir
}

test_that("a clean (fully matching) comparedf summary parses as Match", {
  dir <- local_fixture_dir("comparedf_clean.txt")
  on.exit(unlink(dir, recursive = TRUE))

  df <- suppressMessages(scan_comparedf(dir))
  expect_equal(nrow(df), 1L)
  expect_equal(df$file, "comparedf_clean.txt")
  expect_equal(df$status, "Match")
  expect_equal(df$n_vars_x_not_y, 0L)
  expect_equal(df$n_vars_y_not_x, 0L)
  expect_equal(df$n_values_unequal, 0L)
  expect_true(is.na(df$vars_with_diffs))
})

test_that("a comparedf summary with discrepancies parses correctly", {
  dir <- local_fixture_dir("comparedf_diffs.txt")
  on.exit(unlink(dir, recursive = TRUE))

  df <- suppressMessages(scan_comparedf(dir))
  expect_equal(nrow(df), 1L)
  expect_equal(df$status, "Discrepancies found")
  expect_equal(df$n_vars_x_not_y, 1L)
  expect_equal(df$n_vars_y_not_x, 1L)
  expect_equal(df$n_values_unequal, 1L)
  expect_equal(df$vars_with_diffs, "val")
})

test_that("multiple files are scanned and returned in list.files order", {
  dir <- local_fixture_dir(c("comparedf_clean.txt", "comparedf_diffs.txt"))
  on.exit(unlink(dir, recursive = TRUE))

  df <- suppressMessages(scan_comparedf(dir))
  expect_equal(nrow(df), 2L)
  expect_equal(df$file, c("comparedf_clean.txt", "comparedf_diffs.txt"))
  expect_equal(df$status, c("Match", "Discrepancies found"))
})

test_that("a non-identical attribute counts as a discrepancy by default", {
  dir <- local_fixture_dir("comparedf_attr_diff.txt")
  on.exit(unlink(dir, recursive = TRUE))

  df <- suppressMessages(scan_comparedf(dir))
  expect_equal(df$status, "Discrepancies found")
  expect_equal(df$n_values_unequal, 0L)
  expect_equal(df$n_attrs_diff, 1L)
  expect_equal(df$attrs_with_diffs, "val")
})

test_that("ignore_attrs excludes attribute differences from status", {
  dir <- local_fixture_dir("comparedf_attr_diff.txt")
  on.exit(unlink(dir, recursive = TRUE))

  df <- suppressMessages(scan_comparedf(dir, ignore_attrs = TRUE))
  expect_equal(df$status, "Match")
  expect_equal(df$n_attrs_diff, 1L)
  expect_equal(df$attrs_with_diffs, "val")
})

test_that("a clean comparedf summary has zero attribute differences", {
  dir <- local_fixture_dir("comparedf_clean.txt")
  on.exit(unlink(dir, recursive = TRUE))

  df <- suppressMessages(scan_comparedf(dir))
  expect_equal(df$n_attrs_diff, 0L)
  expect_true(is.na(df$attrs_with_diffs))
})

test_that("a file with no comparedf table warns and is reported as Parse error", {
  dir <- local_fixture_dir("not_comparedf.txt")
  on.exit(unlink(dir, recursive = TRUE))

  expect_warning(df <- suppressMessages(scan_comparedf(dir)), "Could not locate")
  expect_equal(nrow(df), 1L)
  expect_equal(df$status, "Parse error")
  expect_true(is.na(df$n_values_unequal))
  expect_true(is.na(df$vars_with_diffs))
})

test_that("a truncated comparedf file (missing overall-comparison table) is a Parse error", {
  dir <- local_fixture_dir("truncated_comparedf.txt")
  on.exit(unlink(dir, recursive = TRUE))

  expect_warning(df <- suppressMessages(scan_comparedf(dir)), "Could not locate")
  expect_equal(df$status, "Parse error")
})

test_that("a folder matching no files warns and returns an empty data frame", {
  dir <- tempfile("scan_comparedf_empty_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  expect_warning(df <- scan_comparedf(dir), "No files matching")
  expect_equal(nrow(df), 0L)
  expect_true(all(c("file", "status", "vars_with_diffs") %in% names(df)))
})

test_that("pattern argument filters which files are scanned", {
  dir <- local_fixture_dir("comparedf_clean.txt")
  on.exit(unlink(dir, recursive = TRUE))
  writeLines("not a comparedf file", file.path(dir, "notes.log"))

  df <- suppressMessages(scan_comparedf(dir))
  expect_equal(nrow(df), 1L)
  expect_equal(df$file, "comparedf_clean.txt")
})

test_that("full_names controls whether file column has full paths or basenames", {
  dir <- local_fixture_dir("comparedf_clean.txt")
  on.exit(unlink(dir, recursive = TRUE))

  df_base <- suppressMessages(scan_comparedf(dir, full_names = FALSE))
  expect_equal(df_base$file, "comparedf_clean.txt")

  df_full <- suppressMessages(scan_comparedf(dir, full_names = TRUE))
  expect_equal(df_full$file, file.path(dir, "comparedf_clean.txt"))
})

test_that("status is the second column in the result", {
  dir <- local_fixture_dir("comparedf_clean.txt")
  on.exit(unlink(dir, recursive = TRUE))

  df <- suppressMessages(scan_comparedf(dir))
  expect_equal(names(df)[1:2], c("file", "status"))
})

test_that("a summary message reports counts of matches, discrepancies, and parse errors", {
  dir <- local_fixture_dir(c("comparedf_clean.txt", "comparedf_diffs.txt", "not_comparedf.txt"))
  on.exit(unlink(dir, recursive = TRUE))

  expect_message(
    df <- suppressWarnings(scan_comparedf(dir)),
    "Compared 3 file\\(s\\): 1 match, 1 with discrepancies, 1 could not be parsed\\."
  )
  expect_equal(nrow(df), 3L)
})

test_that("the summary message pluralizes 'matches' correctly", {
  dir <- local_fixture_dir(c("comparedf_clean.txt", "comparedf_diffs.txt"))
  on.exit(unlink(dir, recursive = TRUE))

  expect_message(
    scan_comparedf(dir),
    "Compared 2 file\\(s\\): 1 match, 1 with discrepancies\\."
  )

  dir2 <- local_fixture_dir(c("comparedf_clean.txt"))
  file.copy(test_path("fixtures", "comparedf_clean.txt"), file.path(dir2, "comparedf_clean_2.txt"))
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)

  expect_message(
    scan_comparedf(dir2),
    "Compared 2 file\\(s\\): 2 matches, 0 with discrepancies\\."
  )
})

test_that("invalid arguments are rejected", {
  expect_error(scan_comparedf(123), "`path` must be a single character string")
  expect_error(scan_comparedf("does-not-exist-dir"), "Directory not found")

  dir <- local_fixture_dir("comparedf_clean.txt")
  on.exit(unlink(dir, recursive = TRUE))

  expect_error(scan_comparedf(dir, pattern = 123), "`pattern` must be a single character string")
  expect_error(scan_comparedf(dir, recursive = "yes"), "`recursive` must be TRUE or FALSE")
  expect_error(scan_comparedf(dir, full_names = "yes"), "`full_names` must be TRUE or FALSE")
  expect_error(scan_comparedf(dir, ignore_attrs = "yes"), "`ignore_attrs` must be TRUE or FALSE")
})
