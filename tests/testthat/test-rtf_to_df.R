test_that("explicit \\trhdr header is detected and include_header works", {
  path <- test_path("fixtures", "trhdr_table.rtf")

  df_no_header <- rtf_to_df(path)
  expect_equal(names(df_no_header), c("V1", "V2"))
  expect_equal(df_no_header$V1, c("Alice", "Bob"))
  expect_equal(df_no_header$V2, c("10", "20"))

  df_header <- rtf_to_df(path, include_header = TRUE)
  expect_equal(names(df_header), c("Name", "Value"))
  expect_equal(df_header$Name, c("Alice", "Bob"))
})

test_that("r2rtf-style vertical-alignment header hints are detected", {
  path <- test_path("fixtures", "r2rtf_style_table.rtf")

  df <- rtf_to_df(path, include_header = TRUE)
  expect_equal(names(df), c("Treatment", "N"))
  expect_equal(df$Treatment, c("Placebo", "Drug"))
  expect_equal(df$N, c("50", "48"))
})

test_that("first body row is promoted to header when none is auto-detected", {
  path <- test_path("fixtures", "no_header_table.rtf")

  df <- rtf_to_df(path, include_header = TRUE)
  expect_equal(names(df), c("Col1", "Col2"))
  expect_equal(nrow(df), 1L)
  expect_equal(df$Col1, "X")
})

test_that("multiple tables are merged by default and selectable via table_index", {
  path <- test_path("fixtures", "multi_table.rtf")

  df_merged <- rtf_to_df(path, include_header = TRUE)
  expect_equal(nrow(df_merged), 2L)

  df_t1 <- rtf_to_df(path, include_header = TRUE, table_index = 1L)
  expect_equal(names(df_t1), c("A", "B"))
  expect_equal(df_t1$A, "1")

  df_t2 <- rtf_to_df(path, include_header = TRUE, table_index = 2L)
  expect_equal(names(df_t2), c("C", "D"))
  expect_equal(df_t2$C, "3")
})

test_that("header_row selects among multiple detected header rows", {
  path <- test_path("fixtures", "multi_table.rtf")

  df_first <- rtf_to_df(path, include_header = TRUE, header_row = 1L)
  expect_equal(names(df_first), c("A", "B"))

  df_last <- rtf_to_df(path, include_header = TRUE)
  expect_equal(names(df_last), c("C", "D"))
})

test_that("encoding argument controls how non-UTF-8 bytes are decoded", {
  path <- test_path("fixtures", "latin1_table.rtf")

  expect_error(rtf_to_df(path), "invalid")

  df <- rtf_to_df(path, include_header = TRUE, encoding = "latin1")
  expect_equal(df$Value, "5")
  expect_equal(df$Name, "Caf\u00e9")
})

test_that("a file with no table rows warns and returns an empty data frame", {
  path <- test_path("fixtures", "no_table.rtf")

  expect_warning(df <- rtf_to_df(path), "No table body rows found")
  expect_equal(df, data.frame())
})

test_that("invalid arguments are rejected", {
  expect_error(rtf_to_df(123), "`path` must be a single character string")
  expect_error(rtf_to_df("does-not-exist.rtf"), "File not found")

  path <- test_path("fixtures", "trhdr_table.rtf")
  expect_error(rtf_to_df(path, include_header = "yes"), "`include_header` must be TRUE or FALSE")
  expect_error(
    rtf_to_df(test_path("fixtures", "multi_table.rtf"), table_index = 5L),
    "out of range"
  )
})
