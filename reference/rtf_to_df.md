# Parse an RTF file and extract table body cells into a data frame

Reads an RTF file and returns a `data.frame` of the body cell values.

## Usage

``` r
rtf_to_df(
  path,
  include_header = FALSE,
  header_row = NULL,
  encoding = "UTF-8",
  table_index = NULL
)
```

## Arguments

- path:

  Character scalar. Path to the `.rtf` file.

- include_header:

  Logical scalar. If `TRUE`, column names are taken from the detected
  header row. When no header row is auto-detected, the first body row is
  used as column names and dropped from the data. **Default:** `FALSE`

- header_row:

  Integer scalar. Which header row to use as column names when
  `include_header = TRUE` and there are multiple detected header rows.
  Defaults to the last header row. Use `1L` for the first header row.
  **Default:** `NULL`

- encoding:

  Character scalar. Encoding passed to
  [`readLines()`](https://rdrr.io/r/base/readLines.html) when reading
  the RTF file. Use `"latin1"` for older SAS RTF that is not UTF-8.
  **Default:** `"UTF-8"`

- table_index:

  Integer scalar. Index of the table to extract when the RTF contains
  multiple distinct tables. `1L` selects the first table, `2L` the
  second, etc. `NULL` merges all tables. **Default:** `NULL`

## Value

A `data.frame` with one row per body row and one column per cell. All
values are character strings. Column names are `V1`, `V2`, ... unless
`include_header = TRUE`.

## Details

Header rows are detected automatically via `\trhdr` (explicit; Word/SAS)
or via cell vertical-alignment hints (`\clvertalb` / `\clvertalt`) used
by [{r2rtf}](https://merck.github.io/r2rtf/).

When `include_header = TRUE` and no header rows are auto-detected, the
first body row is promoted to column names and removed from the data
(analogous to `read.csv(header = TRUE)`). Footnote and source rows are
excluded by their cell count (they span the full table width as a single
cell).

When an RTF file contains multiple logically distinct tables (separated
by paragraph text), use `table_index` to select a specific one. The
default (`NULL`) merges all tables into a single data frame, which is
the correct behaviour for paginated single-table outputs.

## Examples

``` r
path <- system.file("extdata", "example.rtf", package = "otter")
rtf_to_df(path)
#>      V1 V2
#> 1 Alice 10
#> 2   Bob 20
rtf_to_df(path, include_header = TRUE)
#>    Name Value
#> 1 Alice    10
#> 2   Bob    20

multi_path <- system.file("extdata", "multi_table_example.rtf", package = "otter")
rtf_to_df(multi_path, include_header = TRUE, table_index = 2L)
#>   C D
#> 1 3 4
```
