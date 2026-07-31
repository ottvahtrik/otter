
<!-- README.md is generated from README.Rmd. Please edit that file -->

# otter

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/otter)](https://CRAN.R-project.org/package=otter)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/gh/ottvahtrik/otter/graph/badge.svg)](https://app.codecov.io/gh/ottvahtrik/otter)
<!-- badges: end -->

otter is a small collection of functions that are useful for me, Ott,
and hopefully for others too. Sharing is caring!

Currently otter provides `rtf_to_df()`, which parses RTF files –
including those produced by SAS and the
[{r2rtf}](https://merck.github.io/r2rtf/) package – and extracts
embedded tables into a data frame. It automatically detects header rows,
handles multi-page and multi-table RTF output, and supports non-UTF-8
encodings.

## Installation

You can install the development version of otter from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ottvahtrik/otter")
```

## Example

`rtf_to_df()` reads an RTF table and returns its contents as a data
frame:

``` r
library(otter)

path <- system.file("extdata", "example.rtf", package = "otter")
rtf_to_df(path, include_header = TRUE)
#>    Name Value
#> 1 Alice    10
#> 2   Bob    20
```

If the RTF file contains multiple distinct tables, use `table_index` to
select one of them:

``` r
multi_path <- system.file("extdata", "multi_table_example.rtf", package = "otter")
rtf_to_df(multi_path, include_header = TRUE, table_index = 2L)
#>   C D
#> 1 3 4
```
