# Extract table rows from a tokenized RTF stream

State machine over the token vector produced by
[`rtf_tokenize()`](https://ottvahtrik.github.io/otter/reference/rtf_tokenize.md).

## Usage

``` r
rtf_extract_tables(tokens, hex_encoding = "CP1252")
```

## Arguments

- tokens:

  Character vector from
  [`rtf_tokenize()`](https://ottvahtrik.github.io/otter/reference/rtf_tokenize.md).

- hex_encoding:

  iconv-compatible encoding used to decode `\'XX` hex escapes. Detected
  from `\ansicpgN` by `detect_ansicpg()`. **Default:** `"CP1252"`.

## Value

A list with three elements:

- `header_rows` / `body_rows`: lists of character vectors (one per row;
  one element per cell) with all tables merged – the existing
  single-table interface.

- `all_tables`: list of per-table `list(header_rows, body_rows)` for
  files that contain multiple logically distinct tables.
