# Tokenize raw RTF text into a flat character vector

Splits an RTF string into control words, control symbols, group braces,
and plain-text runs. Newlines are not tokenized (they are insignificant
whitespace in RTF between tokens).

## Usage

``` r
rtf_tokenize(rtf_text)
```

## Arguments

- rtf_text:

  A single character string of raw RTF content.

## Value

A character vector; each element is one RTF token.
