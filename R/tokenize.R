#' Tokenize raw RTF text into a flat character vector
#'
#' Splits an RTF string into control words, control symbols, group braces, and
#' plain-text runs. Newlines are not tokenized (they are insignificant whitespace
#' in RTF between tokens).
#'
#' @param rtf_text A single character string of raw RTF content.
#' @return A character vector; each element is one RTF token.
#' @keywords internal
rtf_tokenize <- function(rtf_text) {
  stopifnot(is.character(rtf_text), length(rtf_text) == 1L)
  # 4 backslashes in R string → [\\] in regex → matches 1 literal backslash
  pat <- paste(
    "[\\\\][a-zA-Z]+-?[0-9]*[ \t]?",  # control word: \word, \word123, \word123<sp>
    "[\\\\][^a-zA-Z\r\n]",             # control symbol: \' \\ \{ \} \~ \- \_ \* etc.
    "[{}]",                             # group open / close
    "[^\\\\{}\r\n]+",                  # plain text: any run without \ { } newlines
    sep = "|"
  )
  regmatches(rtf_text, gregexpr(pat, rtf_text, perl = TRUE))[[1L]]
}
