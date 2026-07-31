# Internal character decoding helpers for RTF control words and escapes.

cw_param <- function(tok) {
  m <- regmatches(tok, regexpr("-?[0-9]+", tok))
  if (length(m) == 0L) {
    NA_integer_
  } else {
    as.integer(m)
  }
}

decode_unicode <- function(n) {
  if (is.na(n)) {
    return("")
  }
  if (n < 0L) {
    n <- n + 65536L # RTF uses signed 16-bit; wrap negatives
  }
  intToUtf8(n)
}

decode_hex <- function(hex2, encoding = "CP1252") {
  raw_byte <- as.raw(strtoi(hex2, base = 16L))
  iconv(rawToChar(raw_byte), from = encoding, to = "UTF-8", sub = "")
}

# Detect the Windows code page declared by \ansicpgN and return an iconv-
# compatible encoding string. Defaults to CP1252 (RTF spec default for \ansi).
detect_ansicpg <- function(rtf_text) {
  m <- regmatches(rtf_text, regexpr("\\\\ansicpg([0-9]+)", rtf_text, perl = TRUE))
  if (length(m) == 0L || identical(m, character(0L))) {
    return("CP1252")
  }
  paste0("CP", sub("\\\\ansicpg", "", m, fixed = FALSE))
}
