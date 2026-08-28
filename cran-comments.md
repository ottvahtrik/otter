## Resubmission

This is a resubmission. In response to CRAN feedback on the initial
submission:

The Description field has been expanded into a full paragraph
explaining what the package does, why it is useful, and which tools
(SAS, 'r2rtf') commonly produce the RTF tables it parses.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release, so the "New submission" NOTE is expected.

## Test environments

* local macOS install, R 4.5.2 (`rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))`);
  `--no-manual` used locally because this machine has no LaTeX installation
  to build the PDF reference manual -- not expected to be an issue on
  CRAN's own check machines, which have LaTeX available.
* win-builder (R-devel, `devtools::check_win_devel()`, checked 2026-08-28):
  0 errors | 0 warnings | 1 note. The note flags "New submission" and the
  possibly misspelled words "Ott's" (maintainer's name) and "RTF" (the
  file format the package parses), both expected false positives.
* GitHub Actions (via `usethis::use_github_action("test-coverage")`)
