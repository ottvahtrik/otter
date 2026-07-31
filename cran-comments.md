## Submission

This is a new release.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release, so the "New submission" NOTE is expected.

## Test environments

* local macOS install, R 4.5.2 (`rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))`);
  `--no-manual` used locally because this machine has no LaTeX installation
  to build the PDF reference manual -- not expected to be an issue on
  CRAN's own check machines, which have LaTeX available.
* win-builder (R-devel, `devtools::check_win_devel()`): 0 errors | 0 warnings | 1 note.
  The note flags "New submission" and the possibly misspelled word "Ott's"
  in the Title, which is the maintainer's name and expected to be a false
  positive.
* GitHub Actions (via `usethis::use_github_action("test-coverage")`)
