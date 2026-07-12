## Test environments

* local Windows 11 Pro, R 4.5.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes

This is a patch release that fixes `col_show` not hiding columns when
printing a sticky tibble (a display-only bug; no exported API changes).

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across
CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages
