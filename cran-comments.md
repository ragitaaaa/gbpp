## Test environments
* local Windows 11 x64, R 4.5.1
* win-builder (devel and release)
* GitHub Actions / R-hub (Ubuntu Linux, macOS, Windows)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes regarding optional dependencies
* The package interfaces with gradient boosting engines ('xgboost', 'lightgbm', 'catboost') via 'reticulate'. All tests and examples using Python engines are guarded with `testthat::skip_on_cran()` and `reticulate::py_available()`, ensuring that CRAN check machines without Python configured execute and pass cleanly.
