## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

* "New submission".

* The suggested package 'vdemdata' is not on CRAN. It is only used, behind
  a `requireNamespace()` guard, to fetch V-Dem democracy data in
  `get_country_indicators()`; the bundled `indicators` dataset already
  contains that variable, so the package is fully functional without it.
  It is available from the maintainers' r-universe, declared in
  `Additional_repositories`.

* Some example, test, and vignette code paths use network access (World
  Bank and V-Dem APIs) or optional heavy packages. These are wrapped in
  `\dontrun{}` / `skip_if_offline()` / `skip_on_cran()` /
  `requireNamespace()` guards and are not run during a standard check.

## Test environments

* local: Ubuntu 24.04, R 4.6.1
* R-hub and win-builder: pending
