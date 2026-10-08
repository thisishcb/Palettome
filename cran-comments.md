## Resubmission

This is a resubmission. In this version I have:

* Put package and software names in single quotes in the Description
  ('Seurat', 'shiny').
* Added references for the methods used to the Description, in the form
  authors (year) <doi:...>.
* Changed the License field to 'Apache License (== 2.0)' and removed the
  LICENSE file from the package. (The review mentioned 'GPL-3 + file LICENSE';
  the package is licensed under Apache 2.0, which is now declared without
  the file.)
* Added executable examples to the Rd files of all exported functions.
  Examples that write files write only to tempdir() and remove them
  afterwards, and the example that launches the 'shiny' app is wrapped in
  if (interactive()).
* Made functions that take a `seed` argument restore the user's random
  number state (.Random.seed) on exit, so they no longer change the global
  RNG state.
* Added the maintainer's ORCID iD.

## Test environments

* local macOS 26.6.2 (aarch64), R 4.4.2
* win-builder, R-devel <!-- TODO: fill in after devtools::check_win_devel() -->

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
