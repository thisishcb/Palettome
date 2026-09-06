# dev/

Developer-only scripts, excluded from the built package (see
`.Rbuildignore`). Not part of the palettome API.

## Manually checking the package as you edit it

Keep one R session open (a terminal `R`/`radian`, or the VS Code R
extension's R terminal) rather than spawning a new `Rscript` process per
check -- `devtools::load_all()` only re-sources changed files and is near-
instant for this pure-R package, so there's no need to restart R.

```r
devtools::load_all()      # once
# ... edit R/*.R, save ...
devtools::load_all()      # re-run after each edit; fast, incremental
```

- **Automated tests**: `devtools::test()` (full suite), or
  `testthat::test_file("tests/testthat/test-color_generation.R")` for one
  file, or `testthat::auto_test_package()` to have it watch `R/` and
  `tests/testthat/` and rerun automatically on save.
- **Manual/exploratory check** (this isn't a substitute for tests -- it's
  for eyeballing behavior, plots, and the UI after a change):
  run `dev/manual_check.R` a section at a time in your live session
  (`devtools::load_all()` first, then paste/step through it), or all at
  once with `Rscript dev/manual_check.R`. It builds a small synthetic
  dataset and walks through family detection, both color modes, manual
  overrides surviving regeneration, the static plot/swatch figures, and a
  JSON export round-trip -- printing/plotting results as it goes so you can
  see them, not just pass/fail.
- **Manually checking the Shiny UI**: uncomment the last line of
  `dev/manual_check.R` (or run directly):
  ```r
  devtools::load_all()
  pdata <- as_cluster_data(your_df, coord_cols = c("UMAP_1","UMAP_2"), cluster_col = "cluster")
  launch_palettome_ui(pdata)
  ```
  then click through it by hand: drag a cluster chip into a different
  family bin, click a chip/family header to recolor it, toggle
  mode/seed/lightness/chroma/colorblind preview, and try each export
  button.
