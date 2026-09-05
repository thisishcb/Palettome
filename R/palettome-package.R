#' palettome: Hierarchy-Aware, Interactively Editable Color Palettes
#'
#' Core functions (`as_cluster_data()`, `detect_families()`,
#' `generate_palette()`, `plot_palette_static()`, `export_palette_json()`,
#' ...) depend only on colorspace/farver/stats/jsonlite and run with zero UI
#' packages installed. The optional interactive UI (`launch_palettome_ui()`)
#' lives entirely behind Suggests-only packages (shiny, plotly, colourpicker,
#' shinyjs) and degrades to a friendly error if they are missing.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# plot_palette_static() uses the ggplot2 .data pronoun (only when ggplot2 is
# installed) to reference columns of a locally-built data frame; declaring
# it here silences the R CMD check "no visible binding for global variable"
# NOTE without adding a hard dependency on ggplot2 or rlang.
utils::globalVariables(".data")
