# Internal environment used to hand data from launch_palettome_ui() to the
# Shiny app in inst/app/ without making the core package depend on Shiny.
.pt_env <- new.env(parent = emptyenv())

#' Launch the interactive Palettome Shiny UI
#'
#' This is the *only* function in the package that requires UI packages
#' (shiny, plotly, colourpicker, shinyjs). All of them are Suggests-only, so
#' installing/loading the palettome package itself never requires them, and
#' every other function ([detect_families()], [generate_palette()],
#' [plot_palette_static()], [export_palette_json()], ...) works without
#' them. If any are missing, this fails with a friendly message rather than
#' an obscure error, and points out that the rest of the package still
#' works.
#'
#' @param pdata A `palettome_data` object from [as_cluster_data()].
#' @param families Optional: a `palettome_families` object (from
#'   [detect_families()]) or a plain `cluster`/`family_id` data frame to
#'   seed the initial grouping. If `NULL`, [detect_families()] is run with
#'   `k`/`h`.
#' @param session Optional: a `palettome_session` (e.g. loaded via
#'   [import_palette_json()]) to resume editing, preserving prior manual
#'   overrides. If `NULL`, a fresh harmonious palette is generated.
#' @param k,h Passed to [detect_families()] when `families` is `NULL`.
#' @param preview_max_n Passed to [downsample_stratified()] for the live
#'   scatter plot; the full dataset is only used for the final PNG export.
#' @param ... Passed on to `shiny::runApp()` (e.g. `launch.browser`, `port`).
#' @return Does not return; runs the Shiny app until interrupted.
#' @export
launch_palettome_ui <- function(pdata, families = NULL, session = NULL,
                                 k = NULL, h = NULL, preview_max_n = 50000, ...) {
  required <- c("shiny", "plotly", "colourpicker", "shinyjs")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "UI dependencies not installed: ", paste(missing, collapse = ", "), ".\n",
      "Core functions (generate_palette(), detect_families(), ",
      "plot_palette_static(), export_palette_json(), ...) remain fully ",
      "usable without them -- this only affects the interactive UI.\n",
      "Run install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "),
      ")) to enable it.",
      call. = FALSE
    )
  }
  stopifnot(inherits(pdata, "palettome_data"))

  fam_result <- if (!is.null(families)) {
    if (inherits(families, "palettome_families")) {
      families
    } else {
      structure(list(assignment = families, method = "explicit", dendro = NULL),
        class = "palettome_families"
      )
    }
  } else {
    detect_families(pdata, k = k, h = h)
  }

  centroids <- compute_centroids(pdata)
  init_session <- session %||% generate_palette(
    fam_result$assignment,
    mode = "harmonious", seed = 1,
    n_cells = centroids[, c("cluster", "n_cells")]
  )

  assign("pdata", pdata, envir = .pt_env)
  assign("centroids", centroids, envir = .pt_env)
  assign("assignment", fam_result$assignment, envir = .pt_env)
  assign("dendro", fam_result$dendro, envir = .pt_env)
  assign("session", init_session, envir = .pt_env)
  assign("preview_max_n", preview_max_n, envir = .pt_env)

  app_dir <- system.file("app", package = "palettome")
  if (!nzchar(app_dir)) {
    # Running from a source checkout (not yet installed as a package).
    app_dir <- file.path(getwd(), "inst", "app")
  }
  if (!file.exists(file.path(app_dir, "app.R"))) {
    stop("Could not locate the Shiny app directory (looked in: ", app_dir, ")", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
