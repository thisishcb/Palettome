#' Get cluster colors as a named vector or a plain data frame
#'
#' The most common thing to actually do with a `palettome_session` is hand
#' its colors to something else's plotting call (e.g. `Seurat::DimPlot(cols
#' = cluster_colors(session))`, `ggplot2::scale_color_manual(values =
#' cluster_colors(session))`). This pulls just the `cluster -> color`
#' mapping out of the fuller session object in whichever shape is
#' convenient, without needing to know the session's internal structure.
#'
#' @param session A `palettome_session` (from [generate_palette()]).
#' @param format `"vector"` (default) for a named character vector keyed by
#'   cluster id, or `"data.frame"` for a `cluster`/`color`/`family_id`/
#'   `family_color` data frame (the family's own representative color
#'   merged in alongside its id, so grouping/faceting/legend code doesn't
#'   need a separate join against [family_colors()]).
#' @return A named character vector, or a data frame, depending on `format`.
#' @export
#' @examples
#' assignment <- data.frame(cluster = paste0("c", 1:4), family_id = rep(c("F1", "F2"), each = 2))
#' session <- generate_palette(assignment, mode = "harmonious", seed = 1)
#' cluster_colors(session)
#' cluster_colors(session, format = "data.frame")
cluster_colors <- function(session, format = c("vector", "data.frame")) {
  format <- match.arg(format)
  stopifnot(inherits(session, "palettome_session"))
  if (format == "vector") {
    stats::setNames(session$clusters$color, session$clusters$cluster)
  } else {
    df <- session$clusters[, c("cluster", "color", "family_id")]
    df$family_color <- family_colors(session)[df$family_id]
    df
  }
}

#' Get family colors as a named vector or a plain data frame
#'
#' Same as [cluster_colors()], but for each family's representative/accent
#' color (the harmonious-mode base hue, or the contrast-mode secondary
#' channel) rather than per-cluster fill colors.
#'
#' @inheritParams cluster_colors
#' @return A named character vector, or a data frame, depending on `format`.
#' @export
family_colors <- function(session, format = c("vector", "data.frame")) {
  format <- match.arg(format)
  stopifnot(inherits(session, "palettome_session"))
  if (format == "vector") {
    stats::setNames(session$families$color, session$families$family_id)
  } else {
    session$families[, c("family_id", "color")]
  }
}
