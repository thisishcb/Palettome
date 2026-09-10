#' Static plot of an embedding colored by the current palette
#'
#' Works with zero UI packages installed: uses ggplot2 for a nicer plot if
#' it happens to be available (it is Suggests-only, never required), and
#' otherwise falls back to base [graphics::plot()]. Either way, no Shiny/UI
#' dependency is ever touched.
#'
#' For large datasets, consider subsampling `pdata$cells` (e.g. stratified
#' per cluster) before calling this for interactive-style previews, and only
#' plotting the full data for a final export; see `inst/benchmark/` for the
#' rendering-strategy discussion behind this recommendation.
#'
#' @param pdata A `palettome_data` object from [as_cluster_data()].
#' @param session A `palettome_session` (from [generate_palette()]) supplying
#'   the cluster -> color mapping.
#' @param cvd `"none"` or a CVD type; when set, points are drawn with
#'   [simulate_cvd()]-adjusted colors as a colorblind-safe preview.
#' @param point_size,alpha Passed through to the plotting backend.
#' @return A `ggplot` object if ggplot2 is installed, otherwise `NULL`
#'   (invisibly) after drawing directly to the current base graphics device.
#' @export
plot_palette_static <- function(pdata, session, cvd = "none",
                                 point_size = 0.6, alpha = 0.7) {
  stopifnot(inherits(pdata, "palettome_data"), inherits(session, "palettome_session"))
  cells <- pdata$cells
  color_map <- stats::setNames(session$clusters$color, session$clusters$cluster)
  if (!identical(cvd, "none")) color_map <- simulate_cvd(color_map, type = cvd)
  cols <- color_map[cells$cluster]
  x <- cells[[pdata$coord_cols[1]]]
  y <- cells[[pdata$coord_cols[2]]]

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    df <- data.frame(x = x, y = y, cluster = cells$cluster, stringsAsFactors = FALSE)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, color = .data$cluster)) +
      ggplot2::geom_point(size = point_size, alpha = alpha) +
      ggplot2::scale_color_manual(values = color_map, name = "cluster") +
      ggplot2::labs(x = pdata$coord_cols[1], y = pdata$coord_cols[2]) +
      ggplot2::theme_minimal()
    return(p)
  }

  graphics::plot(
    x, y, col = cols, pch = 16, cex = point_size,
    xlab = pdata$coord_cols[1], ylab = pdata$coord_cols[2],
    main = "Palettome embedding"
  )
  invisible(NULL)
}

#' Standalone swatch/legend figure, grouped by family
#'
#' A base-graphics-only (no ggplot2/Shiny needed) legend figure: one row per
#' cluster, colored swatch + label, visually grouped and separated by
#' family, with the family's own color shown as a header bar. Suitable for
#' exporting as PNG/SVG alongside the main plot.
#'
#' @param session A `palettome_session`.
#' @param cvd `"none"` or a CVD type to preview the swatches under.
#' @param order_by `"lightness"` (default) sorts families, and clusters
#'   within each family, by lightness so a harmonious sweep reads as a
#'   continuous ramp; `"id"` keeps `family_id` / `cluster` order.
#' @return Invisibly, `NULL`; draws to the current graphics device.
#' @export
plot_swatches <- function(session, cvd = "none", order_by = c("lightness", "id")) {
  stopifnot(inherits(session, "palettome_session"))
  order_by <- match.arg(order_by)
  clusters <- session$clusters
  if (order_by == "lightness") {
    cl_l <- farver::decode_colour(clusters$color, to = "lab")[, "l"]
    fam_l <- tapply(cl_l, clusters$family_id, mean)
    clusters <- clusters[order(fam_l[clusters$family_id], cl_l), ]
  } else {
    clusters <- clusters[order(clusters$family_id, clusters$cluster), ]
  }
  families <- session$families[match(unique(clusters$family_id), session$families$family_id), ]

  cluster_col <- clusters$color
  family_col <- families$color
  if (!identical(cvd, "none")) {
    cluster_col <- simulate_cvd(cluster_col, type = cvd)
    family_col <- simulate_cvd(family_col, type = cvd)
  }

  n_rows <- nrow(clusters) + nrow(families)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, n_rows))
  y <- n_rows
  for (i in seq_len(nrow(families))) {
    fid <- families$family_id[i]
    graphics::rect(0, y - 1, 0.05, y, col = family_col[i], border = NA)
    graphics::text(0.07, y - 0.5, labels = fid, adj = 0, font = 2)
    y <- y - 1
    members <- clusters[clusters$family_id == fid, ]
    for (j in seq_len(nrow(members))) {
      idx <- which(clusters$cluster == members$cluster[j])
      graphics::rect(0.02, y - 1, 0.07, y, col = cluster_col[idx], border = NA)
      lab <- members$cluster[j]
      if (!is.na(members$n_cells[j])) lab <- paste0(lab, " (n=", members$n_cells[j], ")")
      graphics::text(0.09, y - 0.5, labels = lab, adj = 0)
      y <- y - 1
    }
  }
  invisible(NULL)
}
