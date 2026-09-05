#!/usr/bin/env Rscript
# Performance validation for the "millions of cells" requirement.
#
# What this measures (server-side, reproducibly, no browser needed):
#   1. That the *interactive* path (family detection, palette generation,
#      re-coloring) only ever touches per-cluster summaries, so its cost is
#      driven by the number of CLUSTERS, not the number of CELLS -- flat
#      across 100k -> 5M cells.
#   2. That downsample_stratified() caps the data actually sent to the
#      browser for live dragging/recoloring at a constant size regardless of
#      total cell count (this is what keeps plotly's WebGL scattergl trace
#      responsive -- see inst/benchmark/README.md for the argument and its
#      limits).
#   3. The one-time cost of a genuine "full render" (every cell, for a final
#      export) as a function of size, and that it stays in the
#      seconds-not-minutes range using base-R rasterized plotting.
#
# Usage: Rscript inst/benchmark/benchmark.R [max_size]
# (defaults to running the full 100k..5M sweep; pass e.g. 1e6 to cap it for
# a quicker local run)

suppressMessages({
  devtools_available <- requireNamespace("devtools", quietly = TRUE)
})
if (devtools_available && file.exists("DESCRIPTION")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(palettome)
}
source(file.path("inst", "benchmark", "generate_synthetic.R"))

args <- commandArgs(trailingOnly = TRUE)
max_size <- if (length(args) >= 1) as.numeric(args[1]) else 5e6
sizes <- c(1e5, 5e5, 1e6, 2e6, 5e6)
sizes <- sizes[sizes <= max_size]

timeit <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  proc.time()[["elapsed"]] - t0
}

results <- list()
for (n in sizes) {
  n <- as.integer(n)
  cat(sprintf("\n=== n_cells = %s ===\n", format(n, big.mark = ",")))

  t_gen <- timeit(df <- generate_synthetic_dataset(n, n_families = 8, clusters_per_family = 4, seed = 1))
  cat(sprintf("  generate synthetic data:      %6.2fs\n", t_gen))

  t_adapt <- timeit(pd <- as_cluster_data(df, coord_cols = c("UMAP_1", "UMAP_2"), cluster_col = "cluster"))
  cat(sprintf("  as_cluster_data():             %6.2fs\n", t_adapt))

  t_centroid <- timeit(cent <- compute_centroids(pd))
  cat(sprintf("  compute_centroids() [%2d clusters]: %6.2fs\n", nrow(cent), t_centroid))

  t_detect <- timeit(fam <- detect_families(pd, k = 8))
  cat(sprintf("  detect_families() [hclust]:    %6.2fs\n", t_detect))

  t_harm <- timeit(sess_h <- generate_palette(fam$assignment, mode = "harmonious", seed = 1))
  t_contr <- timeit(sess_c <- generate_palette(fam$assignment, mode = "contrast", seed = 1))
  cat(sprintf("  generate_palette(harmonious):   %6.3fs\n", t_harm))
  cat(sprintf("  generate_palette(contrast):     %6.3fs\n", t_contr))

  t_down <- timeit(pd_small <- downsample_stratified(pd, max_n = 50000))
  cat(sprintf("  downsample_stratified(->50k):   %6.2fs  (%s -> %s rows)\n",
    t_down, format(nrow(pd$cells), big.mark = ","), format(nrow(pd_small$cells), big.mark = ",")
  ))

  full_mb <- as.numeric(object.size(pd$cells)) / 1e6
  small_mb <- as.numeric(object.size(pd_small$cells)) / 1e6
  cat(sprintf("  in-memory size: full=%.1fMB  preview=%.1fMB\n", full_mb, small_mb))

  # "Full render" cost: every cell, rasterized via base graphics (pch='.'),
  # the recommended approach for a final export at this scale (see README).
  t_render <- timeit({
    grDevices::png(tempfile(fileext = ".png"), width = 1600, height = 1200, res = 150)
    color_map <- stats::setNames(sess_h$clusters$color, sess_h$clusters$cluster)
    graphics::plot(
      pd$cells[[pd$coord_cols[1]]], pd$cells[[pd$coord_cols[2]]],
      col = color_map[pd$cells$cluster], pch = ".",
      xlab = "", ylab = "", main = ""
    )
    grDevices::dev.off()
  })
  cat(sprintf("  full-data static PNG render:    %6.2fs\n", t_render))

  results[[length(results) + 1]] <- data.frame(
    n_cells = n, n_clusters = nrow(cent),
    t_generate_data = t_gen, t_as_cluster_data = t_adapt,
    t_compute_centroids = t_centroid, t_detect_families = t_detect,
    t_generate_palette_harmonious = t_harm, t_generate_palette_contrast = t_contr,
    t_downsample = t_down, t_full_render_png = t_render,
    full_cells_mb = full_mb, preview_cells_mb = small_mb
  )
  rm(df, pd, pd_small, cent, fam, sess_h, sess_c)
  gc(FALSE)
}

results <- do.call(rbind, results)
out_csv <- file.path("inst", "benchmark", "results.csv")
utils::write.csv(results, out_csv, row.names = FALSE)
cat("\nWrote", out_csv, "\n")
print(results, row.names = FALSE)
