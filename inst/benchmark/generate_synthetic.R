# Purely synthetic benchmark dataset generator (no real expression matrix /
# no Splatter): Gaussian blobs per cluster, clusters arranged into families
# so a UMAP-like layout with real hierarchical structure results, at
# whatever total cell count is requested (validated up to 5 million).
#
# Not sourced by the palettome package itself -- this is a standalone
# benchmark script (see inst/benchmark/README.md).

generate_synthetic_dataset <- function(n_cells, n_families = 8, clusters_per_family = 4,
                                        seed = 1) {
  set.seed(seed)
  n_clusters <- n_families * clusters_per_family
  cluster_ids <- paste0("c", seq_len(n_clusters))
  family_of_cluster <- rep(paste0("F", seq_len(n_families)), each = clusters_per_family)

  # Family centers spread widely on a circle; cluster centers jitter around
  # their family's center, well inside the family's "territory" -- this is
  # what gives detect_families() an unambiguous hierarchy to recover.
  fam_angle <- seq(0, 2 * pi, length.out = n_families + 1)[seq_len(n_families)]
  fam_x <- 40 * cos(fam_angle)
  fam_y <- 40 * sin(fam_angle)

  clust_angle <- stats::runif(n_clusters, 0, 2 * pi)
  clust_r <- stats::runif(n_clusters, 3, 8)
  clust_x <- fam_x[match(family_of_cluster, paste0("F", seq_len(n_families)))] + clust_r * cos(clust_angle)
  clust_y <- fam_y[match(family_of_cluster, paste0("F", seq_len(n_families)))] + clust_r * sin(clust_angle)

  # Unequal cluster sizes (log-normal weights), like real cell-type
  # abundances, summing to n_cells.
  weights <- stats::rlnorm(n_clusters)
  sizes <- pmax(1L, round(n_cells * weights / sum(weights)))
  # Fix rounding drift against the exact requested total.
  sizes[n_clusters] <- sizes[n_clusters] + (n_cells - sum(sizes))

  parts <- vector("list", n_clusters)
  for (i in seq_len(n_clusters)) {
    ni <- sizes[i]
    parts[[i]] <- data.frame(
      UMAP_1 = stats::rnorm(ni, clust_x[i], 1.2),
      UMAP_2 = stats::rnorm(ni, clust_y[i], 1.2),
      cluster = cluster_ids[i],
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, parts)
  attr(df, "family_of_cluster") <- stats::setNames(family_of_cluster, cluster_ids)
  df
}
