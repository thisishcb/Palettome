# Small synthetic dataset shared across tests: 12 clusters arranged into 4
# spatial groups of 3, so hierarchical clustering has an unambiguous answer.
make_test_pdata <- function(seed = 1) {
  set.seed(seed)
  n_clusters <- 12
  df <- do.call(rbind, lapply(seq_len(n_clusters), function(i) {
    group <- (i - 1) %/% 3
    cx <- (group %% 2) * 20
    cy <- (group %/% 2) * 20
    data.frame(
      UMAP_1 = stats::rnorm(30, cx, 0.5),
      UMAP_2 = stats::rnorm(30, cy, 0.5),
      cluster = paste0("c", i)
    )
  }))
  as_cluster_data(df, coord_cols = c("UMAP_1", "UMAP_2"), cluster_col = "cluster")
}

make_test_assignment <- function(pdata) {
  detect_families(pdata, k = 4)$assignment
}
