test_that("downsample_stratified caps total points and keeps every cluster represented", {
  set.seed(1)
  n_clusters <- 20
  sizes <- sample(50:5000, n_clusters, replace = TRUE)
  df <- do.call(rbind, lapply(seq_len(n_clusters), function(i) {
    data.frame(x = stats::rnorm(sizes[i]), y = stats::rnorm(sizes[i]), cl = paste0("c", i))
  }))
  pd <- as_cluster_data(df, coord_cols = c("x", "y"), cluster_col = "cl")

  small <- downsample_stratified(pd, max_n = 2000, min_per_cluster = 10)
  expect_lte(nrow(small$cells), 2000 + n_clusters) # rounding slack
  expect_setequal(unique(small$cells$cluster), unique(pd$cells$cluster))
  expect_true(all(table(small$cells$cluster) >= 10))
})

test_that("downsample_stratified is a no-op below the cap", {
  df <- data.frame(x = 1:10, y = 1:10, cl = rep(c("a", "b"), 5))
  pd <- as_cluster_data(df, coord_cols = c("x", "y"), cluster_col = "cl")
  same <- downsample_stratified(pd, max_n = 1000)
  expect_identical(same, pd)
})
