test_that("as_cluster_data.data.frame validates inputs", {
  df <- data.frame(x = 1:4, y = 1:4, cl = c("a", "a", "b", "b"))
  expect_error(as_cluster_data(df, coord_cols = "x", cluster_col = "cl"), "length 2 or 3")
  expect_error(as_cluster_data(df, coord_cols = c("x", "z"), cluster_col = "cl"), "columns of")
  expect_error(as_cluster_data(df, coord_cols = c("x", "y"), cluster_col = "nope"), "column of")

  pd <- as_cluster_data(df, coord_cols = c("x", "y"), cluster_col = "cl")
  expect_s3_class(pd, "palettome_data")
  expect_false(pd$has_family)
  expect_equal(pd$coord_cols, c("x", "y"))
  expect_equal(nrow(pd$cells), 4)
})

test_that("as_cluster_data.data.frame carries an explicit family column", {
  df <- data.frame(
    x = 1:4, y = 1:4,
    cl = c("a", "b", "c", "d"),
    fam = c("F1", "F1", "F2", "F2")
  )
  pd <- as_cluster_data(df, coord_cols = c("x", "y"), cluster_col = "cl", family_col = "fam")
  expect_true(pd$has_family)
  expect_equal(pd$cells$family, df$fam)
})

test_that("compute_centroids averages coordinates per cluster and counts cells", {
  df <- data.frame(x = c(0, 2, 10, 12), y = c(0, 2, 10, 12), cl = c("a", "a", "b", "b"))
  pd <- as_cluster_data(df, coord_cols = c("x", "y"), cluster_col = "cl")
  cent <- compute_centroids(pd)
  expect_equal(sort(cent$cluster), c("a", "b"))
  expect_equal(cent$n_cells, c(2, 2))
  expect_equal(cent$x_centroid[cent$cluster == "a"], 1)
  expect_equal(cent$y_centroid[cent$cluster == "b"], 11)
})
