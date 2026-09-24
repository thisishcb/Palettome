test_that("detect_families uses an explicit family column directly, without clustering", {
  df <- data.frame(
    x = c(0, 1, 10, 11), y = c(0, 1, 10, 11),
    cl = c("a", "b", "c", "d"), fam = c("F1", "F1", "F2", "F2")
  )
  pd <- as_cluster_data(df, coord_cols = c("x", "y"), cluster_col = "cl", family_col = "fam")
  res <- detect_families(pd)
  expect_s3_class(res, "palettome_families")
  expect_equal(res$method, "explicit")
  expect_null(res$dendro)
  assignment <- res$assignment[order(res$assignment$cluster), ]
  expect_equal(assignment$family_id, c("F1", "F1", "F2", "F2"))
})

test_that("hclust-based detect_families recovers well-separated spatial groups", {
  pd <- make_test_pdata()
  res <- detect_families(pd, k = 4)
  expect_equal(res$method, "hclust")
  expect_false(is.null(res$dendro))
  expect_equal(length(unique(res$assignment$family_id)), 4)
  expect_equal(sort(res$assignment$cluster), sort(unique(pd$cells$cluster)))

  # clusters 1-3 were generated as one spatial group; they should all land
  # in the same family
  fam_of <- stats::setNames(res$assignment$family_id, res$assignment$cluster)
  expect_equal(fam_of[["c1"]], fam_of[["c2"]])
  expect_equal(fam_of[["c2"]], fam_of[["c3"]])
})

test_that("cut_families re-cuts a precomputed dendrogram without recomputing distances", {
  pd <- make_test_pdata()
  dendro <- compute_family_dendrogram(pd)
  a2 <- cut_families(dendro, k = 2)
  a4 <- cut_families(dendro, k = 4)
  expect_equal(length(unique(a2$family_id)), 2)
  expect_equal(length(unique(a4$family_id)), 4)
})

test_that("detect_families accepts a connectivity matrix in place of centroid distance", {
  pd <- make_test_pdata()
  clusters <- sort(unique(pd$cells$cluster))
  n <- length(clusters)
  # Fake connectivity matching make_test_pdata()'s spatial groups:
  # {c1,c2,c3}, {c4,c5,c6}, {c7,c8,c9}, {c10,c11,c12} are each ~0 apart,
  # ~1 apart across groups.
  conn <- matrix(0.01, n, n, dimnames = list(clusters, clusters))
  cluster_num <- as.integer(sub("^c", "", clusters))
  groups <- split(clusters, (cluster_num - 1) %/% 3)
  for (g in groups) conn[g, g] <- 1
  diag(conn) <- 1

  res <- detect_families(pd, k = 4, connectivity = conn)
  expect_equal(res$method, "graph")
  fam_of <- stats::setNames(res$assignment$family_id, res$assignment$cluster)
  expect_equal(fam_of[["c1"]], fam_of[["c2"]])
})
