aj <- function(name) get(name, envir = asNamespace("palettome"))

test_that(".cluster_distance builds a distance matrix from centroid columns", {
  cd <- aj(".cluster_distance")
  df <- data.frame(
    cluster = c("a", "b", "c"),
    x_centroid = c(0, 0, 10), y_centroid = c(0, 1, 10)
  )
  d <- cd(df, c("a", "b", "c"))
  expect_equal(dim(d), c(3, 3))
  expect_equal(d["a", "b"], 1, tolerance = 1e-6)
  expect_gt(d["a", "c"], d["a", "b"])
})

test_that(".cluster_distance recognises a similarity matrix and inverts it", {
  cd <- aj(".cluster_distance")
  sim <- matrix(c(1, 0.9, 0.1, 0.9, 1, 0.2, 0.1, 0.2, 1), 3,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  d <- cd(sim, c("a", "b", "c"))
  expect_lt(d["a", "b"], d["a", "c"]) # high similarity -> small distance
  expect_equal(diag(d), c(a = 0, b = 0, c = 0))
})

test_that(".cluster_distance returns NULL for NULL input", {
  expect_null(aj(".cluster_distance")(NULL, c("a", "b")))
})

test_that(".family_distance is single-linkage over cluster distances", {
  fd <- aj(".family_distance")
  cdist <- matrix(0, 4, 4, dimnames = list(paste0("c", 1:4), paste0("c", 1:4)))
  cdist["c1", "c3"] <- cdist["c3", "c1"] <- 5
  cdist["c1", "c4"] <- cdist["c4", "c1"] <- 8
  cdist["c2", "c3"] <- cdist["c3", "c2"] <- 3
  cdist["c2", "c4"] <- cdist["c4", "c2"] <- 9
  assignment <- data.frame(
    cluster = paste0("c", 1:4), family_id = c("F1", "F1", "F2", "F2")
  )
  d <- fd(cdist, assignment)
  expect_equal(d["F1", "F2"], 3) # min over the cross pairs
})

test_that(".seriate_order returns a permutation ordering points along a line", {
  so <- aj(".seriate_order")
  pts <- c(0, 5, 1, 4, 2, 3)
  d <- as.matrix(dist(pts))
  o <- so(d)
  expect_setequal(o, seq_along(pts))
  # points should end up sorted (or exactly reversed) along the line
  expect_true(!is.unsorted(pts[o]) || !is.unsorted(rev(pts[o])))
})

test_that(".knn_adjacency is symmetric with a zero diagonal", {
  ka <- aj(".knn_adjacency")
  d <- as.matrix(dist(c(0, 1, 2, 10, 11)))
  adj <- ka(d, k = 1)
  expect_true(isSymmetric(unname(adj)))
  expect_false(any(diag(adj)))
  expect_true(adj[4, 5]) # the far pair are each other's nearest
})
