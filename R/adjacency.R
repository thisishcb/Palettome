# Adjacency derivation for neighbor-aware coloring. `neighbors` reaches
# generate_palette() as either cluster centroids (a data frame) or a
# cluster-by-cluster connectivity/distance matrix, and both are reduced here
# to a cluster distance matrix, then to family adjacency + 1-D orderings.

#' Cluster-by-cluster distance matrix from a `neighbors` argument
#'
#' @param neighbors One of: `NULL`; a data frame with a `cluster` column
#'   plus either `*_centroid` columns or plain numeric coordinate columns
#'   (e.g. [compute_centroids()] output); or a square numeric matrix (a
#'   similarity/connectivity matrix or a distance matrix) with row/col names
#'   covering `clusters`.
#' @param clusters Character vector of cluster ids to restrict/order to.
#' @return A `clusters x clusters` numeric distance matrix (smaller = more
#'   adjacent), or `NULL` if `neighbors` is `NULL`.
#' @keywords internal
.cluster_distance <- function(neighbors, clusters) {
  if (is.null(neighbors)) return(NULL)

  if (is.matrix(neighbors) || inherits(neighbors, "dist")) {
    m <- as.matrix(neighbors)
    if (is.null(rownames(m)) || !all(clusters %in% rownames(m))) {
      stop("`neighbors` matrix must have row/col names covering every cluster", call. = FALSE)
    }
    m <- m[clusters, clusters, drop = FALSE]
    off <- m[upper.tri(m)]
    looks_like_similarity <- mean(diag(m)) >= stats::median(off) &&
      all(m >= 0) && stats::sd(diag(m)) < .Machine$double.eps^0.25
    if (looks_like_similarity) {
      maxv <- max(m, na.rm = TRUE)
      if (maxv <= 0) maxv <- 1
      d <- 1 - m / maxv
    } else {
      d <- m
    }
    d <- (d + t(d)) / 2
    diag(d) <- 0
    dimnames(d) <- list(clusters, clusters)
    return(d)
  }

  if (is.data.frame(neighbors)) {
    if (!"cluster" %in% names(neighbors)) {
      stop("`neighbors` data frame must have a `cluster` column", call. = FALSE)
    }
    rownames(neighbors) <- as.character(neighbors$cluster)
    if (!all(clusters %in% rownames(neighbors))) {
      stop("`neighbors` data frame is missing rows for some clusters", call. = FALSE)
    }
    coord_cols <- grep("_centroid$", names(neighbors), value = TRUE)
    if (!length(coord_cols)) {
      coord_cols <- setdiff(
        names(neighbors)[vapply(neighbors, is.numeric, logical(1))],
        c("n_cells")
      )
    }
    if (length(coord_cols) < 1) {
      stop("`neighbors` data frame has no usable coordinate columns", call. = FALSE)
    }
    coords <- as.matrix(neighbors[clusters, coord_cols, drop = FALSE])
    d <- as.matrix(stats::dist(coords))
    dimnames(d) <- list(clusters, clusters)
    return(d)
  }

  stop("`neighbors` must be NULL, a data frame, or a square matrix", call. = FALSE)
}

#' Family-by-family distance (single-linkage over cluster distances)
#' @keywords internal
.family_distance <- function(cluster_dist, assignment) {
  fam_ids <- unique(assignment$family_id)
  members <- split(assignment$cluster, assignment$family_id)[fam_ids]
  n <- length(fam_ids)
  fd <- matrix(0, n, n, dimnames = list(fam_ids, fam_ids))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j <= i) next
      block <- cluster_dist[members[[i]], members[[j]], drop = FALSE]
      fd[i, j] <- fd[j, i] <- min(block)
    }
  }
  fd
}

#' 1-D seriation order of items given a distance matrix
#'
#' Returns a permutation `o` such that `o[j]` is the item at position `j`
#' along a line that keeps near items near. Uses the first classical-MDS
#' coordinate, falling back to average-linkage dendrogram order.
#' @keywords internal
.seriate_order <- function(d) {
  d <- as.matrix(d)
  n <- nrow(d)
  if (n <= 2) return(seq_len(n))
  o <- tryCatch(order(stats::cmdscale(stats::as.dist(d), k = 1)[, 1]),
    error = function(e) NULL
  )
  if (is.null(o) || length(o) != n || anyNA(o)) {
    o <- tryCatch(stats::hclust(stats::as.dist(d), "average")$order,
      error = function(e) seq_len(n)
    )
  }
  o
}

#' Logical adjacency matrix: each item linked to its `k` nearest
#' @keywords internal
.knn_adjacency <- function(d, k = NULL) {
  d <- as.matrix(d)
  n <- nrow(d)
  if (is.null(k)) k <- min(3L, n - 1L)
  k <- max(1L, min(k, n - 1L))
  adj <- matrix(FALSE, n, n, dimnames = dimnames(d))
  for (i in seq_len(n)) {
    nn <- order(d[i, ])[-1][seq_len(k)]
    adj[i, nn] <- TRUE
  }
  adj | t(adj)
}
