#' Build a cluster-distance dendrogram for family detection
#'
#' Computes cluster centroids (or uses a supplied connectivity/distance
#' matrix) and runs hierarchical clustering on them. Kept separate from
#' [detect_families()] so a UI can render a dendrogram preview once and then
#' re-cut it at different `k`/`h` cheaply via [cut_families()], without
#' recomputing distances every time the user drags a cut-height slider.
#'
#' @param pdata A `palettome_data` object from [as_cluster_data()].
#' @param dist_method Distance metric for centroid-based clustering, passed
#'   to [stats::dist()]. Ignored if `connectivity` is supplied.
#' @param hclust_method Agglomeration method, passed to [stats::hclust()].
#' @param connectivity Optional square cluster-by-cluster connectivity or
#'   similarity matrix (e.g. from a shared-nearest-neighbor graph), with row
#'   and column names matching cluster ids. When supplied, it is converted to
#'   a distance matrix (`1 - normalized similarity`) and used instead of
#'   Euclidean centroid distance.
#' @return A list with `hclust` (an [stats::hclust] object), `centroids`
#'   (from [compute_centroids()]), and `clusters` (the cluster id order used).
#' @export
compute_family_dendrogram <- function(pdata, dist_method = "euclidean",
                                       hclust_method = "average",
                                       connectivity = NULL) {
  stopifnot(inherits(pdata, "palettome_data"))
  centroids <- compute_centroids(pdata)
  clusters <- centroids$cluster

  if (!is.null(connectivity)) {
    stopifnot(
      "`connectivity` must be a square matrix" =
        is.matrix(connectivity) && nrow(connectivity) == ncol(connectivity),
      "`connectivity` row/col names must cover all clusters" =
        all(clusters %in% rownames(connectivity)) &&
        all(clusters %in% colnames(connectivity))
    )
    conn <- connectivity[clusters, clusters, drop = FALSE]
    maxv <- max(conn, na.rm = TRUE)
    if (maxv <= 0) maxv <- 1
    d <- stats::as.dist(1 - conn / maxv)
  } else {
    coord_mat <- as.matrix(centroids[, paste0(pdata$coord_cols, "_centroid"), drop = FALSE])
    rownames(coord_mat) <- clusters
    d <- stats::dist(coord_mat, method = dist_method)
  }

  hc <- stats::hclust(d, method = hclust_method)
  hc$labels <- clusters
  list(
    hclust = hc, centroids = centroids, clusters = clusters,
    connectivity = connectivity
  )
}

#' Cut a precomputed dendrogram into families
#'
#' @param dendro A list as returned by [compute_family_dendrogram()].
#' @param k Desired number of families (mutually exclusive with `h`).
#' @param h Height at which to cut the dendrogram (mutually exclusive with
#'   `k`). If both are `NULL`, `k` defaults to the square root of the number
#'   of clusters, rounded, as a reasonable starting point.
#' @return A data frame with columns `cluster` and `family_id` (character,
#'   `"F1"`, `"F2"`, ...).
#' @export
cut_families <- function(dendro, k = NULL, h = NULL) {
  stopifnot(is.list(dendro), !is.null(dendro$hclust))
  n <- length(dendro$clusters)
  if (is.null(k) && is.null(h)) {
    k <- max(1L, round(sqrt(n)))
  }
  grp <- if (!is.null(h)) {
    stats::cutree(dendro$hclust, h = h)
  } else {
    stats::cutree(dendro$hclust, k = k)
  }
  data.frame(
    cluster = names(grp),
    family_id = paste0("F", as.integer(grp)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Detect (or pass through) cluster families
#'
#' If `pdata` carries an explicit family column (i.e. `pdata$has_family` is
#' `TRUE`), that mapping is used as-is and no clustering is performed.
#' Otherwise, families are auto-detected by hierarchical clustering of
#' cluster centroids (or a supplied connectivity graph). Either way, the
#' result is meant to be a *starting point*: it is a plain
#' cluster -> family_id data frame that a UI can freely rewrite by hand
#' (drag-to-regroup) afterward.
#'
#' @inheritParams compute_family_dendrogram
#' @param k,h See [cut_families()]. Ignored when families are explicit.
#' @return A `palettome_families` object: a list with
#'   \describe{
#'     \item{assignment}{data frame with columns `cluster`, `family_id`.}
#'     \item{method}{`"explicit"`, `"hclust"`, or `"graph"`.}
#'     \item{dendro}{The `compute_family_dendrogram()` result, or `NULL` if
#'       `method == "explicit"`; kept so a UI can preview/re-cut the
#'       dendrogram without recomputation.}
#'     \item{neighbors}{Neighbor information ready to pass to
#'       [generate_palette()] -- the supplied `connectivity` matrix if any,
#'       otherwise the cluster centroids -- so neighbor-aware coloring works
#'       without recomputing anything.}
#'   }
#' @export
detect_families <- function(pdata, k = NULL, h = NULL,
                             dist_method = "euclidean",
                             hclust_method = "average",
                             connectivity = NULL) {
  stopifnot(inherits(pdata, "palettome_data"))
  centroids <- compute_centroids(pdata)
  neighbors <- if (!is.null(connectivity)) connectivity else centroids

  if (isTRUE(pdata$has_family)) {
    assignment <- unique(pdata$cells[, c("cluster", "family")])
    names(assignment) <- c("cluster", "family_id")
    assignment <- assignment[order(assignment$cluster), ]
    rownames(assignment) <- NULL
    return(structure(
      list(
        assignment = assignment, method = "explicit", dendro = NULL,
        neighbors = neighbors
      ),
      class = "palettome_families"
    ))
  }

  dendro <- compute_family_dendrogram(
    pdata,
    dist_method = dist_method,
    hclust_method = hclust_method,
    connectivity = connectivity
  )
  assignment <- cut_families(dendro, k = k, h = h)
  structure(
    list(
      assignment = assignment,
      method = if (is.null(connectivity)) "hclust" else "graph",
      dendro = dendro,
      neighbors = neighbors
    ),
    class = "palettome_families"
  )
}

#' Plot a dendrogram preview of detected families
#'
#' A dependency-light preview: uses [dendextend](https://cran.r-project.org/package=dendextend)
#' to color branches by family when it is installed, and falls back to base
#' [stats::plot.hclust()] with [stats::rect.hclust()] otherwise. Either way
#' this only needs base R + stats to run.
#'
#' @param dendro A list from [compute_family_dendrogram()] (or the `$dendro`
#'   element of a [detect_families()] result).
#' @param k,h Passed to [cut_families()] / [stats::rect.hclust()] to draw the
#'   family cut.
#' @return Invisibly, the (possibly colored) dendrogram object that was
#'   plotted.
#' @export
plot_dendrogram <- function(dendro, k = NULL, h = NULL) {
  stopifnot(is.list(dendro), !is.null(dendro$hclust))
  hc <- dendro$hclust
  n <- length(dendro$clusters)
  if (is.null(k) && is.null(h)) k <- max(1L, round(sqrt(n)))

  if (requireNamespace("dendextend", quietly = TRUE)) {
    dend <- stats::as.dendrogram(hc)
    dend <- if (!is.null(h)) {
      dendextend::color_branches(dend, h = h)
    } else {
      dendextend::color_branches(dend, k = k)
    }
    graphics::plot(dend, main = "Cluster family dendrogram")
    return(invisible(dend))
  }

  graphics::plot(hc, main = "Cluster family dendrogram", xlab = "", sub = "")
  if (!is.null(h)) {
    stats::rect.hclust(hc, h = h)
  } else {
    stats::rect.hclust(hc, k = k)
  }
  invisible(hc)
}
