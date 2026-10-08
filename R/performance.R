#' Stratified per-cluster subsample for interactive preview
#'
#' Precomputing a small, cluster-stratified subsample is the core trick that
#' keeps hierarchy editing and recoloring responsive on multi-million-cell
#' datasets: interactive redraws use this subsample, and only a final export
#' needs to touch every cell. See `inst/benchmark/` for a benchmark
#' justifying the approach and default threshold.
#'
#' @param pdata A `palettome_data` object.
#' @param max_n Target total number of points after subsampling. If
#'   `nrow(pdata$cells) <= max_n`, `pdata` is returned unchanged.
#' @param min_per_cluster Minimum points kept per cluster even if that
#'   over-represents small clusters relative to a uniform per-cluster share,
#'   so rare clusters stay visible.
#' @param seed Random seed for reproducible subsampling. The caller's random
#'   number state is restored on exit.
#' @return A `palettome_data` object with a (possibly) reduced `cells` table.
#' @export
#' @examples
#' cells <- data.frame(
#'   x = rnorm(120, rep(c(0, 1, 6, 7, 3, 4), each = 20)),
#'   y = rnorm(120, rep(c(0, 1, 0, 1, 6, 7), each = 20)),
#'   cluster = rep(c("T1", "T2", "B1", "B2", "M1", "M2"), each = 20)
#' )
#' pdata <- as_cluster_data(cells, coord_cols = c("x", "y"), cluster_col = "cluster")
#' small <- downsample_stratified(pdata, max_n = 60, min_per_cluster = 5)
#' table(small$cells$cluster)
downsample_stratified <- function(pdata, max_n = 50000, min_per_cluster = 20, seed = 1) {
  stopifnot(inherits(pdata, "palettome_data"))
  cells <- pdata$cells
  n <- nrow(cells)
  if (n <= max_n) return(pdata)

  .preserve_rng_state()
  set.seed(seed)
  clusters <- split(seq_len(n), cells$cluster)
  n_clusters <- length(clusters)
  share <- max_n / n

  keep_idx <- unlist(lapply(clusters, function(idx) {
    k <- max(min_per_cluster, round(length(idx) * share))
    k <- min(k, length(idx))
    sample(idx, k)
  }), use.names = FALSE)

  pdata$cells <- cells[sort(keep_idx), , drop = FALSE]
  pdata
}
