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
#' @param seed Random seed for reproducible subsampling.
#' @return A `palettome_data` object with a (possibly) reduced `cells` table.
#' @export
downsample_stratified <- function(pdata, max_n = 50000, min_per_cluster = 20, seed = 1) {
  stopifnot(inherits(pdata, "palettome_data"))
  cells <- pdata$cells
  n <- nrow(cells)
  if (n <= max_n) return(pdata)

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
