#' Build a palettome data object from a data frame or Seurat object
#'
#' `as_cluster_data()` normalizes either a plain data frame of embedding
#' coordinates + cluster labels, or a `Seurat` object, into the common
#' `palettome_data` structure that every other core function (
#' [detect_families()], [generate_palette()], [plot_palette_static()], ...)
#' consumes. This is the only place that needs to know about Seurat, and
#' Seurat is never required to be installed to use the rest of the package.
#'
#' @param x A `data.frame` (or object coercible to one) or a `Seurat` object.
#' @param ... Passed to methods.
#'
#' @return A `palettome_data` object: a list with elements
#'   \describe{
#'     \item{cells}{A data frame with columns `cell_id`, the coordinate
#'       columns (as named in `coord_cols`), `cluster`, and optionally
#'       `family`.}
#'     \item{coord_cols}{Character vector of coordinate column names in
#'       `cells`, in order (length 2 or 3).}
#'     \item{has_family}{Logical; whether an explicit family column was
#'       supplied.}
#'   }
#' @export
as_cluster_data <- function(x, ...) {
  UseMethod("as_cluster_data")
}

#' @rdname as_cluster_data
#' @param coord_cols Character vector of 2 or 3 column names holding
#'   embedding coordinates (e.g. `c("UMAP_1", "UMAP_2")`).
#' @param cluster_col Name of the column holding cluster/subtype labels.
#' @param family_col Optional name of a column holding an explicit
#'   parent/family label. When `NULL` (default), families are left
#'   undetected and must be filled in by [detect_families()].
#' @param cell_id_col Optional name of a column with unique per-row/cell
#'   identifiers. When `NULL`, row numbers are used.
#' @export
as_cluster_data.data.frame <- function(x, coord_cols, cluster_col,
                                        family_col = NULL,
                                        cell_id_col = NULL, ...) {
  stopifnot(
    "`coord_cols` must have length 2 or 3" = length(coord_cols) %in% c(2L, 3L),
    "all `coord_cols` must be columns of `x`" = all(coord_cols %in% names(x)),
    "`cluster_col` must be a column of `x`" = cluster_col %in% names(x)
  )
  if (!is.null(family_col) && !family_col %in% names(x)) {
    stop("`family_col` = '", family_col, "' is not a column of `x`", call. = FALSE)
  }

  cell_id <- if (!is.null(cell_id_col)) {
    stopifnot(cell_id_col %in% names(x))
    as.character(x[[cell_id_col]])
  } else {
    as.character(seq_len(nrow(x)))
  }

  cells <- data.frame(cell_id = cell_id, stringsAsFactors = FALSE)
  for (cc in coord_cols) cells[[cc]] <- as.numeric(x[[cc]])
  cells$cluster <- as.character(x[[cluster_col]])
  has_family <- !is.null(family_col)
  if (has_family) cells$family <- as.character(x[[family_col]])

  structure(
    list(cells = cells, coord_cols = coord_cols, has_family = has_family),
    class = "palettome_data"
  )
}

#' @rdname as_cluster_data
#' @param reduction Name of the dimensional reduction to pull coordinates
#'   from (passed to `Seurat::Embeddings()`), e.g. `"umap"`.
#' @param dims Integer vector of length 2 or 3 selecting which dimensions of
#'   `reduction` to use.
#' @export
as_cluster_data.Seurat <- function(x, reduction = "umap", dims = 1:2,
                                    cluster_col = "seurat_clusters",
                                    family_col = NULL, ...) {
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop(
      "Reading a Seurat object requires the 'Seurat' package. ",
      "Install it with install.packages('Seurat'), or extract a plain ",
      "data frame yourself (embeddings + metadata) and call ",
      "as_cluster_data.data.frame() instead.",
      call. = FALSE
    )
  }
  emb <- Seurat::Embeddings(x, reduction = reduction)
  stopifnot(
    "`dims` must have length 2 or 3" = length(dims) %in% c(2L, 3L),
    "`dims` exceed the number of columns in the requested reduction" =
      max(dims) <= ncol(emb)
  )
  coord_cols <- colnames(emb)[dims]
  meta <- x[[]]
  stopifnot(cluster_col %in% names(meta))
  if (!is.null(family_col)) stopifnot(family_col %in% names(meta))

  df <- as.data.frame(emb[, dims, drop = FALSE])
  df$.cell_id <- rownames(emb)
  df[[cluster_col]] <- meta[[cluster_col]]
  if (!is.null(family_col)) df[[family_col]] <- meta[[family_col]]

  as_cluster_data.data.frame(
    df,
    coord_cols = coord_cols,
    cluster_col = cluster_col,
    family_col = family_col,
    cell_id_col = ".cell_id"
  )
}

#' Compute per-cluster centroids and cell counts
#'
#' Precomputes cluster centroids (mean coordinate per cluster), cell counts,
#' and (if present) the family each cluster belongs to. Every downstream
#' interactive operation (recoloring, regrouping) should be able to work off
#' this small summary rather than touching `pdata$cells` again.
#'
#' @param pdata A `palettome_data` object from [as_cluster_data()].
#' @return A data frame with one row per cluster: `cluster`, one column per
#'   coordinate named `<coord>_centroid`, `n_cells`, and `family` if
#'   `pdata$has_family` is `TRUE`.
#' @export
compute_centroids <- function(pdata) {
  stopifnot(inherits(pdata, "palettome_data"))
  cells <- pdata$cells
  clusters <- sort(unique(cells$cluster))

  out <- lapply(clusters, function(cl) {
    rows <- cells[cells$cluster == cl, , drop = FALSE]
    row <- list(cluster = cl)
    for (cc in pdata$coord_cols) {
      row[[paste0(cc, "_centroid")]] <- mean(rows[[cc]], na.rm = TRUE)
    }
    row$n_cells <- nrow(rows)
    if (pdata$has_family) row$family <- rows$family[1]
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
