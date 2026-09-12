# Cache a Seurat object's palettome-relevant data to disk, so repeat runs
# don't need to reload the full Seurat object (counts, etc.) every time.
#
# Not part of the package (see .Rbuildignore) -- a dev-only convenience.
# Cached files land under dev/data/, which is gitignored: this data is
# large, machine-local, and (if it's real patient/sample data) shouldn't be
# committed. Only the small `palettome_data` table extracted by
# as_cluster_data() is cached -- never the Seurat object itself.

CACHE_DIR <- "dev/data"

#' Extract + cache the palettome_data for a Seurat object
#'
#' Run once per Seurat object (or whenever its embedding/clusters change).
#' See ?as_cluster_data for `reduction`/`dims`/`cluster_col`/`family_col`.
save_seurat_cache <- function(seurat_obj, name, reduction = "umap", dims = 1:2,
                               cluster_col = "seurat_clusters", family_col = NULL) {
  devtools::load_all(quiet = TRUE) # for as_cluster_data(), if not already loaded
  pdata <- as_cluster_data(seurat_obj, reduction = reduction, dims = dims,
                            cluster_col = cluster_col, family_col = family_col)
  dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  saveRDS(pdata, path)
  message("Cached ", nrow(pdata$cells), " cells -> ", path)
  invisible(path)
}

#' Load a previously cached palettome_data object
load_seurat_cache <- function(name) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop("No cache at '", path, "'. Build it once with save_seurat_cache().", call. = FALSE)
  }
  readRDS(path)
}

# ---- usage --------------------------------------------------------------
# One-time (in a session where your Seurat object `so` is already loaded):
#   source("dev/seurat_cache.R")
#   save_seurat_cache(so, name = "my_dataset", reduction = "umap",
#                      cluster_col = "seurat_clusters")
#
# Every session after that -- no Seurat object or `Seurat` package needed:
#   source("dev/seurat_cache.R")
#   pdata <- load_seurat_cache("Palette_merged_all_cells")
#   pdata <- load_seurat_cache("Palette_rough_selected_bcells")

