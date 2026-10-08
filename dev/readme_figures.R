# Regenerates the showcase figures used in README.md (written to man/figures/).
# Run from the package root:  Rscript dev/readme_figures.R
# Needs ggplot2, scales, patchwork and ragg (dev-only; not package dependencies).

devtools::load_all(quiet = TRUE)
library(ggplot2)
library(patchwork)

out_dir <- file.path("man", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- synthetic "scRNA-seq-like" embedding --------------------------------
# Four compartments, each a loose island of subclusters, as in a typical UMAP.
set.seed(2026)
compartments <- list(
  T       = list(center = c(-6,  4), subs = c("CD4 naive", "CD4 mem", "Treg", "CD8 eff", "CD8 mem")),
  Myeloid = list(center = c( 6,  3), subs = c("cMono", "ncMono", "Macro", "cDC")),
  B       = list(center = c(-1, -6), subs = c("B naive", "B mem", "Plasma")),
  NK      = list(center = c( 7, -5), subs = c("NK bright", "NK dim"))
)
cells <- do.call(rbind, lapply(names(compartments), function(comp) {
  info <- compartments[[comp]]
  k <- length(info$subs)
  ang <- seq(0, 2 * pi, length.out = k + 1)[seq_len(k)] + stats::runif(1, 0, pi)
  do.call(rbind, lapply(seq_len(k), function(i) {
    n <- sample(250:600, 1)
    ctr <- info$center + 1.9 * c(cos(ang[i]), sin(ang[i]))
    data.frame(
      UMAP_1 = stats::rnorm(n, ctr[1], 0.75),
      UMAP_2 = stats::rnorm(n, ctr[2], 0.75),
      cluster = info$subs[i],
      compartment = comp
    )
  }))
}))
cells$cluster <- factor(cells$cluster, levels = unlist(lapply(compartments, `[[`, "subs")))

pdata <- as_cluster_data(cells, coord_cols = c("UMAP_1", "UMAP_2"),
                         cluster_col = "cluster", family_col = "compartment")
fam <- detect_families(pdata)

harmonious <- generate_palette(fam$assignment, mode = "harmonious",
                               neighbors = fam$neighbors, seed = 10)
contrast <- generate_palette(fam$assignment, mode = "contrast",
                             neighbors = fam$neighbors, seed = 1)

theme_umap <- theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 2)),
        plot.subtitle = element_text(hjust = 0.5, colour = "grey40", margin = margin(b = 6)),
        legend.position = "none",
        plot.margin = margin(6, 6, 6, 6))

umap_panel <- function(cols, title, subtitle) {
  ggplot(cells, aes(UMAP_1, UMAP_2, colour = cluster)) +
    geom_point(size = 0.9, alpha = 0.9, stroke = 0) +
    scale_colour_manual(values = cols) +
    coord_equal() +
    labs(title = title, subtitle = subtitle) +
    theme_umap
}

n_cl <- nlevels(cells$cluster)
default_cols <- stats::setNames(scales::hue_pal()(n_cl), levels(cells$cluster))

hero <- umap_panel(default_cols, "ggplot2 default", "every cluster unrelated") +
  umap_panel(cluster_colors(harmonious), "palettome: harmonious", "families share a hue region") +
  umap_panel(cluster_colors(contrast), "palettome: contrast", "max separation, family-aware")
ggsave(file.path(out_dir, "readme-hero.png"), hero, width = 12, height = 4.3,
       dpi = 110, device = ragg::agg_png, bg = "white")

# ---- palette gallery ----------------------------------------------------
fam_order <- names(compartments)
cluster_order <- levels(cells$cluster)

strip_df <- function(session, label) {
  df <- cluster_colors(session, format = "data.frame")
  df$family_id <- factor(df$family_id, levels = fam_order)
  df <- df[order(df$family_id, match(df$cluster, cluster_order)), ]
  # leave a small gap between families so the grouping is visible
  df$x <- seq_len(nrow(df)) + 0.35 * (as.integer(df$family_id) - 1)
  df$row <- label
  df
}

gallery_rows <- list(
  "harmonious, seed = 3" = generate_palette(fam$assignment, neighbors = fam$neighbors, seed = 3),
  "harmonious, seed = 9" = generate_palette(fam$assignment, neighbors = fam$neighbors, seed = 9),
  "harmonious, seed = 10" = harmonious,
  "sweep_anchors = navy > coral > gold" = generate_palette(
    fam$assignment, neighbors = fam$neighbors,
    sweep_anchors = c("#0B3D91", "#FF5733", "#FFD166")),
  "per_family, seed = 1" = generate_palette(fam$assignment, harmonious_style = "per_family",
                                            neighbors = fam$neighbors, seed = 1),
  "contrast, seed = 1" = contrast
)
gallery <- do.call(rbind, Map(strip_df, gallery_rows, names(gallery_rows)))
gallery$row <- factor(gallery$row, levels = rev(names(gallery_rows)))

fam_labels <- aggregate(x ~ family_id, data = gallery[gallery$row == levels(gallery$row)[1], ],
                        FUN = mean)

g_gallery <- ggplot(gallery, aes(x, row, fill = color)) +
  geom_tile(width = 0.94, height = 0.78) +
  scale_fill_identity() +
  scale_x_continuous(breaks = fam_labels$x, labels = fam_labels$family_id,
                     position = "top", expand = expansion(add = 0.3)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(hjust = 1, colour = "grey20", family = "mono"),
        axis.text.x.top = element_text(face = "bold", colour = "grey20"),
        plot.margin = margin(6, 10, 6, 6))
ggsave(file.path(out_dir, "readme-gallery.png"), g_gallery, width = 9, height = 3.4,
       dpi = 110, device = ragg::agg_png, bg = "white")

# ---- colour-vision-deficiency preview -------------------------------------
cvd_rows <- lapply(c("none", "deutan", "protan", "tritan"), function(type) {
  df <- strip_df(contrast, if (type == "none") "original" else type)
  if (type != "none") df$color <- simulate_cvd(df$color, type = type)
  df
})
cvd <- do.call(rbind, cvd_rows)
cvd$row <- factor(cvd$row, levels = rev(c("original", "deutan", "protan", "tritan")))

g_cvd <- g_gallery %+% cvd
ggsave(file.path(out_dir, "readme-cvd.png"), g_cvd, width = 9, height = 2.4,
       dpi = 110, device = ragg::agg_png, bg = "white")

message("Wrote figures to ", normalizePath(out_dir))
