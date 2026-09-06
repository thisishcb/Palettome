# Manual smoke-test / exploratory walkthrough of the palettome package.
#
# Not part of the package itself (see .Rbuildignore) -- this is a
# developer script for eyeballing that everything still works after a
# change, without writing a formal test. Run it a section at a time in a
# live R session (see dev/README.md for the recommended workflow), or all
# at once with:
#   Rscript dev/manual_check.R

devtools::load_all(quiet = TRUE)

# ---- 1. Build a small synthetic dataset (12 clusters, 4 spatial groups) --
set.seed(1)
df <- do.call(rbind, lapply(seq_len(12), function(i) {
  group <- (i - 1) %/% 3
  cx <- (group %% 2) * 20
  cy <- (group %/% 2) * 20
  data.frame(
    UMAP_1 = rnorm(80, cx, 1), UMAP_2 = rnorm(80, cy, 1),
    cluster = paste0("c", i)
  )
}))
pdata <- as_cluster_data(df, coord_cols = c("UMAP_1", "UMAP_2"), cluster_col = "cluster")
str(pdata, max.level = 1)

# ---- 2. Family detection + dendrogram preview -----------------------------
fam <- detect_families(pdata, k = 4)
print(fam$assignment)
plot_dendrogram(fam$dendro, k = 4) # opens a plot window/device

# ---- 3. Generate a palette, look at it -------------------------------------
sess <- generate_palette(fam$assignment, mode = "harmonious", seed = 1)
print(sess$clusters)
print(sess$families)
plot_palette_static(pdata, sess) # ggplot2 object if installed -> auto-prints
plot_swatches(sess) # base-graphics swatch/legend figure

# ---- 4. Try contrast mode + a colorblind preview ---------------------------
sess_contrast <- generate_palette(fam$assignment, mode = "contrast", seed = 1)
plot_palette_static(pdata, sess_contrast, cvd = "deutan")

# ---- 5. Manual override survives regeneration + a family move -------------
sess2 <- set_manual_color(sess, cluster = "c1", color = "#FF00AA")
moved <- fam$assignment
moved$family_id[moved$cluster == "c1"] <- "F3"
sess3 <- generate_palette(moved, session = sess2, mode = "contrast", seed = 2)
stopifnot(sess3$clusters$color[sess3$clusters$cluster == "c1"] == "#FF00AA")
cat("OK: manual override on c1 survived mode switch + family move\n")

# ---- 6. Export round-trip --------------------------------------------------
tmp <- tempfile(fileext = ".json")
export_palette_json(sess3, tmp)
sess4 <- import_palette_json(tmp)
stopifnot(identical(sess3$clusters$color, sess4$clusters$color))
cat("OK: JSON export/import round-trip matches\n")

# ---- 7. Launch the interactive UI for a hands-on check ---------------------
# Uncomment to actually open it (requires shiny/plotly/colourpicker/shinyjs):
# launch_palettome_ui(pdata, families = fam)
