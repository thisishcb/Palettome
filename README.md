# Palettome

Hierarchy-aware, interactively editable color palettes for clustered data —
built for scRNA-seq clusters/cell subtypes (Seurat, UMAP) but not
hard-dependent on Seurat.

Clusters that belong to the same **family** (e.g. subtypes of T cells,
subtypes of myeloid cells) can be colored either:

- **harmonious** — family members share a base hue and vary only in
  shade/tint/tone, while distinct families get maximally separated hues, or
- **contrast** — every cluster gets a maximally distinguishable color, with
  family membership still visible through a secondary color channel (the
  family's own accent/tag color).

Every generated color can be overridden by hand, and manual overrides
survive re-running the generator (new mode, new seed, edited families) until
explicitly reset.

## Core vs. UI

**Core is fully independent of the UI.** `as_cluster_data()`,
`detect_families()`, `generate_palette()`, `plot_palette_static()`,
`plot_swatches()`, `export_palette_json()`/`export_palette_csv()`,
`import_palette_json()`, `downsample_stratified()`, `color_distance()`, and
`simulate_cvd()` depend only on `colorspace`, `farver`, `stats`, `graphics`,
`methods`, `utils`, and `jsonlite` — all listed in `Imports`, so a plain
`install.packages("palettome")` (or `devtools::load_all()`) gives you a
fully working package with zero UI packages installed.

The interactive UI is exactly one function, `launch_palettome_ui()`. It
requires `shiny`, `plotly`, `colourpicker`, and `shinyjs`, all listed under
`Suggests` only. If they aren't installed, `launch_palettome_ui()` fails
with a message telling you which packages to install and reminding you that
every other function still works without them — it never blocks or breaks
the rest of the package.

| Function | Core or UI |
|---|---|
| `as_cluster_data()`, `compute_centroids()` | Core |
| `detect_families()`, `compute_family_dendrogram()`, `cut_families()`, `plot_dendrogram()` | Core |
| `generate_palette()`, `set_manual_color()`, `reset_overrides()` | Core |
| `color_distance()`, `simulate_cvd()` | Core |
| `downsample_stratified()` | Core |
| `plot_palette_static()`, `plot_swatches()` | Core (ggplot2 used opportunistically if installed; base-R fallback otherwise) |
| `export_palette_json()`, `export_palette_csv()`, `import_palette_json()` | Core |
| `launch_palettome_ui()` | **UI** (Suggests-only: shiny, plotly, colourpicker, shinyjs) |

## Quick start (core only)

```r
devtools::load_all()  # or library(palettome) once installed

pdata <- as_cluster_data(
  my_df,
  coord_cols = c("UMAP_1", "UMAP_2"),
  cluster_col = "cluster"
)

fam <- detect_families(pdata, k = 4)          # auto-detect via hclust
session <- generate_palette(fam$assignment, mode = "harmonious", seed = 1)

plot_palette_static(pdata, session)           # ggplot2 if installed, base R otherwise
export_palette_json(session, "palette.json")
```

To resume editing later, or hand off manual overrides:

```r
session2 <- generate_palette(fam$assignment, session = session, mode = "contrast")
# clusters/families with manual_color == TRUE keep their color unchanged
```

## Interactive UI

```r
install.packages(c("shiny", "plotly", "colourpicker", "shinyjs"))
launch_palettome_ui(pdata)                    # auto-detects families
launch_palettome_ui(pdata, session = session) # resume a saved session
```

The app shows a WebGL (`plotly` `scattergl`) scatter plot of the embedding,
a family dendrogram, and a drag-and-drop family panel: drag a cluster chip
into a different family bin to regroup it, or click a chip/family header to
open a color picker. Mode, seed, lightness/chroma range, and a colorblind
simulation preview are all live. Sessions export as JSON/CSV, and the final
plot exports as a full-resolution PNG.

## The JSON schema (stable contract)

`export_palette_json()` writes, and `import_palette_json()` reads, this
structure. It is deliberately plain and language-agnostic — a future
Python/Scanpy implementation can read and write the exact same file without
any R-specific types, so it can share this UI (or a port of it) with zero
schema changes.

```json
{
  "schema_version": "1.0",
  "clusters": [
    { "cluster": "0", "family_id": "F1", "color": "#3E7CB1", "manual_color": false, "n_cells": 1523 },
    { "cluster": "1", "family_id": "F1", "color": "#7FB1DA", "manual_color": true,  "n_cells": 842 }
  ],
  "families": [
    { "family_id": "F1", "color": "#2C5F8A", "manual_color": false }
  ],
  "params": {
    "mode": "harmonious",
    "seed": 1,
    "lightness_range": [35, 85],
    "chroma_range": [35, 90],
    "hue_range": [0, 360],
    "cvd": "none"
  }
}
```

- `clusters[].color` is the per-cluster fill color actually used for plotting.
- `families[].color` is the family's representative/accent color: in
  harmonious mode it's the shared base hue clusters are shaded from; in
  contrast mode it's the secondary channel (e.g. an outline/tag color) that
  shows family membership even though cluster colors themselves are
  maximally spread apart.
- `manual_color: true` marks an entry a person set by hand; `generate_palette()`
  never overwrites these unless `reset_overrides()` is called first.
- `n_cells` is informational (legend sizing, etc.) and optional/nullable.

CSV export (`export_palette_csv()`) writes the `clusters` table (and
optionally the `families` table) as plain CSV with the same column names.

## Data input contract

`as_cluster_data()` normalizes either input into one `palettome_data` object
so every other function only has to know one shape:

- **Data frame**: `as_cluster_data(df, coord_cols = c("UMAP_1","UMAP_2"[,"UMAP_3"]), cluster_col = "cluster", family_col = NULL, cell_id_col = NULL)`
- **Seurat object**: `as_cluster_data(seurat_obj, reduction = "umap", dims = 1:2, cluster_col = "seurat_clusters", family_col = NULL)` (requires the `Seurat` package, Suggests-only, only touched by this one method)

If `family_col`/an explicit family is supplied, `detect_families()` uses it
directly; otherwise it hierarchically clusters cluster centroids (or a
supplied cluster-cluster connectivity/distance matrix) and lets you pick a
cut (`k` or `h`), with `plot_dendrogram()` for a preview. Either way, the
resulting `cluster -> family_id` assignment is just a plain data frame you
(or the UI's drag-and-drop panel) can freely rewrite afterward — detection
is a starting point, not a final answer.

## Performance for large datasets

See [`inst/benchmark/`](inst/benchmark/) for the full write-up. In short:
the browser never receives one point per cell for interactive editing.
`downsample_stratified()` precomputes a per-cluster-stratified subsample
(default cap 50,000 points) server-side for live dragging/recoloring, and
`plotly`'s WebGL `scattergl` trace renders that subsample smoothly; a full,
un-subsampled render is only produced on final PNG export via
`plot_palette_static()`. Cluster centroids, hierarchy, and per-cluster cell
counts are precomputed once (`compute_centroids()`) so hierarchy edits and
recoloring never touch the raw per-cell table at all.

## Tests

```r
devtools::test()
```

Covers: perceptual-distance thresholds for contrast-mode selection, hue
separation between families in harmonious mode, and that manual overrides
survive `generate_palette()` re-runs (including across a family
reassignment).
