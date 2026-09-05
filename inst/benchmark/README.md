# Performance validation: 1-5 million cells

## The approach, and why

Rendering one point per cell directly in the browser does not scale to
millions of cells, regardless of whether it's SVG, canvas-2D, or naive
WebGL: at 1-5M points, per-frame geometry upload and re-layout during
interactive editing (dragging a cluster between families, dragging a color
picker) will drop frame rate to unusable levels well before 1M points on
typical hardware, and the *data itself* (JSON-serializing coordinates for
millions of rows to the browser on every reactive update) becomes the
bottleneck before the renderer does.

Palettome avoids this with two combined, complementary strategies (as
proposed in the project brief) rather than picking just one:

1. **Server-side stratified downsampling for interaction**
   (`downsample_stratified()`, core, no UI dependency): a per-cluster
   stratified subsample capped at a fixed size (default 50,000 points) is
   computed once per dataset and is what the live view actually redraws on
   every hierarchy edit or recolor. Because it's stratified per cluster
   (with a `min_per_cluster` floor), rare cell types stay visible instead of
   being drowned out by a uniform random sample. Interactive fidelity is
   capped, not exact -- which is the explicit tradeoff the project brief
   asked for ("prioritize interaction smoothness ... over exact per-cell
   fidelity during editing").
2. **GPU-accelerated rendering for what *is* sent**
   (`plotly`'s WebGL `scattergl` trace in the Shiny UI): even the capped
   preview uses WebGL rather than SVG/canvas-2D, so panning/zooming/legend
   toggling on the ~50k-point preview stays smooth with headroom to spare,
   and the cap can comfortably be raised (see below) if a given machine can
   afford it.

Everything that drives interactive editing itself -- family detection,
palette generation, drag-to-regroup, click-to-recolor -- operates only on
precomputed **per-cluster** summaries (`compute_centroids()`,
`palettome_session` cluster/family tables), never on the raw per-cell
table. A full, un-subsampled render is only ever produced once, on export
(`plot_palette_static()` / the "Export PNG" button), which is expected to
take a few seconds rather than being interactive.

## What was actually measured here, and what wasn't

This sandbox has no display and no headless-browser tooling installed
(`chromote`/`webshot2` were not available and installing a full Chrome
binary was out of scope for this session), so **no live in-browser
frame-rate number is reported here** -- claiming one without measuring it
would be worse than not claiming it. What *is* measured, reproducibly, via
`Rscript inst/benchmark/benchmark.R` (results in `results.csv`), is the
server-side half of the architecture, which is what the mitigation strategy
above actually depends on:

- that every interactive-editing operation is `O(n_clusters)`, not
  `O(n_cells)`, and so its cost is flat from 100k to 5M cells;
- that `downsample_stratified()` caps the payload that would be sent to the
  browser at a constant size regardless of total cell count; and
- that a genuine full-data export stays in the low seconds even at 5M
  cells, using a rasterized (not per-point-vector) renderer.

The WebGL-stays-smooth-at-~50k-points claim itself rests on `plotly`
scattergl's documented design intent and widely-reported practical range
(tens of thousands to a few hundred thousand points interactively; degrading
well before that on SVG/canvas-2D at the same count) rather than a
measurement taken in this session -- flagged here explicitly rather than
presented as measured.

## Results

Machine: Apple M4 Pro, 51 GB RAM, R 4.4.2. Synthetic dataset: 32 clusters
in 8 families (see `generate_synthetic.R`), purely-synthetic Gaussian blobs
(no Splatter / no simulated expression matrix), log-normal cluster-size
imbalance. Full sweep: `Rscript inst/benchmark/benchmark.R`.

| n_cells | compute_centroids | detect_families (hclust) | generate_palette | downsample_stratified | full-data PNG render | full in-memory | preview in-memory |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 100,000   | 0.02s | 0.02s | 0.04s | 0.01s | 0.08s | 8.8 MB   | 4.6 MB |
| 500,000   | 0.10s | 0.08s | 0.02s | 0.01s | 0.33s | 44.0 MB  | 4.6 MB |
| 1,000,000 | 0.17s | 0.17s | 0.02s | 0.02s | 0.66s | 88.0 MB  | 4.6 MB |
| 2,000,000 | 0.36s | 0.29s | 0.02s | 0.03s | 1.42s | 176.0 MB | 4.6 MB |
| 5,000,000 | 0.81s | 0.73s | 0.02s | 0.08s | 3.33s | 440.0 MB | 4.6 MB |

(`generate_palette` column is harmonious + contrast combined; both
individually stay under 40ms at every size, since they only ever touch the
32-row cluster/family assignment.)

Takeaways:

- **Interactive operations are flat and fast.** `generate_palette()` --
  what fires on every drag-to-regroup or click-to-recolor -- costs ~1-40ms
  regardless of whether the dataset has 100k or 5M cells, because it never
  touches `pdata$cells`. `detect_families()`/`compute_centroids()` do touch
  raw cells once (to compute centroids) and grow linearly with cell count,
  but stay under 1 second even at 5M cells since they're only run when
  (re-)detecting families from scratch, not on every edit.
- **The preview payload is genuinely constant.** 4.6 MB regardless of
  whether the source dataset is 8.8 MB (100k cells) or 440 MB (5M cells) --
  this is the number that actually matters for keeping the browser
  responsive, since it's what plotly has to serialize and upload to WebGL
  on every redraw.
- **Full export stays interactive-adjacent even at 5M cells** (3.3s) using
  a rasterized renderer (`pch="."` base graphics, or equivalently
  `ggplot2` with a rasterizing device) -- well short of anything a user
  would perceive as "hung," while still faithfully drawing every cell in
  the final artifact.

## Tuning

`preview_max_n` (passed to `launch_palettome_ui()` and
`downsample_stratified()` directly) is the one knob that trades interactive
fidelity for smoothness: lower it on modest hardware or very high cluster
counts, raise it if a target machine handles more comfortably. The
per-cluster floor (`min_per_cluster`, default 20) ensures raising or
lowering this cap doesn't erase small clusters from the preview.
