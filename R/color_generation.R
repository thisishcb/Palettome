#' @importFrom stats setNames
NULL

# ---- perceptual helpers ---------------------------------------------------

#' Pairwise perceptual color distance
#'
#' Thin wrapper over [farver::compare_colour()] in CIE Lab space, used both
#' by the contrast-mode selector and by the package tests to check that
#' generated colors respect a minimum perceptual separation.
#'
#' @param hex Character vector of hex colors.
#' @param method Distance method passed to `farver::compare_colour()`
#'   (`"CIE2000"`, `"CIE94"`, `"CIE1976"`/`"euclidean"`, ...).
#' @return A symmetric numeric matrix of pairwise distances, named by `hex`.
#' @export
color_distance <- function(hex, method = "CIE2000") {
  lab <- farver::decode_colour(hex, to = "lab")
  d <- farver::compare_colour(lab, lab, from_space = "lab", method = method)
  dimnames(d) <- list(hex, hex)
  d
}

#' Simulate color-vision deficiency on a palette
#'
#' Preview-only helper: does not mutate a session, just maps colors through
#' [colorspace::deutan()]/[colorspace::protan()]/[colorspace::tritan()] so a
#' UI can toggle a colorblind-safe preview of an already-generated palette.
#'
#' @param hex Character vector of hex colors.
#' @param type One of `"deutan"`, `"protan"`, `"tritan"`.
#' @param severity Severity in `[0, 1]`, passed through to colorspace.
#' @return Character vector of simulated hex colors, same length as `hex`.
#' @export
simulate_cvd <- function(hex, type = c("deutan", "protan", "tritan"), severity = 1) {
  type <- match.arg(type)
  fn <- switch(type,
    deutan = colorspace::deutan,
    protan = colorspace::protan,
    tritan = colorspace::tritan
  )
  fn(hex, severity = severity)
}

# `cvd` distance space: colors as they will actually look to the viewer, so
# every distance-based decision (family hue spacing, contrast selection)
# optimizes for what matters.
.perceptual_hex <- function(hex, cvd) {
  if (identical(cvd, "none") || is.null(cvd)) hex else simulate_cvd(hex, type = cvd)
}

# ---- lightness / chroma range resolution -----------------------------

# Presets used when a range is "auto" and there are no manual colors to
# take a cue from. Tuned to the reference look: contrast = vivid mid
# lightness; harmonious "sweep" = wide monotone lightness, muted chroma;
# harmonious "per_family" = moderate both.
.auto_range_preset <- function(kind, mode, harmonious_style, n_items = 8) {
  hard <- if (kind == "L") c(16, 94) else c(4, 96)
  rng <- if (mode == "contrast") {
    # widen the working volume as more distinct colors are needed
    w <- min(1, max(0, (n_items - 6) / 30))
    if (kind == "L") c(50 - 12 * w, 76 + 6 * w) else c(36 - 8 * w, 74 + 16 * w)
  } else if (kind == "L") {
    if (harmonious_style == "sweep") c(34, 82) else c(42, 80)
  } else {
    if (harmonious_style == "sweep") c(24, 46) else c(34, 70)
  }
  pmin(pmax(rng, hard[1]), hard[2])
}

.resolve_range <- function(spec, kind, manual_hex, mode, harmonious_style, n_items = 8) {
  if (is.numeric(spec) && length(spec) == 2) return(sort(as.numeric(spec)))
  if (!identical(spec, "auto")) {
    stop("`", kind, "` range must be a length-2 numeric vector or \"auto\"", call. = FALSE)
  }
  hard <- if (kind == "L") c(16, 94) else c(4, 96)
  min_span <- if (kind == "L") 24 else 16

  if (length(manual_hex) >= 1) {
    hcl <- .to_hcl(manual_hex)
    v <- if (kind == "L") hcl[, "l"] else hcl[, "c"]
    if (length(v) == 1) {
      pad <- min_span / 2 + if (kind == "L") 6 else 4
      rng <- c(v - pad, v + pad)
    } else {
      rng <- as.numeric(stats::quantile(v, c(0.05, 0.95), names = FALSE))
      if (diff(rng) < min_span) {
        mid <- mean(rng)
        rng <- c(mid - min_span / 2, mid + min_span / 2)
      }
    }
    return(pmin(pmax(sort(rng), hard[1]), hard[2]))
  }
  .auto_range_preset(kind, mode, harmonious_style, n_items)
}

# ---- family base hues --------------------------------------------------

# manual_hue: named numeric (family_id -> hue in degrees) for families that
# already carry a manual color; those are pinned and the rest placed around
# them. forder: integer permutation of seq_along(family_ids) giving the
# spatial order of families. fadj: logical family x family adjacency, or
# NULL when no neighbor information is available.
.assign_family_hues <- function(family_ids, forder, fadj, seed, neighbor_hues,
                                 hue_range, harmonious_style, manual_hue = NULL) {
  n <- length(family_ids)
  hues <- stats::setNames(rep(NA_real_, n), family_ids)
  if (!is.null(manual_hue)) {
    for (fid in intersect(names(manual_hue), family_ids)) hues[fid] <- manual_hue[[fid]]
  }
  if (n == 0) return(hues)
  set.seed(seed)
  span_full <- diff(hue_range)
  start <- (hue_range[1] + stats::runif(1, 0, span_full)) %% 360
  ord_ids <- family_ids[forder]

  if (n == 1) {
    if (is.na(hues[1])) hues[1] <- start
    return(hues)
  }

  if (neighbor_hues == "coherent") {
    arc <- if (harmonious_style == "sweep") min(span_full, 150) else min(span_full, 280)
    start <- .best_arc_start(arc, hue_range, seed)
    grid <- (start + seq(0, arc, length.out = n)) %% 360
    # align the grid to any manual pin (rotate so the pinned family's slot
    # matches its actual hue), then fill the rest in spatial order.
    pinned <- which(!is.na(hues[ord_ids]))
    if (length(pinned)) {
      shift <- .circ_signed(grid[pinned[1]], hues[ord_ids][pinned[1]])
      grid <- (grid - shift) %% 360
    }
    free <- which(is.na(hues[ord_ids]))
    hues[ord_ids[free]] <- grid[free]
    return(hues)
  }

  # neighbor_hues == "contrast": evenly spaced hue slots, greedily assigned
  # so each family sits as far as possible (on the wheel) from its already
  # placed spatial neighbors, with a nudge away from the muddy zone.
  slots <- (start + seq(0, 360, length.out = n + 1)[seq_len(n)]) %% 360
  used <- rep(FALSE, n)
  for (fid in names(manual_hue %||% list())) {
    if (!fid %in% family_ids) next
    j <- which.min(.circ_dist(slots, hues[fid]))
    used[j] <- TRUE
  }
  for (fid in ord_ids) {
    if (!is.na(hues[fid])) next
    nb <- if (is.null(fadj)) character(0) else family_ids[fadj[fid, ]]
    nb_h <- hues[nb]
    nb_h <- nb_h[!is.na(nb_h)]
    cand <- which(!used)
    score <- vapply(cand, function(j) {
      h <- slots[j]
      base <- if (length(nb_h)) min(.circ_dist(h, nb_h)) else 180
      base - 12 * .hue_muddiness_penalty(h)
    }, numeric(1))
    pick <- cand[which.max(score)]
    hues[fid] <- slots[pick]
    used[pick] <- TRUE
  }
  na <- which(is.na(hues))
  if (length(na)) hues[na] <- .golden_hues(length(na), start)
  hues
}

# signed circular difference a - b, in (-180, 180]
.circ_signed <- function(a, b) {
  d <- (a - b) %% 360
  ifelse(d > 180, d - 360, d)
}

# ---- within-family member colors ------------------------------------

# corder: seriation order of the family's members (permutation of seq_along
# members). Members that are spatially adjacent get lightness slots that are
# far apart (feature: neighboring subclusters -> far-away shades).
.family_member_colors <- function(members, corder, fam_hue, l_band, c_band,
                                   harmonious_style) {
  m <- length(members)
  if (m == 0) return(stats::setNames(character(0), character(0)))
  if (m == 1) {
    col <- .hcl_hex(fam_hue, mean(c_band), mean(l_band))
    return(stats::setNames(col, members))
  }

  spread <- .spread_slots(m, cyclic = FALSE)
  spatial_rank <- order(corder)
  l_grid <- seq(l_band[1], l_band[2], length.out = m)
  if (harmonious_style == "sweep") {
    # near-constant chroma, essentially one hue: the multi-hue character of
    # the palette comes from the family-to-family progression, not from
    # within-family variation (matches the reference sweep's triplets).
    c_grid <- rep(mean(c_band), m)
    subarc <- 0
  } else {
    # graded shades of the family hue: more chroma when darker.
    c_grid <- seq(c_band[2] * 0.92, c_band[1], length.out = m)
    subarc <- 6
  }

  out <- character(m)
  for (i in seq_len(m)) {
    slot <- spread[spatial_rank[i]]
    h <- (fam_hue + (slot - (m + 1) / 2) / m * subarc) %% 360
    out[i] <- .hcl_hex(h, c_grid[slot], l_grid[slot])
  }
  stats::setNames(out, members)
}

# Piecewise-linear H/L/C path through a sequence of user-supplied anchor
# colors (decoded to HCL), at positions tt in [0,1]. Hue is unwrapped first
# so interpolation between consecutive anchors always takes the shorter way
# around the wheel rather than potentially the long way through 0/360.
.anchor_path <- function(anchors, tt) {
  hcl <- .to_hcl(anchors)
  at <- seq(0, 1, length.out = nrow(hcl))
  h_unwrapped <- hcl[, "h"]
  for (i in seq_along(h_unwrapped)[-1]) {
    h_unwrapped[i] <- h_unwrapped[i - 1] + .circ_signed(h_unwrapped[i], h_unwrapped[i - 1])
  }
  list(
    h = stats::approx(at, h_unwrapped, xout = tt, rule = 2)$y %% 360,
    l = stats::approx(at, hcl[, "l"], xout = tt, rule = 2)$y,
    c = stats::approx(at, hcl[, "c"], xout = tt, rule = 2)$y
  )
}

# Global analogous "sweep": one monotone path through HCL space, cut into
# contiguous segments assigned to families in spatial order. Within each
# family segment the path positions are permuted (.spread_slots) so
# spatially-adjacent clusters land far apart on the segment -> local shade
# contrast without breaking the global sweep.
#
# By default the path is auto-generated (rising lightness, an analogous hue
# arc placed to skip the muddy yellow-green zone, a gentle chroma bulge). If
# `anchors` (2+ hex colors) is supplied, the path instead interpolates
# through exactly those colors in HCL space -- a user-specified gradient to
# sweep the palette along, instead of the automatic one.
#
# manual_family_hue (named family_id -> hue) lets a manually-recolored
# family override just the hue channel of its own segment, so its members
# automatically re-shade around the color the user picked, while keeping
# the lightness/chroma schedule (and thus the family's place in the global
# ramp) intact -- this applies whether the base path is automatic or
# anchor-driven.
.harmonious_sweep <- function(ord_fam, member_order, l_range, c_range,
                               hue_range, seed, manual_family_hue = NULL,
                               anchors = NULL) {
  ordered <- unlist(member_order[ord_fam], use.names = FALSE)
  N <- length(ordered)
  empty <- stats::setNames(character(0), character(0))
  if (N == 0) return(list(cluster = empty, family = empty))

  if (length(anchors) >= 2) {
    # endpoint-inclusive: the first/last cluster should actually land on the
    # first/last anchor the user gave, not an inset bin-centered position.
    pos <- if (N == 1) 0.5 else (seq_len(N) - 1) / (N - 1)
    ap <- .anchor_path(anchors, pos)
    h_path <- ap$h
    l_path <- ap$l
    c_path <- ap$c
  } else {
    tt <- if (N == 1) 0.5 else (seq_len(N) - 0.5) / N
    arc <- min(diff(hue_range), if (N <= 4) 85 else if (N <= 9) 120 else 150)
    start <- .best_arc_start(arc, hue_range, seed)
    h_path <- (start + tt * arc) %% 360
    l_path <- l_range[1] + tt * diff(l_range)
    c_path <- mean(c_range) + (diff(c_range) / 2) * 0.4 * sin(pi * tt)
  }

  if (length(manual_family_hue)) {
    p <- 0L
    for (fid in ord_fam) {
      m <- length(member_order[[fid]])
      if (fid %in% names(manual_family_hue)) h_path[p + seq_len(m)] <- manual_family_hue[[fid]]
      p <- p + m
    }
  }
  path <- .hcl_hex(h_path, c_path, l_path)

  cluster_col <- stats::setNames(rep(NA_character_, N), ordered)
  family_col <- character(0)
  p <- 0L
  for (fid in ord_fam) {
    ms <- member_order[[fid]]
    m <- length(ms)
    if (m == 0) next
    idx <- p + seq_len(m)
    sp <- .spread_slots(m, cyclic = FALSE)
    for (j in seq_len(m)) cluster_col[ms[j]] <- path[idx[sp[j]]]
    family_col[fid] <- path[idx[ceiling(m / 2)]]
    p <- p + m
  }
  list(cluster = cluster_col, family = family_col)
}

# ---- contrast mode ------------------------------------------------------

# Candidate pool for greedy max-min selection, drawn with the color-theory
# constraints: low-discrepancy hues, lightness inside the resolved range,
# chroma a randomised fraction of the per-(h,l) gamut ceiling (so vivid but
# never neon / out-of-gamut).
.contrast_pool <- function(n_target, seed, lightness_range, chroma_range,
                            hue_range, pool_size = NULL) {
  if (is.null(pool_size)) pool_size <- max(2600L, 90L * n_target)
  set.seed(seed)
  start <- stats::runif(1, 0, 360)
  h <- .golden_hues(pool_size, start)
  h <- hue_range[1] + (h / 360) * diff(hue_range)
  l <- stats::runif(pool_size, lightness_range[1], lightness_range[2])
  frac <- stats::runif(pool_size, 0.45, 0.85)
  mc <- colorspace::max_chroma(h = h %% 360, l = pmin(pmax(l, 1), 99))
  c_ <- pmin(pmax(chroma_range[1], frac * mc), chroma_range[2])
  # thin out muddy yellow-green candidates (keep a few so coverage is still
  # possible if a palette genuinely needs that region)
  keep <- stats::runif(pool_size) > 0.78 * .hue_muddiness_penalty(h)
  h <- h[keep]
  l <- l[keep]
  c_ <- c_[keep]
  unique(colorspace::hex(colorspace::polarLUV(L = l, C = c_, H = h), fixup = TRUE))
}

#' Greedy max-min ("Glasbey-style") color selection
#'
#' Greedily picks, from a candidate pool, the color that maximizes the
#' minimum perceptual distance to colors already chosen (including any
#' fixed/manual colors passed in), repeated until `n` colors are chosen.
#'
#' @param n Number of new colors to choose.
#' @param seed Random seed for candidate pool generation.
#' @param lightness_range,chroma_range,hue_range Ranges the candidate pool
#'   is drawn from.
#' @param fixed_hex Colors already in use (manual overrides); selection
#'   avoids clashing with these but does not return them.
#' @param cvd `"none"` or a CVD type; distances are then computed on the
#'   CVD-simulated colors.
#' @return Character vector of `n` hex colors.
#' @keywords internal
.contrast_colors <- function(n, seed, lightness_range, chroma_range,
                              hue_range = c(0, 360), fixed_hex = character(0),
                              cvd = "none") {
  if (n <= 0) return(character(0))
  pool <- .contrast_pool(n, seed, lightness_range, chroma_range, hue_range)
  pool <- setdiff(pool, fixed_hex)

  pool_lab <- farver::decode_colour(.perceptual_hex(pool, cvd), to = "lab")
  chosen_lab <- if (length(fixed_hex) > 0) {
    farver::decode_colour(.perceptual_hex(fixed_hex, cvd), to = "lab")
  } else {
    matrix(numeric(0), ncol = 3)
  }

  min_dist <- if (nrow(chosen_lab) > 0) {
    apply(farver::compare_colour(pool_lab, chosen_lab, from_space = "lab", method = "CIE2000"), 1, min)
  } else {
    rep(Inf, nrow(pool_lab))
  }

  chosen <- character(0)
  for (i in seq_len(min(n, length(pool)))) {
    best <- which.max(min_dist)
    chosen <- c(chosen, pool[best])
    d_new <- as.numeric(farver::compare_colour(
      pool_lab, pool_lab[best, , drop = FALSE],
      from_space = "lab", method = "CIE2000"
    ))
    min_dist <- pmin(min_dist, d_new)
    min_dist[best] <- -Inf
  }
  if (length(chosen) < n) {
    chosen <- c(chosen, sample(pool, n - length(chosen), replace = TRUE))
  }
  chosen
}

# ---- main entry point ----------------------------------------------------

#' Generate a cluster/family color palette
#'
#' The core color-generation entry point. Given a cluster -> family
#' assignment, it places one base hue per family and then colors each
#' cluster, in one of two modes:
#'
#' * `mode = "harmonious"` -- a coordinated palette in HCL space.
#'   `harmonious_style = "sweep"` (default) lays families along a single
#'   analogous hue arc with a shared monotone lightness ramp and muted
#'   chroma (a smooth blue -> purple -> rose -> tan style sweep);
#'   `harmonious_style = "per_family"` gives each family its own hue and
#'   colors its members as graded shades of it.
#' * `mode = "contrast"` -- every cluster gets a maximally-distinguishable
#'   color via greedy max-min selection in Lab space; family membership is
#'   carried by `$families$color` as a secondary channel.
#'
#' Neighbor-awareness: when `neighbors` is supplied, families that are
#' adjacent in the embedding/graph get **contrasting** hues by default
#' (`neighbor_hues = "contrast"`, the default in contrast mode) or
#' **analogous** hues (`neighbor_hues = "coherent"`, the default in
#' harmonious mode); and within a family, spatially-adjacent clusters are
#' given far-apart lightness for local contrast.
#'
#' Passing a previous `session` makes this incremental: any cluster or
#' family with `manual_color == TRUE` keeps its color, the generator treats
#' those as fixed and avoids clashing with them, and with
#' `lightness_range`/`chroma_range` left at `"auto"` the non-manual colors
#' are regenerated inside the L/C envelope implied by the manual picks.
#'
#' @param assignment A data frame with columns `cluster`, `family_id`.
#' @param session An existing `palettome_session` to inherit manual
#'   overrides from, or `NULL`.
#' @param mode `"harmonious"` or `"contrast"`.
#' @param harmonious_style `"sweep"` or `"per_family"` (only used when
#'   `mode = "harmonious"`).
#' @param sweep_anchors Optional character vector of 2+ hex colors (only
#'   used when `mode = "harmonious"` and `harmonious_style = "sweep"`): a
#'   gradient to sweep the palette along, interpolated through in HCL space
#'   in the order given, instead of the automatically-placed analogous arc.
#'   Handy when the auto arc's seed-to-seed variety (see [generate_palette]
#'   examples) still isn't the specific look you want -- e.g. brand colors,
#'   or literally the colors from a reference gradient.
#' @param neighbor_hues `"contrast"`, `"coherent"`, or `NULL` to follow the
#'   mode (contrast -> `"contrast"`, harmonious -> `"coherent"`).
#' @param neighbors Optional neighbor information used to order families and
#'   clusters in space: a data frame of centroids (`cluster` + `*_centroid`
#'   or coordinate columns, e.g. [compute_centroids()]) or a square
#'   cluster-by-cluster connectivity/distance matrix. `NULL` disables
#'   neighbor-aware placement (falls back to even hue spacing).
#' @param seed Random seed.
#' @param lightness_range,chroma_range Length-2 numeric ranges in HCL space,
#'   or `"auto"` (default) to derive them from manual overrides when present
#'   and from mode/style presets otherwise.
#' @param hue_range Length-2 numeric range in degrees to draw hues from.
#' @param cvd `"none"`, `"deutan"`, `"protan"`, or `"tritan"`.
#' @param n_cells Optional named integer vector or `cluster`/`n_cells` data
#'   frame, stored on the session for convenience.
#' @return A `palettome_session` object (see the package README for the JSON
#'   schema).
#' @export
generate_palette <- function(assignment, session = NULL,
                              mode = c("harmonious", "contrast"),
                              harmonious_style = c("sweep", "per_family"),
                              sweep_anchors = NULL,
                              neighbor_hues = NULL,
                              neighbors = NULL,
                              seed = 1,
                              lightness_range = "auto",
                              chroma_range = "auto",
                              hue_range = c(0, 360),
                              cvd = c("none", "deutan", "protan", "tritan"),
                              n_cells = NULL) {
  mode <- match.arg(mode)
  harmonious_style <- match.arg(harmonious_style)
  cvd <- match.arg(cvd)
  if (is.null(neighbor_hues)) {
    neighbor_hues <- if (mode == "contrast") "contrast" else "coherent"
  }
  neighbor_hues <- match.arg(neighbor_hues, c("contrast", "coherent"))
  stopifnot(
    "`assignment` must have `cluster` and `family_id` columns" =
      all(c("cluster", "family_id") %in% names(assignment)),
    "`sweep_anchors` must have 2 or more hex colors, or be NULL" =
      is.null(sweep_anchors) || length(sweep_anchors) >= 2
  )
  assignment <- unique(assignment[, c("cluster", "family_id")])
  assignment$cluster <- as.character(assignment$cluster)
  assignment$family_id <- as.character(assignment$family_id)
  family_ids <- unique(assignment$family_id)
  clusters <- assignment$cluster

  prev_cluster <- if (!is.null(session)) session$clusters else NULL
  prev_family <- if (!is.null(session)) session$families else NULL
  manual_cluster_colors <- .lookup_manual(prev_cluster, "cluster")
  manual_family_colors <- .lookup_manual(prev_family, "family_id")
  manual_hex_all <- unlist(c(manual_cluster_colors, manual_family_colors), use.names = FALSE)

  n_items <- if (mode == "contrast") length(clusters) else length(family_ids)
  l_range <- .resolve_range(lightness_range, "L", manual_hex_all, mode, harmonious_style, n_items)
  c_range <- .resolve_range(chroma_range, "C", manual_hex_all, mode, harmonious_style, n_items)

  # ---- neighbor structure ------------------------------------------
  cdist <- .cluster_distance(neighbors, clusters)
  if (!is.null(cdist)) {
    fdist <- .family_distance(cdist, assignment)
    forder <- .seriate_order(fdist)
    fadj <- .knn_adjacency(fdist)
  } else {
    forder <- seq_along(family_ids)
    fadj <- NULL
  }

  # spatial order of families and, within each family, of its clusters
  ord_fam <- family_ids[forder]
  member_order <- stats::setNames(vector("list", length(family_ids)), family_ids)
  for (fid in family_ids) {
    members <- clusters[assignment$family_id == fid]
    co <- if (!is.null(cdist) && length(members) > 1) {
      .seriate_order(cdist[members, members, drop = FALSE])
    } else {
      seq_along(members)
    }
    member_order[[fid]] <- members[co]
  }

  # ---- family hues (per_family / contrast secondary channel) ------
  manual_family_hue <- if (length(manual_family_colors)) {
    stats::setNames(.to_hcl(unlist(manual_family_colors))[, "h"], names(manual_family_colors))
  } else {
    NULL
  }
  fam_hue <- .assign_family_hues(
    family_ids, forder, fadj, seed, neighbor_hues, hue_range,
    harmonious_style, manual_family_hue
  )

  family_color <- character(0)
  for (fid in family_ids) {
    family_color[fid] <- if (fid %in% names(manual_family_colors)) {
      manual_family_colors[[fid]]
    } else {
      .hcl_hex(fam_hue[[fid]], mean(c_range), mean(l_range))
    }
  }

  # ---- per-cluster colors --------------------------------------------
  cluster_color <- stats::setNames(rep(NA_character_, length(clusters)), clusters)
  for (cl in names(manual_cluster_colors)) {
    if (cl %in% names(cluster_color)) cluster_color[cl] <- manual_cluster_colors[[cl]]
  }

  if (mode == "harmonious" && harmonious_style == "sweep") {
    sweep <- .harmonious_sweep(
      ord_fam, member_order, l_range, c_range, hue_range, seed, manual_family_hue,
      anchors = sweep_anchors
    )
    for (cl in names(sweep$cluster)) {
      if (is.na(cluster_color[cl])) cluster_color[cl] <- sweep$cluster[[cl]]
    }
    for (fid in names(sweep$family)) {
      if (!fid %in% names(manual_family_colors)) family_color[fid] <- sweep$family[[fid]]
    }
  } else if (mode == "harmonious") {
    for (fid in family_ids) {
      members <- member_order[[fid]]
      todo <- members[is.na(cluster_color[members])]
      if (!length(todo)) next
      cols <- .family_member_colors(
        members, seq_along(members), fam_hue[[fid]], l_range, c_range, "per_family"
      )
      cluster_color[todo] <- cols[todo]
    }
  } else {
    fixed_hex <- unname(cluster_color[!is.na(cluster_color)])
    todo <- names(cluster_color)[is.na(cluster_color)]
    if (length(todo)) {
      cluster_color[todo] <- .contrast_colors(
        length(todo), seed, l_range, c_range, hue_range,
        fixed_hex = fixed_hex, cvd = cvd
      )
    }
  }

  clusters_df <- data.frame(
    cluster = clusters,
    family_id = assignment$family_id,
    color = unname(cluster_color[clusters]),
    manual_color = clusters %in% names(manual_cluster_colors),
    stringsAsFactors = FALSE
  )
  clusters_df$n_cells <- .lookup_n_cells(n_cells, clusters_df$cluster, prev_cluster)

  families_df <- data.frame(
    family_id = family_ids,
    color = unname(family_color[family_ids]),
    manual_color = family_ids %in% names(manual_family_colors),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      schema_version = "1.0",
      clusters = clusters_df,
      families = families_df,
      params = list(
        mode = mode,
        harmonious_style = harmonious_style,
        sweep_anchors = sweep_anchors,
        neighbor_hues = neighbor_hues,
        seed = seed,
        lightness_range = l_range,
        chroma_range = c_range,
        hue_range = hue_range,
        cvd = cvd,
        auto_lightness = identical(lightness_range, "auto"),
        auto_chroma = identical(chroma_range, "auto")
      )
    ),
    class = "palettome_session"
  )
}

.lookup_manual <- function(df, id_col) {
  if (is.null(df) || !"manual_color" %in% names(df)) return(stats::setNames(list(), character(0)))
  keep <- df[df$manual_color %in% TRUE, , drop = FALSE]
  stats::setNames(as.list(keep$color), keep[[id_col]])
}

.lookup_n_cells <- function(n_cells, clusters, prev_cluster) {
  out <- stats::setNames(rep(NA_integer_, length(clusters)), clusters)
  if (!is.null(prev_cluster) && "n_cells" %in% names(prev_cluster)) {
    m <- stats::setNames(prev_cluster$n_cells, prev_cluster$cluster)
    common <- intersect(clusters, names(m))
    out[common] <- m[common]
  }
  if (!is.null(n_cells)) {
    m <- if (is.data.frame(n_cells)) stats::setNames(n_cells$n_cells, n_cells$cluster) else n_cells
    common <- intersect(clusters, names(m))
    out[common] <- m[common]
  }
  unname(out)
}

#' Snap a hand-picked color into the palette's aesthetic envelope
#'
#' Feature companion to manual recoloring: keeps the hue you chose but pulls
#' its lightness and chroma to something in-gamut and coherent -- either
#' matched to the lightness/chroma distribution of an existing `session`
#' (so a hand-picked color sits with the rest), or, with no session, toward
#' the natural lightness for that hue and a pleasing (non-neon) chroma.
#'
#' @param color A hex color string.
#' @param session Optional `palettome_session` whose current colors define
#'   the target L/C register.
#' @param target `"auto"` (keep near the input chroma), `"muted"`, or
#'   `"vivid"`.
#' @return A hex color string with the same hue, adjusted L/C.
#' @export
optimize_color <- function(color, session = NULL, target = c("auto", "muted", "vivid")) {
  target <- match.arg(target)
  hcl <- .to_hcl(color)
  h <- hcl[1, "h"]
  c0 <- hcl[1, "c"]
  l0 <- hcl[1, "l"]

  if (!is.null(session) && nrow(session$clusters) > 0) {
    o <- .to_hcl(session$clusters$color)
    l <- 0.5 * l0 + 0.5 * stats::median(o[, "l"])
    c_t <- 0.5 * c0 + 0.5 * stats::median(o[, "c"])
  } else {
    l <- 0.55 * l0 + 0.45 * .natural_lightness(h)
    c_t <- switch(target,
      muted = 26,
      vivid = .pleasing_chroma(h, l, frac = 0.9),
      auto = c0
    )
  }
  unname(.hcl_hex(h, c_t, l, chroma_frac = 0.9))
}

#' Manually set a cluster's or family's color
#'
#' Sets `color` and flags `manual_color = TRUE` for the given cluster and/or
#' family in a session. Manually-colored entries are left untouched by later
#' [generate_palette()] calls (until reset with [reset_overrides()]).
#'
#' @param session A `palettome_session`.
#' @param cluster,family Cluster id / family id to recolor (either or both;
#'   at least one required).
#' @param color A hex color string.
#' @return The updated `palettome_session`.
#' @export
set_manual_color <- function(session, cluster = NULL, family = NULL, color) {
  stopifnot(inherits(session, "palettome_session"), !is.null(cluster) || !is.null(family))
  if (!is.null(cluster)) {
    i <- match(cluster, session$clusters$cluster)
    if (is.na(i)) stop("Unknown cluster: ", cluster, call. = FALSE)
    session$clusters$color[i] <- color
    session$clusters$manual_color[i] <- TRUE
  }
  if (!is.null(family)) {
    i <- match(family, session$families$family_id)
    if (is.na(i)) stop("Unknown family: ", family, call. = FALSE)
    session$families$color[i] <- color
    session$families$manual_color[i] <- TRUE
  }
  session
}

#' Reset manual color overrides
#'
#' @param session A `palettome_session`.
#' @param cluster,family Character vector of ids to reset, or `TRUE` to
#'   reset all clusters/families respectively. Defaults to resetting
#'   everything.
#' @return The updated `palettome_session` (colors are left as-is; only the
#'   `manual_color` flags are cleared -- call [generate_palette()] again to
#'   recompute the now-unlocked colors).
#' @export
reset_overrides <- function(session, cluster = TRUE, family = TRUE) {
  stopifnot(inherits(session, "palettome_session"))
  if (isTRUE(cluster)) {
    session$clusters$manual_color <- FALSE
  } else if (is.character(cluster)) {
    session$clusters$manual_color[session$clusters$cluster %in% cluster] <- FALSE
  }
  if (isTRUE(family)) {
    session$families$manual_color <- FALSE
  } else if (is.character(family)) {
    session$families$manual_color[session$families$family_id %in% family] <- FALSE
  }
  session
}
