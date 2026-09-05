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

# ---- family base hues -------------------------------------------------

#' Pick maximally-separated base hues for families
#'
#' Evenly spaces hues around the wheel with a seeded random starting offset,
#' then (when `cvd != "none"`) checks the minimum pairwise perceptual
#' distance *as it would appear under that color-vision deficiency* and
#' retries a few alternate offsets if hues that look distinct to full-color
#' vision would collapse together under simulation.
#'
#' @param n Number of families.
#' @param seed Random seed controlling the starting hue offset.
#' @param hue_range Two-element numeric range (degrees) to spread hues over;
#'   defaults to the full wheel.
#' @param lightness,chroma L/C used only to render a representative swatch
#'   for the CVD-distance check; does not affect the returned hues.
#' @param cvd `"none"` or a CVD type understood by [simulate_cvd()].
#' @param min_dist Minimum acceptable pairwise CIEDE2000 distance between
#'   representative swatches under simulation.
#' @param max_tries Number of alternate offsets to try before giving up and
#'   returning the best one found.
#' @return Numeric vector of `n` hues in degrees `[0, 360)`.
#' @keywords internal
.pick_family_hues <- function(n, seed, hue_range = c(0, 360),
                               lightness = 60, chroma = 55,
                               cvd = "none", min_dist = 15, max_tries = 20) {
  if (n <= 0) return(numeric(0))
  if (n == 1) {
    set.seed(seed)
    return(stats::runif(1, hue_range[1], hue_range[2]))
  }

  span <- diff(hue_range)
  best_hues <- NULL
  best_score <- -Inf
  tries <- if (identical(cvd, "none")) 1L else max_tries
  for (i in seq_len(tries)) {
    set.seed(seed + i - 1L)
    offset <- stats::runif(1, 0, span)
    hues <- (hue_range[1] + offset + seq(0, span, length.out = n + 1)[seq_len(n)]) %% 360
    if (identical(cvd, "none")) {
      best_hues <- hues
      break
    }
    swatch <- colorspace::hex(colorspace::polarLUV(L = lightness, C = chroma, H = hues), fixup = TRUE)
    sim <- .perceptual_hex(swatch, cvd)
    d <- color_distance(sim)
    diag(d) <- NA
    score <- min(d, na.rm = TRUE)
    if (score > best_score) {
      best_score <- score
      best_hues <- hues
    }
    if (score >= min_dist) break
  }
  best_hues
}

# ---- harmonious mode --------------------------------------------------

#' Shades for one family at a fixed hue
#'
#' Interpolates jointly through lightness and chroma (dark/muted ->
#' light/vivid) at a single hue, giving `m` perceptually graded shades of the
#' same base color ("tone" variation), which is what harmonious mode uses to
#' distinguish family members.
#'
#' @param m Number of members.
#' @param hue Fixed hue in degrees.
#' @param lightness_range,chroma_range Two-element numeric ranges.
#' @return Character vector of `m` hex colors.
#' @keywords internal
.family_shades <- function(m, hue, lightness_range, chroma_range) {
  if (m <= 0) return(character(0))
  if (m == 1) {
    L <- mean(lightness_range)
    C <- mean(chroma_range)
  } else {
    L <- seq(lightness_range[1], lightness_range[2], length.out = m)
    C <- seq(chroma_range[1], chroma_range[2], length.out = m)
  }
  colorspace::hex(colorspace::polarLUV(L = L, C = C, H = hue), fixup = TRUE)
}

# ---- contrast mode ------------------------------------------------------

#' Greedy max-min ("Glasbey-style") color selection
#'
#' Builds a large candidate pool of HCL colors and greedily picks the
#' candidate that maximizes the minimum perceptual distance to colors
#' already chosen (including any fixed/manual colors passed in as a
#' starting set), repeated until `n` colors are chosen. This is the standard
#' approach for maximally-distinguishable qualitative palettes in a
#' perceptually uniform space.
#'
#' @param n Number of new colors to choose.
#' @param seed Random seed for candidate pool generation.
#' @param lightness_range,chroma_range,hue_range Ranges to draw candidates
#'   from.
#' @param fixed_hex Character vector of colors already in use (e.g. manual
#'   overrides); the selection avoids clashing with these but does not
#'   return them.
#' @param cvd `"none"` or a CVD type; when set, distances are computed on
#'   the CVD-simulated candidates so the chosen palette stays distinguishable
#'   under that deficiency.
#' @param pool_size Size of the random candidate pool to search.
#' @return Character vector of `n` hex colors.
#' @keywords internal
.contrast_colors <- function(n, seed, lightness_range, chroma_range,
                              hue_range = c(0, 360), fixed_hex = character(0),
                              cvd = "none", pool_size = 2000) {
  if (n <= 0) return(character(0))
  set.seed(seed)
  h <- stats::runif(pool_size, hue_range[1], hue_range[2])
  l <- stats::runif(pool_size, lightness_range[1], lightness_range[2])
  c_ <- stats::runif(pool_size, chroma_range[1], chroma_range[2])
  pool <- unique(colorspace::hex(colorspace::polarLUV(L = l, C = c_, H = h), fixup = TRUE))
  pool <- setdiff(pool, fixed_hex)

  pool_space <- .perceptual_hex(pool, cvd)
  pool_lab <- farver::decode_colour(pool_space, to = "lab")

  chosen <- character(0)
  chosen_lab <- if (length(fixed_hex) > 0) {
    farver::decode_colour(.perceptual_hex(fixed_hex, cvd), to = "lab")
  } else {
    matrix(numeric(0), ncol = 3, dimnames = list(NULL, c("l", "a", "b")))
  }

  min_dist_to_chosen <- if (nrow(chosen_lab) > 0) {
    apply(farver::compare_colour(pool_lab, chosen_lab, from_space = "lab", method = "CIE2000"), 1, min)
  } else {
    rep(Inf, nrow(pool_lab))
  }

  for (i in seq_len(min(n, length(pool)))) {
    best <- which.max(min_dist_to_chosen)
    chosen <- c(chosen, pool[best])
    new_lab <- pool_lab[best, , drop = FALSE]
    d_new <- as.numeric(farver::compare_colour(pool_lab, new_lab, from_space = "lab", method = "CIE2000"))
    min_dist_to_chosen <- pmin(min_dist_to_chosen, d_new)
    min_dist_to_chosen[best] <- -Inf # never repick
  }
  if (length(chosen) < n) {
    # pool exhausted (extreme n or narrow ranges): recycle with jitter
    extra <- n - length(chosen)
    chosen <- c(chosen, sample(pool, extra, replace = TRUE))
  }
  chosen
}

# ---- main entry point ----------------------------------------------------

#' Generate a cluster/family color palette
#'
#' The core color-generation entry point. Given a cluster -> family
#' assignment, computes one base hue per family and either shades within
#' that hue (`mode = "harmonious"`) or a maximally-distinguishable color per
#' cluster (`mode = "contrast"`), with family membership always carried in
#' `$families$color` as a secondary channel (a header/tag/outline color).
#'
#' Passing a previous `session` makes this incremental: any cluster or
#' family with `manual_color == TRUE` keeps its stored color untouched, and
#' the algorithm treats those fixed colors as already "used" so newly
#' generated colors avoid clashing with them. This is what lets a UI call
#' `generate_palette()` again and again (new mode, new seed, edited
#' families) without ever clobbering a user's manual picks.
#'
#' @param assignment A data frame with columns `cluster`, `family_id` (e.g.
#'   `detect_families(pdata)$assignment`, possibly hand-edited).
#' @param session An existing `palettome_session` (from a previous call) to
#'   preserve manual overrides from, or `NULL` for a fresh palette.
#' @param mode `"harmonious"` or `"contrast"`.
#' @param seed Random seed (family hue order / contrast candidate pool).
#' @param lightness_range,chroma_range Two-element numeric ranges in HCL
#'   space (roughly `[0, 100]`) controlling generated shade/tint/tone.
#' @param hue_range Two-element numeric range in degrees to draw hues from.
#' @param cvd `"none"`, or `"deutan"`/`"protan"`/`"tritan"` to bias
#'   generation towards staying distinguishable under that color-vision
#'   deficiency.
#' @param n_cells Optional named integer vector or data frame with
#'   `cluster`/`n_cells` columns, stored on the session for convenience
#'   (e.g. sizing legend swatches) but not used in color choice.
#' @return A `palettome_session` object (see the package README for the
#'   full JSON schema): a list with `schema_version`, `clusters` (data frame:
#'   `cluster`, `family_id`, `color`, `manual_color`, `n_cells`), `families`
#'   (data frame: `family_id`, `color`, `manual_color`), and `params`.
#' @export
generate_palette <- function(assignment, session = NULL,
                              mode = c("harmonious", "contrast"),
                              seed = 1,
                              lightness_range = c(35, 85),
                              chroma_range = c(35, 90),
                              hue_range = c(0, 360),
                              cvd = c("none", "deutan", "protan", "tritan"),
                              n_cells = NULL) {
  mode <- match.arg(mode)
  cvd <- match.arg(cvd)
  stopifnot(
    "`assignment` must have `cluster` and `family_id` columns" =
      all(c("cluster", "family_id") %in% names(assignment))
  )
  assignment <- unique(assignment[, c("cluster", "family_id")])
  family_ids <- unique(assignment$family_id)

  prev_cluster <- if (!is.null(session)) session$clusters else NULL
  prev_family <- if (!is.null(session)) session$families else NULL

  manual_cluster_colors <- .lookup_manual(prev_cluster, "cluster")
  manual_family_colors <- .lookup_manual(prev_family, "family_id")

  # ---- family base hues (skip families that are fully manually colored) --
  fam_needs_hue <- setdiff(family_ids, names(manual_family_colors))
  hues <- .pick_family_hues(
    length(fam_needs_hue), seed = seed, hue_range = hue_range,
    lightness = mean(lightness_range), chroma = mean(chroma_range), cvd = cvd
  )
  fam_hue <- stats::setNames(as.list(hues), fam_needs_hue)

  family_color <- character(0)
  for (fid in family_ids) {
    if (fid %in% names(manual_family_colors)) {
      family_color[fid] <- manual_family_colors[[fid]]
    } else {
      swatch <- colorspace::hex(
        colorspace::polarLUV(L = mean(lightness_range), C = mean(chroma_range), H = fam_hue[[fid]]),
        fixup = TRUE
      )
      family_color[fid] <- swatch
    }
  }

  # ---- per-cluster colors --------------------------------------------
  cluster_color <- stats::setNames(rep(NA_character_, nrow(assignment)), assignment$cluster)
  for (cl in names(manual_cluster_colors)) {
    if (cl %in% names(cluster_color)) cluster_color[cl] <- manual_cluster_colors[[cl]]
  }

  if (mode == "harmonious") {
    for (fid in family_ids) {
      members <- assignment$cluster[assignment$family_id == fid]
      todo <- members[is.na(cluster_color[members])]
      if (length(todo) == 0) next
      hue <- if (fid %in% names(fam_hue)) {
        fam_hue[[fid]]
      } else {
        as.numeric(colorspace::coords(methods::as(colorspace::hex2RGB(family_color[[fid]]), "polarLUV"))[, "H"])
      }
      shades <- .family_shades(length(todo), hue, lightness_range, chroma_range)
      cluster_color[todo] <- shades
    }
  } else {
    fixed_hex <- unname(cluster_color[!is.na(cluster_color)])
    todo <- names(cluster_color)[is.na(cluster_color)]
    if (length(todo) > 0) {
      new_colors <- .contrast_colors(
        length(todo), seed = seed, lightness_range = lightness_range,
        chroma_range = chroma_range, hue_range = hue_range,
        fixed_hex = fixed_hex, cvd = cvd
      )
      cluster_color[todo] <- new_colors
    }
  }

  clusters_df <- data.frame(
    cluster = assignment$cluster,
    family_id = assignment$family_id,
    color = unname(cluster_color[assignment$cluster]),
    manual_color = assignment$cluster %in% names(manual_cluster_colors),
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
        mode = mode, seed = seed,
        lightness_range = lightness_range, chroma_range = chroma_range,
        hue_range = hue_range, cvd = cvd
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
    if (is.data.frame(n_cells)) {
      m <- stats::setNames(n_cells$n_cells, n_cells$cluster)
    } else {
      m <- n_cells
    }
    common <- intersect(clusters, names(m))
    out[common] <- m[common]
  }
  unname(out)
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
#'   `manual_color` flags are cleared — call [generate_palette()] again to
#'   actually recompute the now-unlocked colors).
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
