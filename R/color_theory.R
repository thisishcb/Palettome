# Color-theory primitives shared by the palette generator. These encode the
# "make it look good even for a random seed" rules: stay inside the
# displayable gamut and away from neon/muddy zones, give each hue its
# natural lightness, spread hues with a low-discrepancy sequence, and place
# spatially-adjacent items on far-apart slots.
#
# All hue/chroma/lightness work here is in polar CIELUV (HCL), matching
# colorspace::polarLUV() and colorspace::max_chroma(). Perceptual *distance*
# is done in CIE Lab via farver (see color_distance()).

# ---- circular hue geometry ---------------------------------------------

# NULL-coalescing helper (internal, undocumented)
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Circular distance between hues, in degrees (0-180)
#' @keywords internal
.circ_dist <- function(a, b) {
  d <- abs((a - b) %% 360)
  pmin(d, 360 - d)
}

.gcd <- function(a, b) {
  a <- as.integer(a)
  b <- as.integer(b)
  while (b != 0L) {
    t <- b
    b <- a %% b
    a <- t
  }
  abs(a)
}

# ---- natural lightness / gamut-aware chroma ----------------------------

#' Natural perceived lightness of a fully-saturated hue
#'
#' Yellows and greens read light; blues and violets read dark. A multi-hue
#' palette that lets lightness drift with hue in roughly this way looks more
#' natural than one forced to constant lightness. Anchor points are
#' approximate perceived lightnesses of vivid hues around the CIELUV wheel,
#' linearly interpolated.
#' @keywords internal
.natural_lightness <- function(h) {
  ah <- c(0, 40, 85, 130, 175, 210, 266, 290, 320, 360)
  aL <- c(52, 66, 92, 82, 76, 60, 40, 43, 52, 52)
  stats::approx(ah, aL, xout = h %% 360, rule = 2)$y
}

#' How "muddy" a hue tends to look at mid/low lightness (0-1)
#'
#' The yellow / yellow-green arc (HCL hue ~60-115) turns olive and sickly
#' when it isn't near-white and is hard to tell apart from its neighbors; a
#' penalty there steers hue placement away from it when there is a free
#' choice. Orange and pure green are only lightly touched.
#' @keywords internal
.hue_muddiness_penalty <- function(h) {
  exp(-0.5 * (.circ_dist(h, 88) / 26)^2)
}

#' Placement of an analogous hue arc that avoids the muddy zone
#'
#' Probes arc start positions across `hue_range`, scores each by the *worst*
#' (not average) muddiness anywhere along the arc it would span -- a mean
#' score can hide a single genuinely olive stretch, which is exactly the
#' part of the sweep that tends to land at low lightness where it's most
#' visible -- then draws the returned start uniformly at random (by seed)
#' from every position that clears the tolerance, not just the single best
#' one. This is what keeps the harmonious "sweep" out of olive while still
#' giving different seeds genuinely different gradients (reds, teals,
#' greens, blues, purples, ...), rather than every seed converging on a
#' small jitter around one "optimal" blue-to-pink arc.
#'
#' @param arc Arc width in degrees.
#' @param hue_range Two-element range the start may fall in.
#' @param seed Seed controlling which acceptable start is picked, and the
#'   jitter applied to it.
#' @param n_probe Number of start positions to test.
#' @param tolerance Worst-point muddiness (0-1) a start may have and still
#'   be considered acceptable; widened automatically if nothing clears it.
#' @param jitter Max +/- continuous jitter (degrees) added after picking a
#'   probe, so results aren't quantized to the probe grid.
#' @return A single hue in degrees.
#' @keywords internal
.best_arc_start <- function(arc, hue_range = c(0, 360), seed = 1,
                             n_probe = 72, tolerance = 0.25, jitter = 2.5) {
  span <- diff(hue_range)
  step <- span / n_probe
  probes <- hue_range[1] + seq(0, span, length.out = n_probe + 1)[seq_len(n_probe)]
  mud <- vapply(probes, function(s) {
    max(.hue_muddiness_penalty((s + seq(0, arc, length.out = 24)) %% 360))
  }, numeric(1))

  tol <- tolerance
  ok <- probes[mud <= tol]
  while (length(ok) == 0 && tol < 1) {
    tol <- tol + 0.1
    ok <- probes[mud <= tol]
  }
  if (length(ok) == 0) ok <- probes[which.min(mud)]

  set.seed(seed + 991L)
  pick <- sample(ok, 1)
  (pick + stats::runif(1, -jitter, jitter)) %% 360
}

#' Build hex from HCL, clamping chroma into the displayable gamut
#'
#' `chroma_frac` keeps a margin below the true gamut boundary so colors
#' don't sit right on the edge (which fixup would otherwise distort).
#' Vectorised; H/C/L are recycled to a common length.
#' @keywords internal
.hcl_hex <- function(H, C, L, chroma_frac = 0.95) {
  n <- max(length(H), length(C), length(L))
  H <- rep_len(H, n) %% 360
  C <- rep_len(C, n)
  L <- rep_len(L, n)
  mc <- colorspace::max_chroma(h = H, l = pmin(pmax(L, 1), 99))
  C <- pmax(0, pmin(C, mc * chroma_frac))
  colorspace::hex(colorspace::polarLUV(L = L, C = C, H = H), fixup = TRUE)
}

#' Largest "pleasing" chroma for a hue at a lightness (gamut fraction, capped)
#' @keywords internal
.pleasing_chroma <- function(h, l, frac = 0.72, cap = 95) {
  pmin(cap, frac * colorspace::max_chroma(h = h %% 360, l = pmin(pmax(l, 1), 99)))
}

#' Decode hex colors to an HCL matrix (columns h, c, l)
#' @keywords internal
.to_hcl <- function(hex) {
  m <- farver::decode_colour(hex, to = "hcl")
  colnames(m) <- c("h", "c", "l")
  m
}

# ---- hue sequences ----------------------------------------------------

#' Low-discrepancy hue sequence (golden-angle additive recurrence)
#'
#' Gives a well-spread set of hues for *any* n, so a "random" starting
#' offset still produces a usable spread rather than occasional clumps.
#' @keywords internal
.golden_hues <- function(n, start = 0) {
  if (n <= 0) return(numeric(0))
  (start + (seq_len(n) - 1) * 137.50776405) %% 360
}

# ---- adjacency-aware slot assignment --------------------------------

#' Permutation of 1:n that puts consecutive inputs on far-apart slots
#'
#' Used so that items in spatial order (adjacent clusters, adjacent
#' families) land on non-adjacent lightness levels or hue positions. For
#' small n an exact max-min-gap permutation is found by brute force; for
#' larger n a half-shift interleave (min gap approx n/2) is used.
#'
#' @param n Number of slots/items.
#' @param cyclic If `TRUE`, slots wrap (hue wheel): uses a coprime stride so
#'   consecutive items jump ~0.4 n around the circle.
#' @return Integer vector `s` of length `n`; item in position `j` (input
#'   order) is assigned slot `s[j]`.
#' @keywords internal
.spread_slots <- function(n, cyclic = FALSE) {
  n <- as.integer(n)
  if (n <= 2L) return(seq_len(n))

  if (cyclic) {
    step <- max(1L, as.integer(round(n * 0.4)))
    while (.gcd(step, n) != 1L && step < n) step <- step + 1L
    if (.gcd(step, n) != 1L) step <- 1L
    return(((seq_len(n) - 1L) * step) %% n + 1L)
  }

  if (n <= 8L) {
    perms <- .perms(n)
    gaps <- apply(perms, 1L, function(p) min(abs(diff(p))))
    return(as.integer(perms[which.max(gaps), ]))
  }

  half <- as.integer(ceiling(n / 2))
  lows <- seq_len(half)
  highs <- seq.int(half + 1L, n)
  out <- integer(n)
  for (i in seq_len(n)) {
    out[i] <- if (i %% 2L == 1L) lows[(i + 1L) %/% 2L] else highs[i %/% 2L]
  }
  out
}

# all permutations of 1:n as a matrix, one per row (small n only)
.perms <- function(n) {
  if (n == 1L) return(matrix(1L, 1L, 1L))
  sub <- .perms(n - 1L)
  do.call(rbind, lapply(seq_len(n), function(k) {
    cbind(k, matrix(ifelse(sub < k, sub, sub + 1L), nrow = nrow(sub)))
  }))
}
