# internal helpers
ct <- function(name) get(name, envir = asNamespace("palettome"))

test_that(".hcl_hex stays in gamut (no fixup clipping needed)", {
  hcl_hex <- ct(".hcl_hex")
  hex <- hcl_hex(H = seq(0, 350, by = 10), C = 200, L = 55) # ask for impossible chroma
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", hex)))
  # round-tripping should not move the hue much (i.e. it was a real color)
  back <- farver::decode_colour(hex, to = "hcl")
  ask <- seq(0, 350, by = 10)
  d <- pmin(abs(back[, 1] - ask), 360 - abs(back[, 1] - ask))
  expect_true(all(d < 12))
})

test_that(".natural_lightness is light for yellow/green and dark for blue", {
  nl <- ct(".natural_lightness")
  expect_gt(nl(85), nl(266)) # yellow brighter than blue
  expect_gt(nl(128), nl(290)) # green brighter than violet
  expect_true(all(nl(seq(0, 360, by = 15)) > 30 & nl(seq(0, 360, by = 15)) < 100))
})

test_that(".hue_muddiness_penalty peaks in the yellow-green zone", {
  pen <- ct(".hue_muddiness_penalty")
  expect_gt(pen(88), pen(0))
  expect_gt(pen(88), pen(266))
  expect_lt(pen(266), 0.2)
})

test_that(".golden_hues spreads well for any n", {
  gh <- ct(".golden_hues")
  for (n in c(3, 5, 8, 12)) {
    h <- sort(gh(n, start = 40))
    gaps <- c(diff(h), 360 - max(h) + min(h))
    expect_gt(min(gaps), (360 / n) * 0.45)
  }
})

test_that(".spread_slots is a permutation that separates consecutive inputs", {
  ss <- ct(".spread_slots")
  expect_setequal(ss(3), 1:3)
  for (n in 4:10) {
    s <- ss(n)
    expect_setequal(s, seq_len(n))
    expect_gte(min(abs(diff(s))), 2)
  }
  # cyclic variant wraps
  sc <- ss(6, cyclic = TRUE)
  expect_setequal(sc, seq_len(6))
})

test_that(".best_arc_start avoids the muddy zone", {
  bas <- ct(".best_arc_start")
  pen <- ct(".hue_muddiness_penalty")
  for (seed in 1:8) {
    s <- bas(arc = 120, hue_range = c(0, 360), seed = seed)
    arc_hues <- (s + seq(0, 120, length.out = 20)) %% 360
    expect_lt(mean(pen(arc_hues)), 0.35)
  }
})

test_that(".best_arc_start actually varies across seeds (not just a small jitter)", {
  bas <- ct(".best_arc_start")
  starts <- vapply(1:30, function(s) bas(arc = 130, seed = s), numeric(1))
  # a real spread of starting hues, not everything clustered in one 30-40deg window
  expect_gt(diff(range(starts)), 90)
  expect_gt(length(unique(round(starts / 10))), 5)
})
