test_assignment <- function() {
  data.frame(cluster = paste0("c", 1:12), family_id = rep(paste0("F", 1:4), each = 3))
}

# 4 families placed at the corners of a square, 3 clusters each, jittered.
test_neighbors <- function() {
  set.seed(1)
  fam <- rep(1:4, each = 3)
  fx <- c(0, 20, 0, 20)[fam]
  fy <- c(0, 0, 20, 20)[fam]
  data.frame(
    cluster = paste0("c", 1:12),
    x_centroid = fx + stats::rnorm(12, 0, 1),
    y_centroid = fy + stats::rnorm(12, 0, 1)
  )
}

cdeg <- function(a, b) {
  d <- abs((a - b) %% 360)
  pmin(d, 360 - d)
}
hcl_of <- function(hex) farver::decode_colour(hex, to = "hcl")

test_that("contrast mode keeps a minimum perceptual distance between all clusters", {
  assignment <- test_assignment()
  for (seed in 1:5) {
    sess <- generate_palette(assignment, mode = "contrast", seed = seed)
    d <- color_distance(sess$clusters$color)
    diag(d) <- NA
    expect_gt(min(d, na.rm = TRUE), 12)
  }
})

test_that("contrast mode colors stay inside the resolved ranges and out of the muddy zone", {
  sess <- generate_palette(test_assignment(), mode = "contrast", seed = 2)
  hcl <- hcl_of(sess$clusters$color)
  lr <- sess$params$lightness_range
  expect_true(all(hcl[, "l"] >= lr[1] - 2 & hcl[, "l"] <= lr[2] + 2))
  # at most one cluster may land in the olive/chartreuse band
  expect_lte(sum(cdeg(hcl[, "h"], 88) < 20), 1)
})

test_that("harmonious sweep is a monotone-lightness analogous sweep", {
  sess <- generate_palette(
    test_assignment(),
    mode = "harmonious", harmonious_style = "sweep",
    neighbors = test_neighbors(), seed = 3
  )
  fh <- hcl_of(sess$families$color)
  o <- order(fh[, "l"])
  # consecutive families along the lightness ramp are analogous in hue
  hstep <- cdeg(fh[o, "h"][-1], fh[o, "h"][-nrow(fh)])
  expect_true(all(hstep < 80))
  # and there is a real lightness ramp across the palette
  cl_l <- hcl_of(sess$clusters$color)[, "l"]
  expect_gt(diff(range(cl_l)), 24)
  # sweep never wanders into the muddy zone
  expect_true(all(cdeg(hcl_of(sess$clusters$color)[, "h"], 88) > 12))
})

test_that("per_family harmonious gives each family one hue with a lightness spread", {
  sess <- generate_palette(
    test_assignment(),
    mode = "harmonious", harmonious_style = "per_family",
    neighbor_hues = "contrast", neighbors = test_neighbors(), seed = 1
  )
  for (f in unique(sess$clusters$family_id)) {
    hcl <- hcl_of(sess$clusters$color[sess$clusters$family_id == f])
    expect_lt(diff(range(hcl[, "h"])), 30) # ~one hue
    expect_gt(diff(range(hcl[, "l"])), 12) # spread in lightness
  }
  # families themselves are well separated in hue
  fh <- hcl_of(sess$families$color)[, "h"]
  gaps <- outer(fh, fh, cdeg)
  diag(gaps) <- Inf
  expect_gt(min(gaps), 25)
})

test_that("neighbor_hues switches adjacent families between analogous and contrasting", {
  a2 <- data.frame(cluster = c("a", "b", "c", "d"), family_id = c("F1", "F1", "F2", "F2"))
  nb2 <- data.frame(cluster = c("a", "b", "c", "d"), x_centroid = c(0, 1, 3, 4), y_centroid = 0)

  coherent <- generate_palette(a2, mode = "harmonious", harmonious_style = "per_family",
    neighbor_hues = "coherent", neighbors = nb2, seed = 1)
  contrast <- generate_palette(a2, mode = "harmonious", harmonious_style = "per_family",
    neighbor_hues = "contrast", neighbors = nb2, seed = 1)

  gap <- function(s) cdeg(hcl_of(s$families$color[1])[, "h"], hcl_of(s$families$color[2])[, "h"])
  expect_lt(gap(coherent), 90)
  expect_gt(gap(contrast), 120)
})

test_that("spatially adjacent subclusters get far-apart shades (not a monotone ramp)", {
  a <- data.frame(cluster = paste0("k", 1:4), family_id = "F1")
  nb <- data.frame(cluster = paste0("k", 1:4), x_centroid = 1:4, y_centroid = 0)
  sess <- generate_palette(a, mode = "harmonious", harmonious_style = "per_family",
    neighbors = nb, seed = 1)
  l <- hcl_of(sess$clusters$color[match(paste0("k", 1:4), sess$clusters$cluster)])[, "l"]
  monotone <- all(diff(l) > 0) || all(diff(l) < 0)
  expect_false(monotone)
  # every spatially-adjacent pair is spread by more than one even step
  even_step <- diff(range(l)) / (length(l) - 1)
  expect_gt(min(abs(diff(l))), even_step * 1.4)
})

test_that("lightness_range = \"auto\" fits non-manual colors to the manual picks", {
  a <- test_assignment()
  sess <- generate_palette(a, mode = "contrast", seed = 1)
  sess <- set_manual_color(sess, cluster = "c1", color = "#20304A") # dark
  sess <- set_manual_color(sess, cluster = "c2", color = "#E7D8C1") # light
  sess2 <- generate_palette(a, session = sess, mode = "contrast", seed = 2)

  man_l <- hcl_of(c("#20304A", "#E7D8C1"))[, "l"]
  auto_l <- hcl_of(sess2$clusters$color[!sess2$clusters$manual_color])[, "l"]
  expect_true(all(auto_l >= min(man_l) - 2 & auto_l <= max(man_l) + 2))
})

test_that("explicit numeric ranges are still honored", {
  sess <- generate_palette(test_assignment(), mode = "contrast",
    lightness_range = c(40, 60), chroma_range = c(30, 45), seed = 1)
  expect_equal(sess$params$lightness_range, c(40, 60))
  expect_false(sess$params$auto_lightness)
  l <- hcl_of(sess$clusters$color)[, "l"]
  expect_true(all(l >= 38 & l <= 62))
})

test_that("optimize_color keeps the hue but pulls lightness/chroma into a sane envelope", {
  neon <- "#7B00FF"
  fixed <- optimize_color(neon)
  h0 <- hcl_of(neon)[, "h"]
  h1 <- hcl_of(fixed)[, "h"]
  expect_lt(cdeg(h0, h1), 12)
  expect_match(fixed, "^#[0-9A-Fa-f]{6}$")

  sess <- generate_palette(test_assignment(), mode = "harmonious", seed = 1)
  snapped <- optimize_color("#00FF00", session = sess)
  sess_l <- stats::median(hcl_of(sess$clusters$color)[, "l"])
  expect_lt(abs(hcl_of(snapped)[, "l"] - sess_l), 30)
})

test_that("set_manual_color marks an entry manual and generate_palette never overwrites it", {
  assignment <- test_assignment()
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  sess <- set_manual_color(sess, cluster = "c1", color = "#123456")
  sess <- set_manual_color(sess, family = "F2", color = "#ABCDEF")

  expect_true(sess$clusters$manual_color[sess$clusters$cluster == "c1"])
  expect_true(sess$families$manual_color[sess$families$family_id == "F2"])

  for (seed in 2:4) {
    sess <- generate_palette(assignment, session = sess, mode = "contrast", seed = seed)
    expect_equal(sess$clusters$color[sess$clusters$cluster == "c1"], "#123456")
    expect_equal(sess$families$color[sess$families$family_id == "F2"], "#ABCDEF")
  }
})

test_that("manual override survives a family reassignment", {
  assignment <- test_assignment()
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  sess <- set_manual_color(sess, cluster = "c1", color = "#123456")

  moved <- assignment
  moved$family_id[moved$cluster == "c1"] <- "F3"
  sess2 <- generate_palette(moved, session = sess, mode = "harmonious", seed = 2)

  expect_equal(sess2$clusters$color[sess2$clusters$cluster == "c1"], "#123456")
  expect_equal(sess2$clusters$family_id[sess2$clusters$cluster == "c1"], "F3")
})

test_that("reset_overrides unlocks colors so the next generate_palette() call changes them", {
  assignment <- test_assignment()
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  sess <- set_manual_color(sess, cluster = "c1", color = "#123456")
  sess <- reset_overrides(sess, cluster = "c1", family = character(0))
  expect_false(sess$clusters$manual_color[sess$clusters$cluster == "c1"])

  sess2 <- generate_palette(assignment, session = sess, mode = "harmonious", seed = 99)
  expect_false(identical(sess2$clusters$color[sess2$clusters$cluster == "c1"], "#123456"))
})

test_that("generate_palette validates the assignment data frame", {
  expect_error(generate_palette(data.frame(cluster = "a")), "family_id")
})
