test_assignment <- function() {
  data.frame(cluster = paste0("c", 1:12), family_id = rep(paste0("F", 1:4), each = 3))
}

test_that("contrast mode keeps a minimum perceptual distance between all clusters", {
  assignment <- test_assignment()
  for (seed in 1:5) {
    sess <- generate_palette(assignment, mode = "contrast", seed = seed)
    d <- color_distance(sess$clusters$color)
    diag(d) <- NA
    expect_gt(min(d, na.rm = TRUE), 15)
  }
})

test_that("harmonious mode separates family base hues", {
  assignment <- test_assignment()
  for (seed in 1:5) {
    sess <- generate_palette(assignment, mode = "harmonious", seed = seed)
    d <- color_distance(sess$families$color)
    diag(d) <- NA
    expect_gt(min(d, na.rm = TRUE), 15)
  }
})

test_that("harmonious mode still keeps members of one family distinguishable", {
  assignment <- test_assignment()
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  min_within <- vapply(unique(sess$clusters$family_id), function(f) {
    cols <- sess$clusters$color[sess$clusters$family_id == f]
    d <- color_distance(cols)
    diag(d) <- NA
    min(d, na.rm = TRUE)
  }, numeric(1))
  expect_true(all(min_within > 8))
})

test_that("harmonious mode gives same-family clusters similar hue but different lightness", {
  assignment <- test_assignment()
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  fam1 <- sess$clusters[sess$clusters$family_id == "F1", ]
  luv <- colorspace::coords(methods::as(colorspace::hex2RGB(fam1$color), "polarLUV"))
  expect_lt(diff(range(luv[, "H"])), 10) # near-identical hue
  expect_gt(diff(range(luv[, "L"])), 10) # but spread in lightness
})

test_that("set_manual_color marks an entry manual and generate_palette never overwrites it", {
  assignment <- test_assignment()
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  sess <- set_manual_color(sess, cluster = "c1", color = "#123456")
  sess <- set_manual_color(sess, family = "F2", color = "#ABCDEF")

  expect_true(sess$clusters$manual_color[sess$clusters$cluster == "c1"])
  expect_true(sess$families$manual_color[sess$families$family_id == "F2"])

  # regenerate repeatedly, with a different mode/seed each time
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
  moved$family_id[moved$cluster == "c1"] <- "F3" # c1 moves from F1 to F3
  sess2 <- generate_palette(moved, session = sess, mode = "harmonious", seed = 2)

  expect_equal(sess2$clusters$color[sess2$clusters$cluster == "c1"], "#123456")
  expect_equal(sess2$clusters$family_id[sess2$clusters$cluster == "c1"], "F3")
})

test_that("reset_overrides unlocks colors so the next generate_palette() call changes them", {
  assignment <- test_assignment()
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  sess <- set_manual_color(sess, cluster = "c1", color = "#123456")
  expect_true(sess$clusters$manual_color[sess$clusters$cluster == "c1"])

  sess <- reset_overrides(sess, cluster = "c1", family = character(0))
  expect_false(sess$clusters$manual_color[sess$clusters$cluster == "c1"])

  sess2 <- generate_palette(assignment, session = sess, mode = "harmonious", seed = 99)
  expect_false(identical(sess2$clusters$color[sess2$clusters$cluster == "c1"], "#123456"))
})

test_that("generate_palette validates the assignment data frame", {
  expect_error(generate_palette(data.frame(cluster = "a")), "family_id")
})
