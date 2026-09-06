test_that("cluster_colors / family_colors return a named vector by default", {
  assignment <- data.frame(cluster = paste0("c", 1:4), family_id = rep(c("F1", "F2"), each = 2))
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)

  cc <- cluster_colors(sess)
  expect_type(cc, "character")
  expect_named(cc, sess$clusters$cluster)
  expect_equal(unname(cc), sess$clusters$color)

  fc <- family_colors(sess)
  expect_named(fc, sess$families$family_id)
  expect_equal(unname(fc), sess$families$color)
})

test_that("cluster_colors / family_colors can return a plain data frame instead", {
  assignment <- data.frame(cluster = paste0("c", 1:4), family_id = rep(c("F1", "F2"), each = 2))
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)

  cc <- cluster_colors(sess, format = "data.frame")
  expect_s3_class(cc, "data.frame")
  expect_named(cc, c("cluster", "color", "family_id", "family_color"))
  expect_equal(cc$color, sess$clusters$color)
  expect_equal(cc$family_id, sess$clusters$family_id)
  # family_color for each cluster matches that family's own color
  expect_equal(
    cc$family_color,
    sess$families$color[match(cc$family_id, sess$families$family_id)]
  )

  fc <- family_colors(sess, format = "data.frame")
  expect_named(fc, c("family_id", "color"))
})
