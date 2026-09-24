test_that("export_palette_json / import_palette_json round-trip a session", {
  assignment <- data.frame(cluster = paste0("c", 1:6), family_id = rep(c("F1", "F2"), each = 3))
  sess <- generate_palette(assignment, mode = "harmonious", seed = 1)
  sess <- set_manual_color(sess, cluster = "c1", color = "#123456")

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  export_palette_json(sess, tmp)
  sess2 <- import_palette_json(tmp)

  expect_s3_class(sess2, "palettome_session")
  expect_equal(sess2$clusters$cluster, sess$clusters$cluster)
  expect_equal(sess2$clusters$color, sess$clusters$color)
  expect_equal(sess2$clusters$manual_color, sess$clusters$manual_color)
  expect_equal(sess2$families$color, sess$families$color)
  expect_equal(sess2$params$mode, "harmonious")
})

test_that("export_palette_csv writes readable cluster and family tables", {
  assignment <- data.frame(cluster = paste0("c", 1:4), family_id = rep(c("F1", "F2"), each = 2))
  sess <- generate_palette(assignment, mode = "contrast", seed = 1)

  clusters_file <- tempfile(fileext = ".csv")
  families_file <- tempfile(fileext = ".csv")
  on.exit(unlink(c(clusters_file, families_file)))
  export_palette_csv(sess, clusters_file, families_file)

  cl <- utils::read.csv(clusters_file, stringsAsFactors = FALSE)
  fa <- utils::read.csv(families_file, stringsAsFactors = FALSE)
  expect_equal(sort(cl$cluster), sort(sess$clusters$cluster))
  expect_equal(sort(fa$family_id), sort(sess$families$family_id))
})
