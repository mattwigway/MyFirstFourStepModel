test_that("mnl_simple works", {
  # Ensure we can serialize and deserialize a model and get the same predictions
  data(mtcars)

  # Create some categorical variables
  d = mtcars |>
    dplyr::mutate(
      cylfac = factor(paste0("cylinders", cyl))
    )

  mod = multinom(cylfac ~ drat + factor(gear), d)

  file = tempfile(fileext = ".zip")

  writer = abort_on_error(ArchiveWriter$new(file))
  write_mnl(mod, writer, "test")
  abort_on_error(writer$finish())

  reader = abort_on_error(ArchiveReader$new(file))
  mod_simple = read_mnl(reader, "test")

  pred_orig = predict(mod, d, type = "probs")

  # mnl_simple does not support rownames
  rownames(pred_orig) <- NULL

  pred_new = predict(mod_simple, d, type = "probs")

  expect_true(all.equal(pred_orig, pred_new))
})
