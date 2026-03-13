test_that("lm_simple works", {
  # Ensure we can serialize and deserialize a model and get the same predictions
  data(mtcars)

  # Create some categorical variables
  d = mtcars |>
    dplyr::mutate(
      wtclass = floor(wt)
    )

  mod = lm(mpg ~ cyl + disp + factor(wtclass), d)

  file = tempfile(fileext = ".zip")

  writer = abort_on_error(ArchiveWriter$new(file))
  write_lm(mod, writer, "test")
  abort_on_error(writer$finish())

  reader = abort_on_error(ArchiveReader$new(file))
  mod_simple = read_lm(reader, "test")

  # lm_simple doesn't support rownames
  pred_orig = unname(predict(mod, d))
  pred_new = predict(mod_simple, d)

  expect_true(all.equal(pred_orig, pred_new))
})
