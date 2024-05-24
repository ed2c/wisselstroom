test_that("key_metrics only accepts a vlpbek", {
  expect_error(key_metrics("not a vlpbek"))
})
