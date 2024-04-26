test_that("input should be a data.frame", {
  expect_error(vlpbek("entering a string"))
  expect_error(vlpbek(7))

})

