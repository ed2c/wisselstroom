test_that("input should be a data.frame", {
  expect_error(make_flow_basics("entering a string"))
  expect_error(make_flow_basics(7))

})

