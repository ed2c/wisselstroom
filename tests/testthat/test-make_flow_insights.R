test_that("make_flow_insights only accepts a flow_basics", {
  expect_error(make_flow_insights("not a flow_basics"))
})
