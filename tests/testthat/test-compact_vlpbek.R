test_that("compact_vlpbek only accepts a vlpbek", {
  expect_error(compact_vlpbek("not a vlpbek"))
})
