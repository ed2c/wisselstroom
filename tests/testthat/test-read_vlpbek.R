test_that("vlpbek file can be read in", {
  expect_error(read_vlpbek("not_the_correct_file"))
})
