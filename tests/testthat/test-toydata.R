test_that("the test passed", {
  expect_equal(OR_95CI(0.2, 0.3, 0.05, 4), "1.2214 (0.6784, 2.1990)")
})
