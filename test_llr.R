context("Check local linear regression function")
source("llr_function.R")

n = 150
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix
  expect_true(is.diagonal.matrix(make_weight_matrix(-.5, rnorm(5), omega = 2), TRUE))
  ## check if all the elements are positive
  expect_equal(diag(make_weight_matrix(0, rnorm(5), omega = 1)) >= 0, rep(TRUE, times=5))
  ## check if the weights are correct in simple cases where you know what the output should be
  expect_equal(round(diag(make_weight_matrix(.2, c(-.4, -.2, 0, .2, .4), omega = 1)), digits = 3), c(0.482, 0.820, 0.976, 1, 0.976))
})

test_that("make_predictor_matrix works on simple cases", {
  ## write tests to check that the dimensions are correct
  expect_equal(ncol(make_predictor_matrix(rnorm(10))), 2)
  expect_equal(nrow(make_predictor_matrix(rnorm(10))), 10)
  ##the first column is all 1's, etc.
  expect_equal(make_predictor_matrix(rnorm(5))[,1], rep(1, times=5))
  expect_equal(make_predictor_matrix(rnorm(10))[,1], rep(1, times=10))
})