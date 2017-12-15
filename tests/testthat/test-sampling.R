library(varameta)
context("sampling")

# Create a random sample from a random distribution for testing.

this_dist <- sample(# Random distribution.
  c("lnorm", "norm", "weibull", "exp"),
  size = 1)

this_n <- sample(# Random sample size.
  c(2:1000),
  size = 1)

this_par <- if (this_dist == "lnorm") {
  list(meanlog = 4, meansd = 0.3)
} else if (this_dist == "norm") {
  list(mean = 50, sd = 17)
} else if (this_dist == "weibull") {
  list(shape = 2, scale = 35)
} else if (this_dist == "exp") {
  list(rate = 10)
} else if (this_dist == "beta") {
  list(shape1 = 9, shape2 = 4)
}

x <- runif(1, 0.2, 0.8)

this_sample <-
  get_sample(n = this_n, dist = this_dist, par = this_par)

test_that("any_dist returns what it should", {
  # Check all defaults.
  expect_equal(any_dist(x, dist = "norm"), qnorm(x))

  # Check default parameters.
  expect_equal(any_dist(x, dist = "lnorm", type = "p"), plnorm(x))

  # Check default type.
  expect_equal(any_dist(x, dist = "lnorm", type = "q"), qlnorm(x))

})

# test_that("get_summary matches summary stats",)
