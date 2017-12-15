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

this_ss <- sample_summary(this_n, this_dist, par = this_par)

ss_sample <- this_ss[[1]]
ss_summary <- this_ss[[2]]


test_that("any_dist returns what it should", {
  # Check all defaults.
  expect_equal(any_dist(x, dist = "norm"), qnorm(x))

  # Check default parameters.
  expect_equal(any_dist(x, dist = "lnorm", type = "p"), plnorm(x))

  # Check default type.
  expect_equal(any_dist(x, dist = "lnorm", type = "q"), qlnorm(x))

})

test_that("sample_summary matches summary stats", {
  # Check all column outputs.
  expect_equal(mean(ss_sample), ss_summary$mean)
  expect_equal(median(ss_sample), ss_summary$median)
  expect_equal(min(ss_sample), ss_summary$min)
  expect_equal(max(ss_sample), ss_summary$max)
  expect_equal(IQR(ss_sample), ss_summary$iqr)
  expect_equal(range(ss_sample)[2] - range(ss_sample)[1], ss_summary$range)
  expect_equal(sd(ss_sample), ss_summary$sd)
  expect_equal(var(ss_sample), ss_summary$var)
})
