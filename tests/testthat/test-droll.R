test_that("roll d20", {
  this_roll <-
    droll(20)
  expect_gt(this_roll, 0)
  expect_lt(this_roll, 21)

})


test_that("roll more than one dice", {
  n_dice <- sample(1:100, 1)
  this_roll <-
    droll(15, n_dice)

  expect_equal(length(this_roll), n_dice)
  expect_true(all(this_roll < 16))
  expect_true(all(this_roll > 0))

})
