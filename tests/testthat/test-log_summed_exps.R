test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("log_summed_exps returns double output", {
  expect_type(log_summed_exps(1:2000), "double")
})

test_that("log_summed_exps returns finite output", {
  expect_lt(log_summed_exps(1:20000), Inf)
})

test_that("log_summed_exps doesn't work with characters", {
  expect_error(log_summed_exps(c("1", "2", "3")))
})

test_that("log_summed_exps of 0 equals 0", {
  expect_equal(log_summed_exps(0), 0)
})
