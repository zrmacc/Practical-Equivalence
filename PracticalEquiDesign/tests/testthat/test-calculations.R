library(PracticalEquiDesign)
library(testthat)

# Median calculation.
test_that("Check Weibull median calculation.", {
  
  med <- WeiMed(shape = 1, rate = 1)
  exp <- log(2)
  expect_equal(med, exp, tolerance = 1e-8)
  
  med <- WeiMed(shape = 1, rate = 2)
  exp <- log(2) / 2
  expect_equal(med, exp, tolerance = 1e-8)
  
  med <- WeiMed(shape = 2, rate = 2)
  exp <- 0.5 * sqrt(log(2))
  expect_equal(med, exp, tolerance = 1e-8)
  
})


# Information calculation.
test_that("Check Weibull information calculation.", {
  
  withr::local_seed(101)
  data <- Temporal::GenData(100, "weibull", theta = c(1, 2), p = 0.2)
  info <- WeiInfo(data, shape = 1, rate = 2)
  ll <- function(x) {Temporal::SurvLogLik(data, "weibull", theta = x)}
  exp <- -numDeriv::hessian(func = ll, x = c(1, 2))
  expect_equal(c(info), c(exp), tolerance = 1e-8)
  
})


# Standard Error calculation.
test_that("Check Weibull standard error calculation.", {
  
  withr::local_seed(101)
  n <- 100
  data <- Temporal::GenData(100, "weibull", theta = c(1, 2), p = 0.2)
  info <- WeiInfo(data, shape = 1, rate = 2)
  se <- WeiMedSE(info, n = n, shape = 1, rate = 2)
  
  me <- function(x) {WeiMed(x[1], x[2])}
  grad <- numDeriv::grad(func = me, x = c(1, 2))
  exp <- sqrt(as.numeric(grad %*% solve(info) %*% grad))
  expect_equal(se, exp, tolerance = 1e-8)
  
})


# Test Weibull specification.
test_that("Check specification of Weibull from time points.", {
  
  theta <- WeibullSpec(t1 = 6, p1 = 0.8, t2 = 12, p2 = 0.5)
  surv <- Temporal::SurvFunc(dist = "weibull", theta = theta)
  expect_equal(0.8, surv(6), ignore_attr = TRUE)
  expect_equal(0.5, surv(12), ignore_attr = TRUE)
  
})
