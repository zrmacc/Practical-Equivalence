# Purpose: Data generation for simulations.
# Updated: 2021-05-27

# -----------------------------------------------------------------------------
# Simulate censored exponential data.
# -----------------------------------------------------------------------------

#' Generate Censored Exponential Data
#' 
#' @param n Sample size.
#' @param rate True rate parameter.
#' @param pi Expected censoring proportion.
#' @return Data.frame.

GenExpData <- function(n, rate, pi = 0) {
  events <- rexp(n = n, rate = rate)
  if (pi > 0) {
    censors <- rexp(n = n, rate = rate * pi / (1 - pi))
  } else {
    censors <- rep(Inf, times = n)
  }
  out <- data.frame(
    time = pmin(events, censors),
    status = 1 * (events <= censors)
  )
  return(out)
}


#' Generate Two-Armed Data
#' 
#' Generates censored exponential data for two arms.
#' 
#' @param n Sample size.
#' @param rate1 True rate parameter for arm 1.
#' @param rate2 True rate parameter for arm 2.
#' @param censor_prop Expected censoring proportion.
#' @return Data.frame.

GenTwoArmData <- function(
  n,
  rate1,
  rate2,
  censor_prop = 0
) {
  arm1 <- GenExpData(n = n, rate = rate1, pi = censor_prop)
  arm1$arm <- 1
  arm2 <- GenExpData(n = n, rate = rate2, pi = censor_prop)
  arm2$arm <- 2
  out <- rbind(arm1, arm2)
  return(out)
}

