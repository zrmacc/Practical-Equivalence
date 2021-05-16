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


# -----------------------------------------------------------------------------
# Empirical Ordering Probability
# -----------------------------------------------------------------------------

#' Empirical Ordering Probability
#' 
#' Calculate the probability that the estimated rate1 is < the estimated rate2
#' via simulation.
#' 
#' @param n Sample size.
#' @param rate1 True rate parameter for arm 1.
#' @param rate2 True rate parameter for arm 2.
#' @param alpha Type 1 error, for confidence interval.
#' @param calc_ci Logical, calculate a confidence interval?
#' @param censor_prop Expected censoring proportion.
#' @param mc_iter Montecarlo iterations.
#' @return Data.frame if calc_ci, else numeric probability.

EmpOrderProb <- function(
  n,
  rate1,
  rate2,
  alpha = 0.05,
  calc_ci = FALSE,
  censor_prop = 0.2,
  mc_iter = 1e4
) {
  
  # Simulates data, estimates the rates parameter for each arm,
  # returns 1 if the estimated rate for arm 2 is greater than
  # the estimated rate for arm 1.
  Simulate <- function(i) {
    out <- GenTwoArmData(n, rate1, rate2, censor_prop) %>%
      dplyr::group_by(arm) %>% 
      dplyr::summarise(
        rate = sum(status) / sum(time)
      )
    return(out$rate[out$arm == 2] - out$rate[out$arm == 1] > 0)
  }
  
  sim <- sapply(seq_len(mc_iter), Simulate)
  prob <- mean(sim)
  if (calc_ci) {
    z <- qnorm(p = 1 - alpha / 2)
    se <- sqrt(prob * (1 - prob) / mc_iter)
    out <- data.frame(
      prob = prob,
      se = se,
      lower = prob - z * se,
      upper = prob + z * se
    )
  } else {
    out <- prob
  }
  return(out)  
}

