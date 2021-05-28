# Purpose: Calculate the probability that the more effective treatment is
#   selected.
# Updated: 2021-05-27

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

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
#' @param mc_iter Monte Carlo iterations.
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