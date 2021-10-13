# Purpose: Calculate parameters of the Weibull distribution from quantiles or event rates.
# Updated: 2021-10-13

#' Weibull Specification
#' 
#' Calculate shape and rate of weibull distribution from two quantiles.
#' 
#' @param t1 First time point.
#' @param p1 Probability at the first time point.
#' @param t2 Second time point.
#' @param p2 Probability at the send time point.
#' @return Numeric vector containing the shape and rate.
#' @export
#' @examples 
#' theta <- WeibullSpec(t1 = 6, p1 = 0.8, t2 = 12, p2 = 0.5)

WeibullSpec <- function(t1, p1, t2, p2) {
  
  # Flag inconsistencies. 
  if (any(t1 < t2 & p1 < p2, t1 > t2 & p1 > p2)) {
    stop("Survival probability at the first time point must be greater")
  }
  
  # Parameter calculation.
  alpha <- log(log(p1) / log(p2)) / log(t1 / t2)
  lambda <- (-log(p2)) ^ (1 / alpha) / t2
  out <- c(shape = alpha, rate = lambda)
  return(out)
}
