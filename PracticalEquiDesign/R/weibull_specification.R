# Purpose: Calculate parameters of the Weibull distribution.
# Updated: 2021-12-04

#' Weibull Specification
#' 
#' Calculate shape and rate of a Weibull distribution from the value
#' of the survival curve at 2 time points.
#' 
#' @param t1 First time point.
#' @param p1 Probability at the first time point.
#' @param t2 Second time point.
#' @param p2 Probability at the second time point.
#' @return Numeric vector containing the shape and rate.
#' @export
#' @examples 
#' # Determine the shape and rate parameter of a Weibull distribution
#' # where survival at 6 (e.g.) months is 80%, and survival at 12
#' # months is 50%.
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
