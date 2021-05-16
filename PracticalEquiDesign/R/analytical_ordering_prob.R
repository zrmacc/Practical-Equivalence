#' Calculate Ordering Probability
#' 
#' Calculates the asymptotic probability that the estimated rate parameter for arm 1
#' is less than the estimated rate parameter for arm 2.
#' 
#' @param n Sample size, assuming 1:1 randomization.
#' @param rate1 True rate parameter for arm 1.
#' @param rate2 True rate parameter for arm 2.
#' @param med1 True median of arm 1, optionally supplied instead of rate1.
#' @param med2 True median of arm 2, optionally supplied instead of rate2.
#' @param censor_prop Expected censoring proportion.
#' @return Numeric probability.
#' 
#' @export
#' 
#' @examples 
#' prob <- OrderProb(n = 10, med1 = 8, med2 = 6)
#' prob <- OrderProb(n = 50, med1 = 8, med2 = 6)

OrderProb <- function(
  n, 
  rate1, 
  rate2,
  med1 = NULL,
  med2 = NULL,
  censor_prop = 0.0
) {
  # Convert medians to rates, if supplied.
  if (!is.null(med1)) {
    rate1 <- log(2) / med1
  }
  if (!is.null(med2)) {
    rate2 <- log(2) / med2
  }
  
  # Probability calculation.
  mu <- rate2 - rate1
  sigma <- sqrt((rate2^2 + rate1^2) / (n * (1 - censor_prop)))
  prob <- stats::pnorm(q = 0, mean = mu, sd = sigma, lower.tail = FALSE)
  return(prob)
}
