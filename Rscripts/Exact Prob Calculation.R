# Note: This calculation assumed that the rate parameter was estimated as
# sum(time) / (n * (1 - pi)). In applications, (1 - pi) is not known, and the
# denominator is typically replaced by sum(status). This introduces additional
# variability that is not captured by the exact calculation below.

#' Calculate Exact Probability
#' 
#' Calculates the exact probability that the estimated rate parameter for arm 1
#' is less than the estimated rate parameter for arm 2 assuming exponential
#' arrival and censoring times.
#' 
#' @param n Sample size, assuming 1:1 randomization.
#' @param rate1 True rate parameter for arm 1.
#' @param rate2 True rate parameter for arm 2.
#' @param censor_prop Expected censoring proportion.
#' @param tau Threshold after which an asymptotic approximation is applied. Note
#'   that the exact probability calculation may fail for large n due to
#'   underflow.
#' @return Numeric probability.
#' 
#' @importFrom stats dgamma integrate pgamma
#' @export
#' 
#' @examples 
#' rate1 <- log(2) / 120
#' rate2 <- log(2) / 100
#' prob <- ExactProb(n = 10, rate1 = rate1, rate2 = rate2)
#' prob <- ExactProb(n = 50, rate1 = rate1, rate2 = rate2)

ExactProb <- function(n, rate1, rate2, censor_prop = 0.0, tau = 25) {
  
  # Check for equality of rates.
  if (rate1 == rate2) { return(0.5) }
  
  # Upper limit of integration.
  beta1 <- rate1 / (1 - censor_prop)
  beta2 <- rate2 / (1 - censor_prop)
  
  # Use normal approximation when numerical integration is unreliable.
  if (n >= tau || min(beta1, beta2) < 0.01) {
    mu <- n * (1 / beta1 - 1 / beta2)
    sigma <- sqrt(n) * sqrt(1 / beta1^2 + 1 / beta2^2)
    prob <- pnorm(q = 0, mean = mu, sd = sigma, lower.tail = FALSE)
  } else {
    # Integrand.
    g <- function(x) {
      return(dgamma(x, shape = n, rate = beta1) * pgamma(q = x, shape = n, rate = beta2))
    }
    prob <- stats::integrate(f = g, lower = 0, upper = Inf)$value
  }

  return(prob)
}
