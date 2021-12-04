# Purpose: Determine the sample size needed for a pre-specified probability of 
# selecting the superior treatment.
# Updated: 2021-12-04

#' Sample Size Estimation
#' 
#' Estimate the sample size for a practical equivalence trial with a time to
#' event endpoint. The sample size is determined by specifying the time to event
#' distribution of each treatment arm, the margin of practical equivalence, and
#' the desired probability of selecting the superior treatment. The distribution
#' in each treatment arm may be specified either by providing the median, in
#' which case the time to event is assumed to be exponential, or by specifying
#' the shape and rate of a Weibull distribution. For guidance on how to set the
#' shape and rate parameters when using a Weibull calculation, see
#' \code{\link{WeibullSpec}}.
#' 
#' @param cens_prop Expected censoring proportion.
#' @param med1 Median for treatment arm 1, assuming shape1 is 1. Overwrites
#'   shape and rate if supplied.
#' @param shape1 Shape parameter for treatment arm 1.
#' @param rate1 Rate parameter for treatment arm 1.
#' @param med2 Median for treatment arm 2, assuming shape2 is 1. Overwrites
#'   shape and rate if supplied.
#' @param shape2 Shape parameter for treatment arm 2.
#' @param rate2 Rate parameter for treatment arm 2.
#' @param info_reps Replicates used for estimating the observed information
#'   matrix.
#' @param margin Margin of practical equivalence.
#' @param min_n Minimum allowable sample size.
#' @param max_n Maximum allowable sample size.
#' @param target_prob Probability of selecting the more effective treatment.
#' @param use_exp_calc If both shape parameters are 1, should the calculations
#'   be performed assuming an exponential distribution for the time to event in
#'   each arm? Default is TRUE.
#' @return Integer sample size.
#' @export
#' @examples 
#' # Sample size calculation based on exponentials. 
#' n <- SampleSize(
#'   cens_prop = 0.15,
#'   med1 = 9,
#'   med2 = 12
#' )
#' 
#' # Sample size calculation based on exponentials with a 2 month margin.
#' # Note that the required sample size is expected to increase.
#' n <- SampleSize(
#'   cens_prop = 0.15,
#'   med1 = 9,
#'   med2 = 12,
#'   margin = 2
#' )
#' 
#' # Sample size calculation based on Weibulls. 
#' n <- SampleSize(
#'   cens_prop = 0.15,
#'   shape1 = 2.8,
#'   rate1 = 0.10,
#'   shape2 = 4.0,
#'   rate2 = 0.08
#' )

SampleSize <- function(
  cens_prop = 0.0,
  med1 = NULL,
  shape1 = NULL,
  rate1 = NULL,
  med2 = NULL,
  shape2 = NULL,
  rate2 = NULL,
  info_reps = 50,
  min_n = 10,
  max_n = 100,
  margin = 0.0,
  target_prob = 0.8,
  use_exp_calc = TRUE
) {
  
  # Convert medians to rates, if supplied.
  if (!is.null(med1)) {
    shape1 <- 1
    rate1 <- log(2) / med1
  } else {
    med1 <- WeiMed(shape = shape1, rate = rate1)
  }
  if (!is.null(med2)) {
    shape2 <- 1
    rate2 <- log(2) / med2
  } else {
    med2 <- WeiMed(shape = shape2, rate = rate2)
  }
  
  # Check median1 <= median 2.
  if (med1 > med2) {
    warning("Median1 exceeds median2 whereas the reverse is expected.")
  }
  
  # Define wrapper function for equivalence probability as a function of n.
  SupProbN <- function(n) {
    out <- SupProb(
      cens_prop = cens_prop,
      n = n,
      shape1 = shape1,
      rate1 = rate1,
      shape2 = shape2,
      rate2 = rate2,
      margin = margin,
      info_reps = info_reps,
      use_exp_calc = use_exp_calc
    )
    return(out)
  }
  
  # Check that the target probability is attainable.
  prob_max_n <- SupProbN(max_n)
  
  if (prob_max_n < target_prob) {
    fail_message <- paste0(
      "Probability of selecting the more effective treatment is ",
      round(prob_max_n, digits = 3),
      ",\nwhich is less than the target probability of ",
      target_prob,
      ".\n"
    )
    stop(fail_message)
  }
  
  # Determine sample size.
  n <- min_n
  prob_n <- SupProbN(n)
  while(prob_n < target_prob) {
    n <- n + 1
    prob_n <- SupProbN(n) 
  }
  return(n)
}
