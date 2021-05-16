#' Probability by Sample Size Curve.
#' 
#' Constructs a data.frame containing the probability that the rate 
#' parameter for arm 1 is <= the rate parameter for arm 2 for 
#' sample size between min_n and max_n.
#' 
#' @param rate1 True rate parameter for arm 1.
#' @param rate2 True rate parameter for arm 2.
#' @param censor_prop Expected censoring proportion.
#' @param min_n Minimum allowable sample size.
#' @param max_n Maximum allowable sample size.
#' @return Data.frame.
#' 
#' @export

ProbCurve <- function(
  rate1, 
  rate2, 
  censor_prop = 0.0, 
  min_n = 1,
  max_n = 100
) {
  
  # Sample size sequence.
  sample_size <- seq(from = min_n, to = max_n)
  
  # Selection probabilities.
  probs <- sapply(
    sample_size, function(n) {
      OrderProb(n = n, rate1 = rate1, rate2 = rate2, censor_prop = censor_prop)
    })
  
  # Sample size by selection probability curve.
  curve <- data.frame(
    sample_size = sample_size,
    selection_prob = probs
  )
  return(curve)
}


#' Calculate Sample Size
#' 
#' Calculates the sample size needed to provide a specified probability of
#' selecting the more-effective treatment.
#'
#' @param rate1 True rate parameter for arm 1.
#' @param rate2 True rate parameter for arm 2.
#' @param med1 True median of arm 1, optionally supplied instead of rate1.
#' @param med2 True median of arm 2, optionally supplied instead of rate2.
#' @param censor_prop Expected censoring proportion.
#' @param min_n Minimum allowable sample size.
#' @param max_n Maximum allowable sample size.
#' @param target_prob Probability of selecting the more-effective treatment.
#' @return Integer sample size.
#' 
#' @export
#' 
#' @examples 
#' # Sample size calculation, specified in terms of rates.
#' n <- SampleSize(rate1 = 1.0, rate2 = 1.4)
#' 
#' # Sample size calculation, specified in terms of medians.
#' n <- SampleSize(med1 = 12, med2 = 8)

SampleSize <- function(
  rate1, 
  rate2,
  med1 = NULL,
  med2 = NULL,
  censor_prop = 0.00, 
  min_n = 1,
  max_n = 100, 
  target_prob = 0.90
) {
  
  # Convert medians to rates, if supplied.
  if (!is.null(med1)) {
    rate1 <- log(2) / med1
  }
  if (!is.null(med2)) {
    rate2 <- log(2) / med2
  }
  
  # Check maximum.
  max_prob <- OrderProb(
    n = max_n, 
    rate1 = rate1,
    rate2 = rate2,
    censor_prop = censor_prop
  )
  
  if (max_prob < target_prob) {
    stop("Target selection probability cannot be achieved with current\nmaximum sample size.")
  }
  
  # Probability curve.
  prob_curve <- ProbCurve(
    rate1 = rate1,
    rate2 = rate2,
    censor_prop = censor_prop,
    min_n = min_n,
    max_n = max_n
  )
  
  # Sample size calculation.
  sample_size <- min(prob_curve$sample_size[prob_curve$selection_prob >= target_prob])
  return(sample_size)
}
