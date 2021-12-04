# Purpose: Plot the sample size curve.
# Updated: 2021-12-04

#' Plot Sample Size Curve
#' 
#' Plot the probability of selecting the superior treatment as a function of
#' the sample size n. 
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
#' @param delta Increment between consecutive sample sizes to evaluate.
#' @param min_n Minimum allowable sample size.
#' @param max_n Maximum allowable sample size.
#' @param target_prob Probability of selecting the more effective treatment.
#' @param use_exp_calc If both shape parameters are 1, should the calculations
#'   be performed assuming an exponential distribution for the time to event in
#'   each arm?
#' @return ggplot object.
#' @export
#' @examples 
#' # Plot the selection probability curve for the case of two exponentials
#' # with medians of 9 and 12 (e.g.) months, and a 2 month margin of
#' # practical equivalence.
#' q <- ProbCurve(
#'   cens_prop = 0.15,
#'   med1 = 9,
#'   med2 = 12,
#'   margin = 2.0
#' )

ProbCurve <- function(
  cens_prop = 0.0,
  med1 = NULL,
  shape1 = NULL,
  rate1 = NULL,
  med2 = NULL,
  shape2 = NULL,
  rate2 = NULL,
  info_reps = 50,
  delta = 1,
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
    med1 <- WeiMed(shape1, rate1)
  }
  if (!is.null(med2)) {
    shape2 <- 1
    rate2 <- log(2) / med2
  } else {
    med2 <- WeiMed(shape2, rate2)
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
  
  # Curve.
  n <- NULL
  prob <- NULL
  sample_sizes <- seq(from = min_n, to = max_n, by = delta)
  df <- data.frame(
    n = sample_sizes,
    prob = sapply(sample_sizes, SupProbN)
  )
  
  # Plotting.
  q <- ggplot2::ggplot(data = df) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = target_prob),
      color = "gray",
      linetype = "dotted"
    ) + 
    ggplot2::geom_line(
      ggplot2::aes(x = n, y = prob),
      color = "#4285F4"
    ) + 
    ggplot2::geom_point(
      ggplot2::aes(x = n, y = prob),
      color = "#4285F4"
    ) + 
    ggplot2::labs(
      x = "Sample Size",
      y = "Probability of\nCorrect Selection"
    )
  return(q)
}
