# Purpose: Calculate the probability of practical equivalence under Weibull models.
# Updated: 2021-07-20


#' Quadratic Form
#' 
#' Calculate the quadratic form q(x) = x'Ax. 
#' 
#' @param x Numeric vector.
#' @param A Numeric matrix.
#' @return Numeric scalar.
#' @noRd

QF <- function(x, A) {
  x <- matrix(x, ncol = 1)
  out <- t(x) %*% A %*% x
  return(as.numeric(out))
}


#' Weibull Median
#' 
#' Calculate the median of a Weibull distribution from the shape and rate.
#' 
#' @param shape Shape parameter, `alpha`.
#' @param rate Rate parameter, `lambda`.
#' @return Numeric median.
#' @export
#' @examples 
#' # In the case of shape = 1 and rate = 1, the distribution
#' # is exponential, and the median is log(2).
#' med <- WeiMed(shape = 1, rate = 1)

WeiMed <- function(shape, rate) {
  mu <- (1 / rate) * exp(log(log(2)) / shape)
  return(mu)
}


#' Weibull Information Matrix.
#' 
#' Calculate the 2x2 information matrix for the Weibull shape and rate
#' parameters.
#' 
#' @param data Data.frame.
#' @param shape Shape parameter, alpha.
#' @param rate Rate parameter, lambda.
#' @return Numeric information matrix for shape and rate.
#' @noRd 

WeiInfo <- function(
  data,
  shape,
  rate
) {
  
  # Unpack.
  time <- data$time
  logtime <- log(time)
  nobs <- sum(data$status)
  tobs <- data$time[data$status == 1]
  logtobs <- log(tobs)
  
  # Calculation.
  ta <- data$time^shape
  s0 <- sum(ta)
  s1 <- sum(ta * logtime)
  s2 <- sum(ta * (logtime)^2)
  
  # Information for shape.
  info_shape <- (nobs / (shape^2)) + 
    (rate^shape) * (log(rate))^2 * s0 + 
    2 * (rate^shape) * log(rate) * s1 + 
    (rate^shape) * s2
  
  # Information for rate.
  info_rate <- (nobs * shape) / (rate^2) + 
    shape * (shape - 1) * (rate^(shape - 2)) * s0
  
  # Cross information.
  cross_info <- -(nobs / rate) + 
    (rate^(shape - 1)) * s0 + 
    shape * (rate^(shape - 1)) * log(rate) * s0 + 
    shape * (rate^(shape - 1)) * s1
  
  # Output.
  info <- matrix(c(info_shape, cross_info, cross_info, info_rate), nrow = 2)
  dimnames(info) <- list(c("shape", "rate"), c("shape", "rate"))
  return(info)
}


#' Weibull Average Information
#' 
#' Estimate the expected information as the average value of the observed
#' information across `reps` realizations of the data.
#' 
#' @param cens_prop Censoring proportion.
#' @param n Sample size.
#' @param shape Shape parameter `alpha`.
#' @param rate Rate parameter `lambda`.
#' @param reps Replicates to average.
#' @return Numeric information matrix for shape and rate.

WeiAvgInfo <- function(cens_prop, n, shape, rate, reps = 10) {
  sim <- lapply(seq_len(reps), function(x) {
    data <- Temporal::GenData(n, "weibull", theta = c(shape, rate), p = cens_prop)
    info <- WeiInfo(data, shape, rate)
    return(info)
  })
  out <- Reduce("+", sim) / reps
  return(out)
}


#' Exponential Median SE
#' 
#' Standard error of the median of an exponential distribution
#' with estimated rate parameter.
#' 
#' @param cens_prop Expected censoring proportion.
#' @param n Sample size.
#' @param rate Rate parameter `lambda`.
#' @return Numeric standard error of the median.
#' @noRd

ExpMedSE <- function(cens_prop, n, rate) {
  se2 <- log(2)^2 / (n * rate^2 * (1 - cens_prop))
  return(sqrt(se2))
}


#' Weibull Median SE
#' 
#' Standard error of the median of a Weibull distribution with
#' estimated shape and rate parameters.
#' 
#' @param info Information matrix.
#' @param n Sample size.
#' @param shape Shape parameter `alpha`.
#' @param rate Rate parameter `lambda`.
#' @return Numeric standard error of the median.
#' @noRd

WeiMedSE <- function(info, n, shape, rate) {
  grad <- c(
    shape = (-1 / rate) * exp(log(log(2)) / shape) * log(log(2)) / shape^2,
    rate = (-1 / rate^2) * exp(log(log(2)) / shape)
  )
  se2 <- QF(grad, solve(info)) 
  return(sqrt(se2))
}


#' Superiority Probability
#' 
#' Probability of selecting the more effective treatment as 
#' pr(median2 - median1 >= margin) + 0.5 * pr(abs(median2 - median1) < margin).
#' 
#' @param cens_prop Expected censoring proportion.
#' @param n Sample size.
#' @param med1 Median for treatment arm 1, assuming shape1 = 1. Overwrites
#'   shape and rate if supplied.
#' @param shape1 Shape parameter for arm 1.
#' @param rate1 Rate parameter for arm 1.
#' @param med2 Median for treatment arm 2, assuming shape2 = 1. Overwrites
#'   shape and rate if supplied.
#' @param shape2 Shape parameter for arm 2.
#' @param rate2 Rate parameter for arm 2.
#' @param info_reps Replicates used for estimating the observed information
#'   matrix.
#' @param margin Margin of practical equivalence.
#' @param use_exp_calc If both shape parameters are 1, should the calculations
#'   be performed assuming an exponential distribution for the time to event in
#'   each arm? Default is TRUE.
#' @return Numeric equivalence probability.
#' @export 
#' @examples 
#' # Calculation in the case of exponentials with no margin.
#' prob <- SupProb(
#'   cens_prop = 0.15,
#'   n = 100,
#'   med1 = 9,
#'   med2 = 12,
#' )
#' 
#' # Calculation in the case of exponentials with a 2 month margin.
#' # The probability should be lower than in the absence of a margin.
#' prob <- SupProb(
#'   cens_prop = 0.15,
#'   n = 100,
#'   med1 = 9,
#'   med2 = 12,
#'   margin = 2
#' )
#' 
#' # Calculation in the case of Weibulls with a 2 month margin.
#' prob <- SupProb(
#'   cens_prop = 0.15,
#'   n = 100,
#'   shape1 = 2.8,
#'   rate1 = 0.10,
#'   shape2 = 4.0,
#'   rate2 = 0.08,
#'   margin = 2
#' )

SupProb <- function(
  cens_prop,
  n,
  med1 = NULL,
  shape1 = NULL,
  rate1 = NULL,
  med2 = NULL,
  shape2 = NULL,
  rate2 = NULL,
  info_reps = 50,
  margin = 0,
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
  
  # If both arms have shape == 1, employ exponential calculation.
  if (all(shape1 == 1, shape2 == 1, use_exp_calc)) {
    
    # Arm 1. 
    se1 <- ExpMedSE(cens_prop = cens_prop, n = n, rate = rate1)
    
    # Arm 2.
    se2 <- ExpMedSE(cens_prop = cens_prop, n = n, rate = rate2)

  } else {
    
    # Arm 1.
    info1 <- WeiAvgInfo(
      cens_prop = cens_prop, 
      n = n, 
      shape = shape1, 
      rate = rate1, 
      reps = info_reps
    )
    se1 <- WeiMedSE(info = info1, n = n, shape = shape1, rate = rate1)
    
    # Arm 2.
    info2 <- WeiAvgInfo(
      cens_prop = cens_prop, 
      n = n, 
      shape = shape2, 
      rate = rate2, 
      reps = info_reps
    )
    se2 <- WeiMedSE(info = info2, n = n, shape = shape2, rate = rate2)
    
  }
  
  # SE of the difference.
  delta <- med2 - med1
  se12 <- sqrt(se1^2 + se2^2)
  
  # Equivalence probability.
  prob <- 1 - 0.5 * stats::pnorm((margin - delta) / se12) -
    0.5 * stats::pnorm((-margin - delta) / se12)
  return(prob)
}
