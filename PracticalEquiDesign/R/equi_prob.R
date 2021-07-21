# Purpose: Calculate the probability of practical equivalence under Weibull models.
# Updated: 2021-07-20


#' Quadratic Form
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
#' @param shape Shape parameter, `alpha`.
#' @param rate Rate parameter, `lambda`.
#' @return Numeric median.
#' @export

WeiMed <- function(shape, rate) {
  mu <- (1 / rate) * exp(log(log(2)) / shape)
  return(mu)
}


#' Weibull Information Matrix.
#' 
#' Information matrix for the Weibull shape and rate parameters.
#' 
#' @param data Data.frame.
#' @param shape Shape parameter, alpha.
#' @param rate Rate parameter, lambda.
#' @return Numeric information matrix for shape and rate.

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
#' Estimate the expected information as the average observed information.
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


#' Weibull Median SE
#' 
#' @param info Information matrix.
#' @param n Sample size.
#' @param shape Shape parameter `alpha`.
#' @param rate Rate parameter `lambda`.
#' @return Numeric standard error of the median.

WeiMedSE <- function(info, n, shape, rate) {
  grad <- c(
    shape = (-1 / rate) * exp(log(log(2)) / shape) * log(log(2)) / shape^2,
    rate = (-1 / rate^2) * exp(log(log(2)) / shape)
  )
  se2 <- QF(grad, solve(info)) 
  return(sqrt(se2))
}


#' Practical Equivalence Probability
#' 
#' Equivalence probability, as pr(median2 - median1 >= margian) 
#' + 0.5 * pr(abs(median2 - median1) < margin).
#' 
#' @param cens_prop Expected censoring proportion.
#' @param n Sample size.
#' @param rate1 Rate parameter for arm 1.
#' @param rate2 Rate parameter for arm 2.
#' @param shape1 Shape parameter for arm 1.
#' @param shape2 Shape parameter for arm 2.
#' @param margin Equivlence margin.
#' @param reps Simulation replicates. 
#' @return Numeric equivalence probability.
#' @export 

EquiProb <- function(
  cens_prop,
  n,
  rate1,
  rate2,
  shape1,
  shape2,
  margin = 0,
  reps = 10
) {
 
  # Arm 1.
  me1 <- WeiMed(shape1, rate1)
  info1 <- WeiAvgInfo(cens_prop, n, shape1, rate1, reps)
  se1 <- WeiMedSE(info1, n, shape1, rate1)
  
  # Arm 2.
  me2 <- WeiMed(shape2, rate2)
  info2 <- WeiAvgInfo(cens_prop, n, shape2, rate2, reps)
  se2 <- WeiMedSE(info2, n, shape2, rate2)
  
  # SE of the difference.
  delta <- me2 - me1
  se12 <- sqrt(se1^2 + se2^2)
  
  # Equivalence probability.
  prob <- 1 - 0.5 * stats::pnorm((margin - delta) / se12) -
    0.5 * stats::pnorm((-margin - delta) / se12)
  return(prob)
}
