#' Empirical Equivalence Probability
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

EmpEquiProb <- function(
  cens_prop,
  n,
  rate1,
  rate2,
  shape1,
  shape2,
  margin = 0,
  reps = 100
) {
  
  sim <- sapply(seq_len(reps), function(x) {
    
    # Generate data.
    data1 <- Temporal::GenData(n, "weibull", theta = c(shape1, rate1), p = cens_prop)
    data2 <- Temporal::GenData(n, "weibull", theta = c(shape2, rate2), p = cens_prop)
    
    # Obtain MLEs.
    fit1 <- try(Temporal::FitParaSurv(data1, dist = "weibull"), silent = TRUE)
    fit2 <- try(Temporal::FitParaSurv(data2, dist = "weibull"), silent = TRUE)
    
    if (!is(fit1, "try-error") & !is(fit2, "try-error")) {
      me1 <- fit1@Outcome$Estimate[2]
      me2 <- fit2@Outcome$Estimate[2]
      
      # Indicator for event of interest.
      out <- 1 * (me2 - me1 >= margin) + 0.5 * (abs(me2 - me1) < margin)
      return(out)
    }
  })
  return(mean(sim))
}
