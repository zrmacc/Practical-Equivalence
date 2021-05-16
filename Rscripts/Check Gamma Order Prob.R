library(ggplot2)
library(PracticalEquiDesign)
source("Rscripts/Exact Prob Calculation.R")

# -----------------------------------------------------------------------------
# Check probability calculation for difference of gamma distributions.
# -----------------------------------------------------------------------------

#' Empirical gamma difference calculation.
#' 
#' Calculates the probability Gamma(n, rate1) < Gamma(n, rate2).
#' 
#' @param n True shape parameter.
#' @param rate1 True rate parameter for arm 1.
#' @param rate2 True rate parameter for arm 2.
#' @param mc_iter Montecarlo iterations.
#' @return Numeric probability.

GammaDiffSim <- function(
  n, 
  rate1, 
  rate2, 
  mc_iter = 1e4
) {
  trial <- function(i) {
    return(rgamma(n = 1, shape = n, rate = rate1) < 
             rgamma(n = 1, shape = n, rate = rate2))
  }
  sim <- sapply(seq_len(mc_iter), trial)
  return(mean(sim))
}


# -----------------------------------------------------------------------------

# Comparisons.
set.seed(10101)
rate1 <- 1.0
rate2 <- 1.2
sample_sizes <- seq_len(100)
exact_probs <- sapply(sample_sizes, function(n) {ExactProb(n, rate2, rate1)})
emp_probs <- sapply(sample_sizes, function(n) {GammaDiffSim(n, rate1, rate2)})

# Compare empirical and exact probabilities.
fit <- lm(emp_probs ~ 0 + exact_probs)
slope <- coef(fit)

df <- data.frame(emp = emp_probs, exact = exact_probs)
q <- ggplot2::ggplot(data = df) +
  ggplot2::theme_bw() + 
  ggplot2::stat_smooth(
    aes(x = exact, y = emp),
    linetype = "dashed",
    formula = "y ~ x",
    color = "gray",
    method = "lm",
    se = FALSE
  ) + 
  ggplot2::geom_point(
    aes(x = exact, y = emp),
    color = "royalblue"
  ) +
  ggplot2::labs(
    x = "Exact Probability",
    y = "Empirical Probability"
  ) + 
  ggplot2::ggtitle(
    "Empirical vs. Exact Prob Gamma(n, rate1) < Gamma(n, rate2)"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 0.15,
    y = 0.4,
    label = sprintf("Slope: %.3f", slope)
  )
show(q)

ggplot2::ggsave(
  filename = "Figures/gamma_order_prob.png",
  plot = q,
  width = 7.0,
  height = 3.5,
  units = "in",
  dpi = 480
)
