# Purpose: Assess calibration of the analytical ordering probability
# when using the asymptotic calculation.
# Updated: 2021-05-16

library(dplyr)
library(ggplot2)
library(PracticalEquiDesign)
source("Rscripts/Data Generation.R")

# -----------------------------------------------------------------------------
# Simulation to compare empirical and analytical rate ordering probability.
# -----------------------------------------------------------------------------

# Parameters.
set.seed(10101)
sample_sizes <- seq(from = 5, to = 50, by = 5)
censor_prop <- 0.80
rate1 <- 1.0
rate2 <- 1.2
mc_iter <- 1e3

# Probabilities (slow).
emp_probs <- lapply(sample_sizes, function(n) {
  EmpOrderProb(
    n = n, 
    rate1 = rate1, 
    rate2 = rate2, 
    calc_ci = TRUE, 
    censor_prop = censor_prop, 
    mc_iter = mc_iter)
})
df <- do.call(rbind, emp_probs)
df$n <- sample_sizes
analytical_probs <- sapply(sample_sizes, function(n) {
  OrderProb(
    n = n,
    rate1 = rate1,
    rate2 = rate2,
    censor_prop = censor_prop
  )
})
df$analytical <- analytical_probs

# -----------------------------------------------------------------------------

# Slope.
fit <- lm(prob ~ 0 + analytical, data = df)
slope <- coef(fit)

# Calibration figure.
q <- ggplot2::ggplot(df) +
  ggplot2::theme_bw() + 
  ggplot2::stat_smooth(
    aes(x = analytical, y = prob),
    linetype = "dashed",
    formula = "y ~ x",
    color = "gray",
    method = "lm",
    se = FALSE
  ) + 
  ggplot2::geom_linerange(
    aes(x = analytical, ymin = lower, ymax = upper),
    color = "gray"
  ) +
  ggplot2::geom_point(
    aes(x = analytical, y = prob),
    color = "royalblue"
  ) +
  ggplot2::labs(
    x = "Analytical Probability",
    y = "Empirical Probability"
  ) + 
  ggplot2::ggtitle(
    expression(Empirial~vs.~Analytical~Prob~hat(lambda)[1]<hat(lambda)[2])
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 0.65,
    y = 0.75,
    label = sprintf("Slope: %.3f", slope)
  )
show(q)

ggplot2::ggsave(
  filename = "Figures/calibration.png",
  plot = q,
  width = 7.0,
  height = 3.5,
  units = "in",
  dpi = 480
)

# -----------------------------------------------------------------------------

# Prob by sample size figure.
df2 <- df %>%
  dplyr::select(n, prob, analytical) %>%
  dplyr::rename(empirical = prob) %>%
  tidyr::pivot_longer(
    cols = empirical:analytical,
    names_to = "method",
    values_to = "prob"
  )

q <- ggplot2::ggplot(df2) +
  ggplot2::theme_bw() + 
  ggplot2::geom_linerange(
    data = df,
    aes(x = n, ymin = lower, ymax = upper),
    color = "gray"
  ) +
  ggplot2::geom_line(
    aes(x = n, y = prob, color = method),
    size = 1
  ) + 
  ggplot2::geom_point(
    aes(x = n, y = prob, color = method),
  ) + 
  ggplot2::labs(
    x = "Sample Size",
    y = "Probability"
  ) + 
  ggplot2::lims(
    y = c(0.0, 1.0)
  ) +
  ggplot2::ggtitle(
    expression(Empirical~and~Analytical~Prob~hat(lambda)[1]<hat(lambda)[2]~by~Sample~Size)
  ) + 
  ggplot2::scale_color_manual(
    name = NULL,
    labels = c("Analytical", "Empirical"),
    values = c("#C65842", "#6385B8")
  ) +
  ggplot2::theme(
    legend.position = c(0.15, 0.3)
  )
show(q)

ggplot2::ggsave(
  filename = "Figures/order_prob_by_sample_size.png",
  plot = q,
  width = 7.0,
  height = 3.5,
  units = "in",
  dpi = 480
)
