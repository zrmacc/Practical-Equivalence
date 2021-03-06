# Purpose: Simulation to validate the equivalence probability calculation.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

gg_opts <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#' Lambda given median and alpha
#' 
#' @param alpha Shape parameter.
#' @param mu Median. 
#' @return Numeric.

CalcRate <- function(alpha, mu) {
  rate <- (log(2)) ^ (1.0 / alpha) / mu
  return(rate)
}


#' Evaluate survival function
#' 
#' @param alpha Shape parameter.
#' @param lambda Rate parameter.
#' @param grid Evaluation grid.
#' @return Numeric.

EvalSurv <- function(alpha, lambda, grid) {
  surv <- Temporal::SurvFunc(dist = "weibull", theta = c(alpha, lambda))
  eval <- surv(grid)
  return(data.frame(grid = grid, surv = eval))
}


# -----------------------------------------------------------------------------
# Weibull example
# -----------------------------------------------------------------------------

median_ctrl <- 12
median_trt <- 16

alpha <- seq(from = 0.5, to = 2.5, by = 0.5)
lambda <- CalcRate(alpha, median_ctrl)
params <- data.frame(alpha = alpha, lambda = lambda)
grid <- seq(from = 0, to = 24, length.out = 500)

# Plotting frame.
df <- params %>%
  dplyr::group_by(alpha, lambda) %>%
  dplyr::summarise(
    EvalSurv(alpha, lambda, grid)
  ) %>%
  dplyr::ungroup()

# Plot.
q <- ggplot2::ggplot(data = df) + 
  gg_opts +
  ggplot2::geom_line(
    aes(x = grid, y = surv, color = alpha, group = alpha)
  ) + 
  ggplot2::scale_color_gradient(name = expression(alpha), low = "blue", high = "red") +
  ggplot2::scale_x_continuous(
    name = "Months",
    breaks = seq(from = 0, to = 24, by = 6)
  ) + 
  ggplot2::scale_y_continuous(
    name = "Survival",
    breaks = seq(from = 0, to = 1.0, by = 0.25)
  )

ggplot2::ggsave(
  file = "Figures/weibull_by_alpha.png",
  plot = q,
  device = "png",
  width = 7.5,
  height = 3.0,
  units = "in",
  dpi = 360
)

# -----------------------------------------------------------------------------

theta0 <- PracticalEquiDesign::WeibullSpec(6, 0.8, 12, 0.5)
theta1 <- PracticalEquiDesign::WeibullSpec(7.2, 0.8, 16, 0.5)

PracticalEquiDesign::SampleSize(
  cens_prop = 0.2,
  shape1 = theta0['shape'],
  rate1 = theta0['rate'],
  shape2 = theta1['shape'],
  rate2 = theta1['rate'],
  min_n = 5,
  max_n = 100,
  target_prob = 0.8,
  margin = 2
)

# -----------------------------------------------------------------------------

theta1 <- PracticalEquiDesign::WeibullSpec(6.0, 0.8, 16, 0.5)

# 2 month margin.
set.seed(101)
PracticalEquiDesign::SampleSize(
  cens_prop = 0.2,
  shape1 = theta0['shape'],
  rate1 = theta0['rate'],
  shape2 = theta1['shape'],
  rate2 = theta1['rate'],
  min_n = 5,
  max_n = 100,
  target_prob = 0.8,
  margin = 2,
  info_reps = 200
)

# -----------------------------------------------------------------------------

theta1 <- PracticalEquiDesign::WeibullSpec(6.0, 0.8, 16, 0.5)

# 1 month margin.
set.seed(101)
PracticalEquiDesign::SampleSize(
  cens_prop = 0.2,
  shape1 = theta0['shape'],
  rate1 = theta0['rate'],
  shape2 = theta1['shape'],
  rate2 = theta1['rate'],
  min_n = 5,
  max_n = 100,
  target_prob = 0.8,
  margin = 1,
  info_reps = 200
)

# 3 month margin.
set.seed(101)
PracticalEquiDesign::SampleSize(
  cens_prop = 0.2,
  shape1 = theta0['shape'],
  rate1 = theta0['rate'],
  shape2 = theta1['shape'],
  rate2 = theta1['rate'],
  min_n = 5,
  max_n = 100,
  target_prob = 0.8,
  margin = 3,
  info_reps = 200
)
