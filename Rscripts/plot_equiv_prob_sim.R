# Purpose: Simulation to validate the equivalence probability calculation.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

gg_opts <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

# -----------------------------------------------------------------------------
# Plotting function.
# -----------------------------------------------------------------------------

#' Create Plot Title
#' 
#' @param data Data.frame
#' @return String.

PlotTitle <- function(data) {
  df <- data %>%
    dplyr::select(cens_prop, shape1, shape2, rate1, rate2, margin) %>%
    dplyr::distinct()
  med1 <- PracticalEquiDesign::WeiMed(df$shape1, df$rate1)
  med2 <- PracticalEquiDesign::WeiMed(df$shape2, df$rate2)
  title <- bquote(
    Censor==.(df$cens_prop)*","
    ~Margin==.(df$margin)*","
    ~Med[1]==.(med1)*","
    ~Med[2]==.(med2)*"."
  )
  return(title)
}


#' Plot Equivalnce Probability Simulation
#' 
#' @param data Data.frame.
#' @param title Title.
#' @param ylim Y-axis limits.
#' @return ggplot2.

PlotEquiProbSim <- function(
  data, 
  title = NULL,
  y_lim = NULL
) {
  
  df <- data %>%
    dplyr::select(n, analytical, simulation) %>%
    tidyr::pivot_longer(
      cols = c("analytical", "simulation"),
      names_to = "Method",
      values_to = "y"
    )
  
  q <- ggplot2::ggplot() +
    gg_opts +
    ggplot2::geom_linerange(
      data = data,
      aes(x = n, ymin = sim_lower, ymax = sim_upper),
      color = "royalblue"
    ) + 
    ggplot2::geom_point(
      data = df,
      aes(x = n, y = y, color = Method)
    ) + 
    ggplot2::labs(
      x = "Sample Size",
      y = "Equivalence\nProbability",
      title = title
    ) + 
    ggplot2::scale_color_manual(
      values = c("red", "royalblue"),
      labels = c("Analytical", "Simuluation")
    ) + 
    ggplot2::scale_x_continuous(
      breaks = sort(unique(data$n))
    ) +
    ggplot2::scale_y_continuous(
      limits = y_lim
    )
  return(q)
}


# -----------------------------------------------------------------------------
# Exponential: No difference.
# -----------------------------------------------------------------------------

files <- file.path("Simulations/ExpNoDiff", dir(path = "Simulations/ExpNoDiff/"))

plot_list <- lapply(files, function(file) {
  data <- readRDS(file)
  if (is.null(data$n)) {data$n <- seq(from = 5, to = 100, by = 5)}
  q <- PlotEquiProbSim(
    data, 
    title = PlotTitle(data),
    y_lim = c(0, 1)
  )
  return(q)
})

plot_panel <- cowplot::plot_grid(
  plotlist = plot_list,
  byrow = TRUE,
  ncol = 2
)

cowplot::ggsave2(
  file = "Figures/exp_no_diff.png",
  plot = plot_panel,
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  scale = 2,
  dpi = 480
)


# -----------------------------------------------------------------------------
# Weibull: No difference.
# -----------------------------------------------------------------------------

files <- file.path("Simulations/WeiNoDiff", dir(path = "Simulations/WeiNoDiff/"))

plot_list <- lapply(files, function(file) {
  data <- readRDS(file)
  if (is.null(data$n)) {data$n <- seq(from = 5, to = 100, by = 5)}
  q <- PlotEquiProbSim(
    data, 
    title = PlotTitle(data),
    y_lim = c(0, 1)
  )
  return(q)
})

plot_panel <- cowplot::plot_grid(
  plotlist = plot_list,
  byrow = TRUE,
  ncol = 2
)

cowplot::ggsave2(
  file = "Figures/wei_no_diff.png",
  plot = plot_panel,
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  scale = 2,
  dpi = 480
)


# -----------------------------------------------------------------------------
# Exponential: 1 month difference.
# -----------------------------------------------------------------------------

files <- file.path("Simulations/ExpDiff1", dir(path = "Simulations/ExpDiff1/"))

plot_list <- lapply(files, function(file) {
  data <- readRDS(file)
  if (is.null(data$n)) {data$n <- seq(from = 5, to = 100, by = 5)}
  q <- PlotEquiProbSim(
    data, 
    title = PlotTitle(data),
    y_lim = c(0, 1)
  )
  return(q)
})

plot_panel <- cowplot::plot_grid(
  plotlist = plot_list,
  byrow = TRUE,
  ncol = 2
)

cowplot::ggsave2(
  file = "Figures/exp_diff_1.png",
  plot = plot_panel,
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  scale = 2,
  dpi = 480
)


# -----------------------------------------------------------------------------
# Weibull: 1 month difference.
# -----------------------------------------------------------------------------

files <- file.path("Simulations/WeiDiff1", dir(path = "Simulations/WeiDiff1/"))

plot_list <- lapply(files, function(file) {
  data <- readRDS(file)
  if (is.null(data$n)) {data$n <- seq(from = 5, to = 100, by = 5)}
  q <- PlotEquiProbSim(
    data, 
    title = PlotTitle(data),
    y_lim = c(0, 1)
  )
  return(q)
})

plot_panel <- cowplot::plot_grid(
  plotlist = plot_list,
  byrow = TRUE,
  ncol = 2
)

cowplot::ggsave2(
  file = "Figures/wei_diff_1.png",
  plot = plot_panel,
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  scale = 2,
  dpi = 480
)


# -----------------------------------------------------------------------------
# Exponential: 3 month difference.
# -----------------------------------------------------------------------------

files <- file.path("Simulations/ExpDiff3", dir(path = "Simulations/ExpDiff3/"))

plot_list <- lapply(files, function(file) {
  data <- readRDS(file)
  if (is.null(data$n)) {data$n <- seq(from = 5, to = 100, by = 5)}
  q <- PlotEquiProbSim(
    data, 
    title = PlotTitle(data),
    y_lim = c(0, 1)
  )
  return(q)
})

plot_panel <- cowplot::plot_grid(
  plotlist = plot_list,
  byrow = TRUE,
  ncol = 2
)

cowplot::ggsave2(
  file = "Figures/exp_diff_3.png",
  plot = plot_panel,
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  scale = 2,
  dpi = 480
)


# -----------------------------------------------------------------------------
# Weibull: 3 month difference.
# -----------------------------------------------------------------------------

files <- file.path("Simulations/WeiDiff3", dir(path = "Simulations/WeiDiff3/"))

plot_list <- lapply(files, function(file) {
  data <- readRDS(file)
  if (is.null(data$n)) {data$n <- seq(from = 5, to = 100, by = 5)}
  q <- PlotEquiProbSim(
    data, 
    title = PlotTitle(data),
    y_lim = c(0, 1)
  )
  return(q)
})

plot_panel <- cowplot::plot_grid(
  plotlist = plot_list,
  byrow = TRUE,
  ncol = 2
)

cowplot::ggsave2(
  file = "Figures/wei_diff_3.png",
  plot = plot_panel,
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  scale = 2,
  dpi = 480
)

