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
  title <- bquote(
    Censor==.(df$cens_prop)*","
    ~Margin==.(df$margin)*","
    ~alpha[1]==.(df$shape1)*","
    ~lambda[1]==.(df$rate1)*","
    ~alpha[2]==.(df$shape2)*","
    ~lambda[2]==.(df$rate2)*"."
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
# Panel.
# -----------------------------------------------------------------------------

files <- file.path("Simulations/EquivProb", dir(path = "Simulations/EquivProb/"))
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
  byrow = FALSE,
  ncol = 3
)
cowplot::ggsave2(
  file = "Figures/weibull_equiv_prob_sim_01.png",
  plot = plot_panel,
  device = "png",
  width = 9,
  height = 6,
  units = "in",
  scale = 2,
  dpi = 480
)
