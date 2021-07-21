# Purpose: Simulation to validate the equivalence probability calculation.
suppressPackageStartupMessages({
  library(dplyr)
  library(optparse)
  source("Rscripts/empirical_equiv_prob.R")
})

# -----------------------------------------------------------------------------
# Unpack simulation settings.
# -----------------------------------------------------------------------------

# Command line options.
opt_list <- list()

# Arm 1 parameters.
opt <- make_option(c("--shape1"), type = "numeric", help = "Shape 1", default = 1.0)
opt_list <- c(opt_list, opt)
opt <- make_option(c("--rate1"), type = "numeric", help = "Rate 1", default = 1.0)
opt_list <- c(opt_list, opt)

# Arm 2 parameters.
opt <- make_option(c("--shape2"), type = "numeric", help = "Shape 2", default = 1.0)
opt_list <- c(opt_list, opt)
opt <- make_option(c("--rate2"), type = "numeric", help = "Rate 2", default = 1.0)
opt_list <- c(opt_list, opt)

# Censoring rate.
opt <- make_option(c("--cens"), type = "numeric", help = "Censoring rate", default = 0.0)
opt_list <- c(opt_list, opt)

# Margin.
opt <- make_option(c("--marg"), type = "numeric", help = "Margin", default = 0.0)
opt_list <- c(opt_list, opt)

# Simulation replicates.
opt <- make_option(c("--reps"), type = "integer", help = "Replicates", default = 1e3)
opt_list <- c(opt_list, opt)

# Output directory.
opt <- make_option(c("--out"), type = "character", help = "Output stem", default = "Simulations")
opt_list <- c(opt_list, opt)

# Option parsing.
t0 <- proc.time()
parsed_opts <- OptionParser(option_list = opt_list)
params <- parse_args(object = parsed_opts)

# Output stem.
file_id <- paste0(
  "Shape1_", params$shape1, 
  "_Rate1_", params$rate1,
  "_Shape2_", params$shape2, 
  "_Rate2_", params$rate2,
  "_Cens_", params$cens,
  "_Marg_", params$marg,
  ".rds"
)

# Report.
cat("\n\n")
cat("Starting equivalence probability simulation with these settings:\n")
cat("Shape 1: ", params$shape1, ",\t", sep = "")
cat("Rate 1: ", params$rate1, ",\t", sep = "")
cat("Shape 2: ", params$shape2, ",\t", sep = "")
cat("Rate 2: ", params$rate2, ",\t", sep = "")
cat("Censoring: ", params$cens, ",\t", sep = "")
cat("Margin: ", params$marg, ".\t", sep = "")
cat("\n")

# -----------------------------------------------------------------------------
# Simulation.
# -----------------------------------------------------------------------------

# Power curve.
n_seq <- seq(from = 5, to = 100, by = 5)
power <- lapply(n_seq, function(n) {
  
  # Analyitcal calculation.
  analytical <- PracticalEquiDesign::EquiProb(
    cens_prop = params$cens,
    n = n,
    shape1 = params$shape1,
    rate1 = params$rate1,
    shape2 = params$shape2,
    rate2 = params$rate2,
    margin = params$marg,
    reps = 50
  )
  
  # Simulation.
  sim <- EmpEquiProb(
    cens_prop = params$cens,
    n = n,
    shape1 = params$shape1,
    rate1 = params$rate1,
    shape2 = params$shape2,
    rate2 = params$rate2,
    margin = params$marg,
    reps = params$reps
  )
  
  # Output.
  out <- data.frame(n = n, analytical = analytical, simulation = sim)
  return(out)
})

power <- do.call(rbind, power)

# -----------------------------------------------------------------------------
# Output.
# -----------------------------------------------------------------------------

# Output.
z <- stats::qnorm(0.975)
out <- power %>%
  dplyr::mutate(
    sim_se = sqrt(simulation * (1 - simulation) / params$reps),
    sim_lower = simulation - z * sim_se,
    sim_upper = simulation + z * sim_se,
    cens_prop = params$cens,
    shape1 = params$shape1,
    rate1 = params$rate1,
    shape2 = params$shape2,
    rate2 = params$rate2,
    margin = params$marg,
    reps = params$reps
  )

# Export.
out_stem <- params$out
if (!dir.exists(out_stem)) {dir.create(out_stem, recursive = TRUE)}
out_file <- file.path(out_stem, file_id)
saveRDS(object = out, file = out_file)

# -----------------------------------------------------------------------------
# End
# -----------------------------------------------------------------------------

# Report. 
t1 <- proc.time()
elapsed <- t1-t0
cat("Time elapsed: ", elapsed["elapsed"], "sec.\n\n")