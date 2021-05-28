# Purpose: Simulation to calculate the probability the treatments are correctly
# ordered.

library(optparse)
source("Rscripts/data_gen.R")
source("Rscripts/empirical_order_prob.R")

# -----------------------------------------------------------------------------
# Unpack simulation settings.
# -----------------------------------------------------------------------------

# Command line options.
opt_list <- list()

# Sample size.
opt <- make_option(c("--n"), type = "integer", help = "Sample size", default = 10)
opt_list <- c(opt_list, opt)

# Median in arm 1.
opt <- make_option(c("--med1"), type = "numeric", help = "Median 1", default = 1.0)
opt_list <- c(opt_list, opt)

# Median in arm 2.
opt <- make_option(c("--med2"), type = "numeric", help = "Median 2", default = 1.2)
opt_list <- c(opt_list, opt)

# Censoring rate.
opt <- make_option(c("--cens"), type = "numeric", help = "Censoring rate", default = 0.0)
opt_list <- c(opt_list, opt)

# Simulation replicates.
opt <- make_option(c("--reps"), type = "integer", help = "Replicates", default = 1e4)
opt_list <- c(opt_list, opt)

# Output directory.
opt <- make_option(c("--out"), type = "character", help = "Output stem", default = "Simulations/")
opt_list <- c(opt_list, opt)

# Option parsing.
t0 <- proc.time()
parsed_opts <- OptionParser(option_list = opt_list)
params <- parse_args(object = parsed_opts)

# Output stem.
file_id <- paste0(
  "N", params$n,
  "_M1_", params$med1, 
  "_M2_", params$med2,
  "_C", params$cens,
  ".rds"
)

# Report.
cat("Starting order probability simulation with these settings:\n")
cat("N: ", params$n, ",\t", sep = "")
cat("Median 1: ", params$med1, ",\t", sep = "")
cat("Median 2: ", params$med2, ",\t", sep = "")
cat("Censoring: ", params$cens, ".\t", sep = "")
cat("\n")

# -----------------------------------------------------------------------------
# Simulation.
# -----------------------------------------------------------------------------

# Convert medians to rates.
rates <- log(2) / c(params$med1, params$med2)
rate1 <- min(rates)
rate2 <- max(rates)

# Simulation.
sim <- EmpOrderProb(
  n = params$n,
  rate1 = rate1,
  rate2 = rate2,
  censor_prop = params$cens,
  calc_ci = TRUE,
  mc_iter = params$reps
)

# -----------------------------------------------------------------------------
# Output.
# -----------------------------------------------------------------------------

# Output.
out <- data.frame(
  n = params$n,
  med1 = params$med1,
  med2 = params$med2,
  cens = params$cens,
  reps = params$reps,
  sim
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