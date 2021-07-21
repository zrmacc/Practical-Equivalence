# Purpose: Prepare simulation configuration.
# Updated: 2021-07-20

# -----------------------------------------------------------------------------
# Equivalence probability simulations.
# -----------------------------------------------------------------------------

# Shape.
shape1 <- 1.0
shape2 <- 1.0

# Rate.
rate1 <- c(1.0, 1.5, 2.0)
rate2 <- 1.0

# Censoring proportion.
cens <- c(0, 0.25)

# Margin.
marg <- c(0, 0.25)

# Configuration frame.
config <- expand.grid(
  shape1 = shape1,
  shape2 = shape2,
  rate1 = rate1,
  rate2 = rate2,
  cens = cens,
  marg = marg
)

data.table::fwrite(
  x = config,
  file = "Configs/EquivProb.txt",
  sep = "\t"
)

