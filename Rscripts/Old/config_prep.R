# Purpose: Prepare simulation configuration.
# Updated: 2021-05-27

# -----------------------------------------------------------------------------
# Testing config.
# -----------------------------------------------------------------------------

# Sample sizes.
n <- c(5, 10)

# Median in arm 1.
med1 <- 1.0

# Median in arm 2.
med2 <- 1.2

# Censoring proportion.
cens <- c(0, 0.25)

# Configuration frame.
config <- expand.grid(n, med1, med2, cens)
colnames(config) <- c("n", "med1", "med2", "cens")

data.table::fwrite(
  x = config,
  file = "Configs/Testing.txt",
  sep = "\t"
)

# -----------------------------------------------------------------------------
# Order probability simulations.
# -----------------------------------------------------------------------------

# Sample sizes.
n <- seq(from = 5, to = 50, by = 5)

# Median in arm 1.
med1 <- 1.0

# Median in arm 2.
med2 <- seq(from = 1.0, to = 2.0, by = 0.2)

# Censoring proportion.
cens <- seq(from = 0.0, to = 0.5, by = 0.1)

# Configuration frame.
config <- expand.grid(n, med1, med2, cens)
colnames(config) <- c("n", "med1", "med2", "cens")

data.table::fwrite(
  x = config,
  file = "Configs/OrderProp.txt",
  sep = "\t"
)
