# Purpose: Prepare simulation configuration.
# Updated: 2021-08-26

# -----------------------------------------------------------------------------
# Equivalent Exponentials. 
# -----------------------------------------------------------------------------

# Shape.
shape1 <- 1.0
shape2 <- 1.0

# Rate.
rate1 <- log(2) / 6
rate2 <- log(2) / 6

# Censoring proportion.
cens <- c(0, 0.2)

# Margin.
marg <- c(0, 2)

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
  file = "Configs/exp_diff_0.txt",
  sep = "\t"
)

# -----------------------------------------------------------------------------
# Equivalent Weibulls. 
# -----------------------------------------------------------------------------

# Shape.
shape1 <- 2.0
shape2 <- 2.0

# Rate.
rate1 <- sqrt(log(2)) / 6
rate2 <-  sqrt(log(2)) / 6

# Censoring proportion.
cens <- c(0, 0.2)

# Margin.
marg <- c(0, 2)

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
  file = "Configs/wei_diff_0.txt",
  sep = "\t"
)


# -----------------------------------------------------------------------------
# Exponentials: 1 month difference.
# -----------------------------------------------------------------------------

# Shape.
shape1 <- 1.0
shape2 <- 1.0

# Rate.
rate1 <- log(2) / 6
rate2 <- log(2) / 7

# Censoring proportion.
cens <- c(0, 0.2)

# Margin.
marg <- c(0, 2)

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
  file = "Configs/exp_diff_1.txt",
  sep = "\t"
)


# -----------------------------------------------------------------------------
# Weibulls: 1 month difference
# -----------------------------------------------------------------------------

# Shape.
shape1 <- 2.0
shape2 <- 2.0

# Rate.
rate1 <- sqrt(log(2)) / 6
rate2 <-  sqrt(log(2)) / 7

# Censoring proportion.
cens <- c(0, 0.2)

# Margin.
marg <- c(0, 2)

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
  file = "Configs/wei_diff_1.txt",
  sep = "\t"
)


# -----------------------------------------------------------------------------
# Exponentials: 3 month difference.
# -----------------------------------------------------------------------------

# Shape.
shape1 <- 1.0
shape2 <- 1.0

# Rate.
rate1 <- log(2) / 6
rate2 <- log(2) / 9

# Censoring proportion.
cens <- c(0, 0.2)

# Margin.
marg <- c(0, 2)

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
  file = "Configs/exp_diff_3.txt",
  sep = "\t"
)


# -----------------------------------------------------------------------------
# Weibulls: 3 month difference
# -----------------------------------------------------------------------------

# Shape.
shape1 <- 2.0
shape2 <- 2.0

# Rate.
rate1 <- sqrt(log(2)) / 6
rate2 <-  sqrt(log(2)) / 9

# Censoring proportion.
cens <- c(0, 0.2)

# Margin.
marg <- c(0, 2)

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
  file = "Configs/wei_diff_3.txt",
  sep = "\t"
)
