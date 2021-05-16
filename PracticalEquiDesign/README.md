# Practical Equivalence Design

Zachary McCaw <br>
Updated: 2021-05-16



### Description

This package performs sample size estimation for designing a practical equivalence trial.

## Installation


```r
devtools::install_github(repo = 'zrmacc/Practical-Equivalence/PracticalEquiDesign')
```


## Examples
Sample size calculation for a median survival time of 8 months in the treatment arm and 6 months in the reference arm with an estimated censoring rate of 20% and a 90% chance of selecting the more-effective treatment.


```r
library(PracticalEquiDesign)
n <- SampleSize(
  med1 = 8,
  med2 = 6,
  censor_prop = 0.2,
  target_prob = 0.9
)
sprintf("Sample size: %d.", n)
```

```
## [1] "Sample size: 52."
```

Probability of selecting the more-effective treatment at the recommended sample size.


```r
prob <- OrderProb(
  n = n,
  med1 = 8,
  med2 = 6,
  censor_prop = 0.2
)
sprintf("Probability of selecting the more-effective treatment: %.3f.", prob)
```

```
## [1] "Probability of selecting the more-effective treatment: 0.901."
```


