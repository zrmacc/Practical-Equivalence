# Practical Equivalence Design

Zachary McCaw <br>
Updated: 2021-12-04



### Description

This package performs sample size estimation for designing a practical equivalence trial with a time to event endpoint. The sample size calculation is based on a Weibull distribution for the time to event in each treatment arm. The Weibull distribution is parameterized in terms of the shape parameter $\alpha$ and the rate parameter $\lambda$:

$$
f(t) = \alpha\lambda (\lambda t)^{\alpha - 1}e^{-(\lambda t)^{\alpha}}.
$$

The hypothesized time to event distribution for each treatment arm may be specified by providing the shape and rate parameters, or by providing the median. If the median is provided, then $\alpha$ is taken as 1, which corresponds to an exponential distribution.

## Installation


```r
devtools::install_github(repo = 'zrmacc/Practical-Equivalence/PracticalEquiDesign')
```


## Examples
Sample size calculation for a median survival time of 6 months in the reference arm and 8 months in the treatment arm, with an estimated censoring rate of 20% and a 80% chance of selecting the more-effective treatment.


```r
set.seed(101)
library(PracticalEquiDesign)
n <- SampleSize(
  med1 = 6,
  med2 = 8,
  cens_prop = 0.2,
  target_prob = 0.8
)
sprintf("Sample size: %d.", n)
```

```
## [1] "Sample size: 23."
```

Probability of selecting the more-effective treatment at the recommended sample size.


```r
set.seed(101)
prob <- SupProb(
  n = n,
  med1 = 6,
  med2 = 8,
  cens_prop = 0.2
)
sprintf("Probability of selecting the more-effective treatment: %.3f.", prob)
```

```
## [1] "Probability of selecting the more-effective treatment: 0.805."
```

Selection probability as a function of the sample size:


```r
set.seed(101)
q <- ProbCurve(
  med1 = 6,
  med2 = 8,
  cens_prop = 0.2,
  delta = 5,
  min_n = 10,
  max_n = 100,
  target_prob = 0.8
)
show(q)
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

By default, the margin of equivalence is zero. The following example specifies a practical equivalence margin of 1 month. 


```r
set.seed(101)
n <- SampleSize(
  med1 = 6,
  med2 = 8,
  cens_prop = 0.2,
  margin = 1,
  target_prob = 0.8
)
sprintf("Sample size: %d.", n)
```

```
## [1] "Sample size: 28."
```

## Weibull Specification

Given the survival probabilities at two distinct time points, the function `WeibullSpec` determines the shape and rate parameter of the corresponding Weibull distribution. For example, suppose the anticipated survival at `t1 = 6` months is 80\% and that at `t2 = 12` months is 50\%. The corresponding Weibull distribution has shape and rate:

```r
theta <- WeibullSpec(t1 = 6, p1 = 0.8, t2 = 12, p2 = 0.5)
show(round(theta, digits = 3))
```

```
## shape  rate 
## 1.635 0.067
```
