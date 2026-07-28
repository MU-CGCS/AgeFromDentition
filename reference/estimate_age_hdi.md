# Estimate HDI from dental age estimate

Estimate HDI from dental age estimate

## Usage

``` r
estimate_age_hdi(age_est, n = 1e+05, interval = 0.5)
```

## Arguments

- age_est:

  numeric vector output from
  [`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md).

- n:

  numeric number of samples to produce. Defaults to 1e5 for speed.
  Increase this number for investigating the tails of the distribution.

- interval:

  numeric width of the HD interval. Defaults to 0.5

## Value

numeric vector with the lower and upper bounds of the interval, and the
completion threshold from `age_est` if it had one

## Details

This is a genuine highest-density interval, obtained by sampling. It is
therefore **stochastic and unseeded**: two calls on the same input give
slightly different bounds. Call
[`set.seed()`](https://rdrr.io/r/base/Random.html) first for
reproducible output, and raise `n` when the tails matter.

Because of that, it is never used to decide whether an estimate is
compatible with a completion threshold.
[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md)
computes its own equal-tailed central interval analytically for that
purpose, so the compatibility code cannot vary between runs.

`completion_threshold` is carried through unchanged so that a caller
plotting the interval can draw the threshold on the same axis.

## See also

[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md),
[`age_samples()`](https://mu-cgcs.github.io/AgeFromDentition/reference/age_samples.md)

## Examples

``` r
age_est <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)
estimate_age_hdi(age_est)
#>          lower_bound          upper_bound completion_threshold 
#>             8.504242             9.626240                   NA 
```
