# Generate sample for age estimate

Generate sample for age estimate

## Usage

``` r
age_samples(age_est, n)
```

## Arguments

- age_est:

  Result of call to estimate_dental_age()

- n:

  Number of samples

## Value

n samples, or `n` `NA`s when the estimate has no finite value

## Details

This function is **stochastic**: it draws from the fitted log-normal and
is not seeded, so results vary between runs. Call
[`set.seed()`](https://rdrr.io/r/base/Random.html) first if you need
reproducible output.

When `age_est` has no point estimate – every scored tooth at terminal
stage `"Ac"`, for instance – there is no distribution to draw from and
`n` `NA`s are returned.

## See also

[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md),
[`estimate_age_hdi()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_age_hdi.md)
