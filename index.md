# AgeFromDentition

The goal of `AgeFromDentition` is to estimate dental age from scores of
dental development.

## Installation

You can install the development version of AgeFromDentition from
[GitHub](https://github.com/) with:

``` r

# install.packages("remotes")
remotes::install_github("MU-CGCS/AgeFromDentition")
```

## Usage

Score six mandibular teeth and pass the row straight to the estimator:

``` r

library(AgeFromDentition)

estimate_dental_age(ExampleScores[1, ], verbose = FALSE)
#> Dental age: 9.04 years (mode of the fitted distribution).
#> Central 95% interval: 7.61 to 10.90 years.
#> Estimated from 5 teeth.
#> Not scored: M3.
```

## Terminal (Ac) stages

A tooth at `Ac` has completed. That tells you the individual is *older
than* the completion age, and nothing more — there is no finite age in
stage to invert, which is why Tables 8–13 of Šešelj et al. (2019) have
no `Ac` row.

Such teeth are excluded from the estimate and reported separately as a
**reference completion threshold**, derived from the ages of attainment
in Tables 2–7:

``` r

x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                M1 = "Ac", M2 = "Ac", M3 = NA)

estimate_dental_age(x, verbose = FALSE)
#> Dental age: no finite point estimate.
#> All scored teeth are at terminal stage Ac (Canine, P3, P4, M1, M2).
#> Reference completion threshold: 12.19 years - the 2.5th percentile of the sex-specific predictive distribution for attaining Ac in M2 (q = 0.025, method = "predictive").
#> This is a descriptive completion threshold for the reference sample, not a lower confidence limit for this individual.
#> Not scored: M3.
```

Note what this does **not** do: it does not return a point estimate, and
it does not return 18. The threshold is a percentile of the reference
sample’s completion ages — a descriptive statistic, not a lower
confidence limit for the individual.

An unscored M3 is never treated as a completed one. The two differ by
nearly two years here.

When some teeth are still developing, you get an estimate *and* a
threshold, plus a code saying whether they agree:

``` r

y <- data.frame(Sex = "F", Canine = "R.c", P3 = "R.c", P4 = "R.75",
                M1 = "Ac", M2 = "R.5", M3 = NA)

est <- estimate_dental_age(y, verbose = FALSE)
ac_info(est)$compatibility
#> [1] "compatible"
```

See
[`vignette("terminal-stages")`](https://mu-cgcs.github.io/AgeFromDentition/articles/terminal-stages.md)
for the full treatment, including the convention to declare in a methods
section.
