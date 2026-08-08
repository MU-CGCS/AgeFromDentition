# Estimate dental age

Estimate dental age from a set of scored teeth, following the method
described in Šešelj M, Sherwood RJ, Konigsberg LW. 2019. Timing of
Development of the Permanent Mandibular Dentition: New Reference Values
from the Fels Longitudinal Study. Anat Rec 302:1733-1753.

## Usage

``` r
estimate_dental_age(
  scores,
  q = 0.025,
  method = c("predictive", "plugin"),
  sex = NULL,
  verbose = TRUE
)
```

## Arguments

- scores:

  a scored row (a data frame with `Sex` and one column per tooth), or a
  6 x 2 data frame of means and standard deviations as returned by
  [`get_means_for_scores()`](https://mu-cgcs.github.io/AgeFromDentition/reference/get_means_for_scores.md).

- q:

  lower-tail probability for the completion threshold. Defaults to
  `0.025`.

- method:

  `"predictive"` (default) or `"plugin"`; passed to
  [`ac_completion_threshold()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_completion_threshold.md).

- sex:

  `"F"` or `"M"`. Only needed when `scores` is a bare means data frame,
  which carries no sex of its own.

- verbose:

  boolean flag for printing diagnostic messages.

## Value

A named numeric vector of length 5 with class `dental_age`: `log_age`,
`log_total_var`, `dental_age`, `ci_lower`, `ci_upper`. All five are
`NA_real_` when no tooth supplies a usable estimate. Terminal-stage
information is attached as an attribute; retrieve it with
[`ac_info()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_info.md).

## Details

Teeth at the terminal stage `"Ac"` are excluded from the estimate and
reported separately as a completion threshold. See Details.

## Terminal stages

A tooth at `"Ac"` has completed, which says only that the individual is
older than the completion age. It carries no finite age in stage and is
therefore excluded from the weighted estimate. Instead, the `Ac` teeth
produce a **reference completion threshold** via
[`ac_completion_threshold()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_completion_threshold.md),
reported alongside the estimate.

When every scored tooth is at `"Ac"`, there is no point estimate at all
and the threshold is the only result. The numeric fields are `NA_real_`.
Returning a fixed age in that situation – 18, say – would impose a
hidden prior and map very different completion patterns onto one number.

## The interval

`ci_lower` and `ci_upper` are an equal-tailed **central 95% interval**,
computed analytically with
[`stats::qlnorm()`](https://rdrr.io/r/stats/Lognormal.html). It is not a
highest-density interval: equal-tailed log-normal quantiles are not a
highest-density region, because the log-normal is asymmetric on the age
scale. For an HDI, use
[`estimate_age_hdi()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_age_hdi.md),
which is a separate, Monte Carlo, function.

The compatibility code below is defined from this interval only, so that
it is deterministic and reproducible.

## Compatibility codes

- no_terminal_information:

  no tooth at `"Ac"`; an ordinary estimate

- completion_threshold_only:

  teeth at `"Ac"` but none estimable; no finite age estimate

- compatible:

  `ci_lower >= threshold`

- overlap:

  the threshold falls inside the interval

- discordant:

  `ci_upper < threshold`; raises a warning

`discordant` does not necessarily indicate a scoring error. The
age-given-stage estimator and the attainment threshold come from related
but different models, combined here deliberately approximately, so
disagreement can also reflect reference-sample variation.

## See also

[`ac_completion_threshold()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_completion_threshold.md),
[`ac_info()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_info.md),
[`estimate_age_hdi()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_age_hdi.md)

## Examples

``` r
# An ordinary estimate
estimate_dental_age(ExampleScores[1, ])
#> Dental age: 9.04 years (mode of the fitted distribution).
#> Central 95% interval: 7.61 to 10.90 years.
#> Estimated from 5 teeth.
#> Not scored: M3.

# Every scored tooth complete: a threshold, and no point estimate
x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                M1 = "Ac", M2 = "Ac", M3 = NA)
estimate_dental_age(x)
#> 5 teeth at terminal stage Ac (Canine, P3, P4, M1, M2); excluded from the age
#> estimate and used for the completion threshold.
#> Dental age: 14.78 years (mode of the M2 Ac attainment distribution).
#> Central 95% interval: 12.19 to 18.29 years.
#> All scored teeth are at terminal stage Ac (Canine, P3, P4, M1, M2).
#> Estimated from the M2 Ac attainment distribution, not the age-given-stage model.
#> Reference completion threshold: 12.19 years - the 2.5th percentile of the sex-specific predictive distribution for attaining Ac in M2 (q = 0.025, method = "predictive").
#> This is a descriptive completion threshold for the reference sample, not a lower confidence limit for this individual.
#> Compatibility: compatible - the interval lies at or above the completion threshold.
#> Not scored: M3.
```
