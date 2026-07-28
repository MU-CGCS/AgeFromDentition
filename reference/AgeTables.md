# Age given stage: log-normal parameters for dental stages

Sex-specific age-given-stage reference values from Tables 8-13 of Šešelj
et al. (2019). Each row gives the log-normal approximation to the
distribution of age *conditional on observing a tooth in a stage*.

## Usage

``` r
AgeTables
```

## Format

A data frame with 150 rows and 12 variables:

- Sex:

  character of sex: "F" or "M"

- Tooth:

  character ID of tooth: "Canine", "P3", "P4", "M1", "M2", or "M3"

- Stage:

  character for tooth stage

- log_mu:

  numeric mean log age given the stage, on the log scale

- log_sd:

  numeric log-scale standard deviation

- mode:

  numeric mode of the fitted distribution, in years

- median:

  numeric median of the fitted distribution, in years

- mean:

  numeric mean of the fitted distribution, in years

- sd:

  numeric standard deviation of the fitted distribution, in years

- hpd_low:

  numeric lower bound of the published 95% highest posterior density
  interval, in years

- opt:

  numeric published optimal age estimate for the stage, in years

- hpd_high:

  numeric upper bound of the published 95% highest posterior density
  interval, in years

## Source

Šešelj M, Sherwood RJ, Konigsberg LW. 2019. Timing of Development of the
Permanent Mandibular Dentition: New Reference Values from the Fels
Longitudinal Study. Anat Rec 302:1733-1753. Tables 8-13, panel (a).

## Details

Mind the mixed scales. `log_mu` and `log_sd` are on the log scale; every
other numeric column is in years. The summary columns are retained under
the names Šešelj et al. print, so a row can be checked against Tables
8a-13a directly.

The summary columns are redundant with `log_mu` and `log_sd` for most
rows – `median = exp(log_mu)`, `mode = exp(log_mu - log_sd^2)`, and so
on, and the build script verifies exactly that. They are shipped because
of the rows where they are *not* redundant; see below.

Note the contrast with
[AttainmentTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md):
here `log_mu` is the mean log age *given* that the tooth is observed in
the stage, whereas in `AttainmentTables` it is the mean log age at
*entering* the stage. The two are different quantities and are not
interchangeable.

There is deliberately no `Ac` row. Apex closure is a terminal, absorbing
stage: once a tooth reaches it, the observation says only that the
individual is older than the completion age, so there is no finite mean
age in stage to report. Use
[AttainmentTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md)
for `Ac`.

## Rows without log-normal parameters

Four rows have `NA` for `log_mu` and `log_sd`, all of them female M3,
and they are not the same kind of gap:

- `R.5` and `A.5`:

  zero-width stages. The fitted transition ages are tied with the
  following stage (see
  [StageTies](https://mu-cgcs.github.io/AgeFromDentition/reference/StageTies.md)),
  so the stage has no width and nothing can be estimated. Every column
  is `NA`.

- `R.75` and `R.c`:

  Šešelj et al. publish no log-normal fit, but they do publish an
  interval: `hpd_low`, `opt`, and `hpd_high` are populated. Those
  intervals are wide – 13.73 to 21.09 years and 14.28 to 21.92 years
  respectively – which is itself informative about late M3 development.

A tooth scored into any of these four rows still contributes nothing to
[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md),
which works from `log_mu` and `log_sd`. The two HPD-only rows are
shipped so that the published information is at least reachable rather
than silently absent.

## See also

[AttainmentTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md),
[StageTies](https://mu-cgcs.github.io/AgeFromDentition/reference/StageTies.md)
