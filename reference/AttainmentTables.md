# Age of attainment: log-normal transition parameters

Sex-specific ages of attainment from Tables 2-7 of Šešelj et al. (2019),
estimated by transition analysis. Each row describes the distribution of
the age at which a tooth *enters* a stage.

## Usage

``` r
AttainmentTables
```

## Format

A data frame with 162 rows and 7 variables:

- Sex:

  character of sex: "F" or "M"

- Tooth:

  character ID of tooth: "Canine", "P3", "P4", "M1", "M2", or "M3"

- Stage:

  character for tooth stage

- log_mu:

  numeric mean log age at entering the stage

- log_sd:

  numeric log-scale standard deviation, common to all stages within a
  sex and tooth

- se_log_mu:

  numeric standard error of `log_mu`

- n:

  integer number of individuals contributing to the transition

## Source

Šešelj M, Sherwood RJ, Konigsberg LW. 2019. Timing of Development of the
Permanent Mandibular Dentition: New Reference Values from the Fels
Longitudinal Study. Anat Rec 302:1733-1753. Tables 2-7, panel (a).

## Details

This table is a strict superset of
[AgeTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AgeTables.md):
the same sex, tooth, and stage combinations, plus twelve `Ac` (apex
closure) rows that `AgeTables` cannot represent. It is therefore the
source for anything involving the terminal stage.

`log_mu` here is **not** comparable to `log_mu` in
[AgeTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AgeTables.md).
This is the age at *entering* a stage; that is the age *given* the
stage.

`se_log_mu` varies by more than an order of magnitude across rows and
should not be ignored. The two M3 `Ac` transitions rest on n = 1
(female) and n = 5 (male) individuals and have `se_log_mu` of 0.0638 and
0.0482, against 0.011-0.029 for the well-estimated transitions.

`Stage` uses the full 14-value vocabulary, but not every stage exists
for every tooth: the single-rooted teeth (Canine, P3, P4) have no `Cl.i`
(root cleft initiation) row. Check tooth and stage together, not stage
alone.

## See also

[AgeTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AgeTables.md),
[StageTies](https://mu-cgcs.github.io/AgeFromDentition/reference/StageTies.md)
