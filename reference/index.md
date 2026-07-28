# Package index

## Estimating age

Estimate dental age from scored teeth, and interpret the result.

- [`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md)
  : Estimate dental age
- [`print(`*`<dental_age>`*`)`](https://mu-cgcs.github.io/AgeFromDentition/reference/print.dental_age.md)
  : Print a dental age estimate
- [`ac_info()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_info.md)
  : Terminal-stage information from a dental age estimate

## Terminal stages

Teeth at apex closure carry no finite age in stage. They are reported as
a percentile of the reference distribution of age at completion.

- [`ac_completion_threshold()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_completion_threshold.md)
  : Reference completion threshold for terminal (Ac) teeth

## Intervals and sampling

Stochastic summaries of the fitted distribution. Neither is used to
judge compatibility with a completion threshold.

- [`estimate_age_hdi()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_age_hdi.md)
  : Estimate HDI from dental age estimate
- [`age_samples()`](https://mu-cgcs.github.io/AgeFromDentition/reference/age_samples.md)
  : Generate sample for age estimate

## Preparing scores

- [`get_means_for_scores()`](https://mu-cgcs.github.io/AgeFromDentition/reference/get_means_for_scores.md)
  : Lookup mean and sd for a sex / tooth / stage combination
- [`recode_score()`](https://mu-cgcs.github.io/AgeFromDentition/reference/recode_score.md)
  : Recode scores
- [`score_to_stage()`](https://mu-cgcs.github.io/AgeFromDentition/reference/score_to_stage.md)
  : Convert stage numeric score to character score
- [`validate_score()`](https://mu-cgcs.github.io/AgeFromDentition/reference/validate_score.md)
  : Validate stage scores

## Reference data

Sex-specific values from Šešelj et al. (2019). Note that `log_mu` means
age *given* the stage in `AgeTables`, and age at *entering* the stage in
`AttainmentTables`.

- [`AgeTables`](https://mu-cgcs.github.io/AgeFromDentition/reference/AgeTables.md)
  : Age given stage: log-normal parameters for dental stages
- [`AttainmentTables`](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md)
  : Age of attainment: log-normal transition parameters
- [`StageTies`](https://mu-cgcs.github.io/AgeFromDentition/reference/StageTies.md)
  : Tied stage transitions
- [`ExampleScores`](https://mu-cgcs.github.io/AgeFromDentition/reference/ExampleScores.md)
  : Example dental scores
