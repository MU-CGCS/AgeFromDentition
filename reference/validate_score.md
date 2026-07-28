# Validate stage scores

Check stage scores against the reference vocabulary, optionally against
the stages that exist for a particular tooth.

## Usage

``` r
validate_score(x, tooth = NULL)
```

## Arguments

- x:

  character vector of stage scores to check. `NA` is treated as valid:
  it means the tooth was not scored.

- tooth:

  optional character vector of tooth names, either length 1 or the same
  length as `x`. When supplied, each score is checked against the stages
  that exist for that tooth rather than against the whole vocabulary.

## Value

logical vector the same length as `x`

## Details

The vocabulary comes from
[AttainmentTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md),
which is the only reference dataset covering all fourteen stages:
[AgeTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AgeTables.md)
omits `"Ac"` because apex closure is terminal and has no finite age in
stage.

Supplying `tooth` matters more than it looks. Not every stage exists for
every tooth: the single-rooted teeth (Canine, P3, P4) have no `"Cl.i"`
(root cleft initiation). Checked against the global vocabulary alone,
`"Cl.i"` on a canine is accepted, matches no reference row, and the
tooth is then silently dropped from the age estimate. The tooth-aware
check catches it instead.

## See also

[`score_to_stage()`](https://mu-cgcs.github.io/AgeFromDentition/reference/score_to_stage.md),
[`recode_score()`](https://mu-cgcs.github.io/AgeFromDentition/reference/recode_score.md)
