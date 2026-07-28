# Recode scores

Recode scores

## Usage

``` r
recode_score(x)
```

## Arguments

- x:

  string stage coding to correct

## Value

string of recoded stage score

## Details

`"Ac"` (apex closure) passes through unchanged rather than becoming
`NA`. It is a terminal stage, not a missing observation; see
[`score_to_stage()`](https://mu-cgcs.github.io/AgeFromDentition/reference/score_to_stage.md).

`"zero"` does become `NA`.

## Examples

``` r
recode_score("zero")
#> [1] NA
recode_score("Ci")
#> [1] "C.i"
recode_score("Ac")
#> [1] "Ac"
```
