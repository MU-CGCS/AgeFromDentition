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

`"zero"` becomes `NA`, because this is the "zero" stage, which
represents an open-ended lower bound.

## Examples

``` r
recode_score("zero")
#> [1] NA
recode_score("Ci")
#> [1] "C.i"
recode_score("Ac")
#> [1] "Ac"
```
