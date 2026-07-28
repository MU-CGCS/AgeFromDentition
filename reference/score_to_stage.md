# Convert stage numeric score to character score

Convert stage numeric score to character score

## Usage

``` r
score_to_stage(x)
```

## Arguments

- x:

  vector of numeric stage scores

## Value

vector with numeric scores converted to character

## Details

Score 14 is apex closure and converts to `"Ac"`, a terminal stage. It is
deliberately **not** converted to `NA`: `NA` means the tooth was not
scored, whereas `"Ac"` means it was scored and is complete. The two are
different observations and must stay distinguishable.

Score 0 (crypt) does convert to `NA`. It lies below the first modelled
stage and has no reference parameters.

## Examples

``` r
data.frame(numeric_stage = 0:14, stage = score_to_stage(0:14))
#>    numeric_stage stage
#> 1              0  <NA>
#> 2              1   C.i
#> 3              2  C.co
#> 4              3  C.oc
#> 5              4  Cr.5
#> 6              5 Cr.75
#> 7              6  Cr.c
#> 8              7   R.i
#> 9              8  Cl.i
#> 10             9  R.25
#> 11            10   R.5
#> 12            11  R.75
#> 13            12   R.c
#> 14            13   A.5
#> 15            14    Ac
```
