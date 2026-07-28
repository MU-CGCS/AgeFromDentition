# Lookup mean and sd for a sex / tooth / stage combination

This function expects a row with columns for "Canine", "P3", "P4", "M1",
"M2", and "M3" that contain scores (or NA). Scores are recoded and
validated, then reference parameters are looked up.

## Usage

``` r
get_means_for_scores(x, verbose = TRUE)
```

## Arguments

- x:

  data frame with one row

- verbose:

  boolean flag for printing diagnostic messages

## Value

An object of class `dental_scores`: a list with the 6 x 2 `means` data
frame, the `sex`, and the tooth names in each of four categories
(estimable, terminal, missing, unparameterized). Can be passed directly
to
[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md).

## See also

[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md)

## Examples

``` r
get_means_for_scores(x = ExampleScores[1, ])
#> $means
#>        log_mu log_sd
#> Canine 2.1878 0.1395
#> P3     2.2077 0.1146
#> P4     2.1085 0.1155
#> M1     2.2621 0.1204
#> M2     2.2993 0.1312
#> M3         NA     NA
#> 
#> $sex
#> [1] "M"
#> 
#> $estimable_teeth
#> [1] "Canine" "P3"     "P4"     "M1"     "M2"    
#> 
#> $terminal_teeth
#> character(0)
#> 
#> $missing_teeth
#> [1] "M3"
#> 
#> $unparameterized_teeth
#> character(0)
#> 
#> attr(,"class")
#> [1] "dental_scores"

# Example from Seselj et al. (2019)
x <- data.frame(Sex = "M", Canine = "R.25", P3 = "R.25",
                P4 = "R.i", M1 = "A.5",
                M2 = "R.25", M3 = NA)
estimate_dental_age(x)
#> Dental age: 9.04 years (mode of the fitted distribution).
#> Central 95% interval: 7.61 to 10.90 years.
#> Estimated from 5 teeth.
#> Not scored: M3.
```
