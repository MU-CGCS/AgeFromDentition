# Print a dental age estimate

Print a dental age estimate

## Usage

``` r
# S3 method for class 'dental_age'
print(x, ...)
```

## Arguments

- x:

  a `dental_age` object from
  [`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md).

- ...:

  ignored.

## Value

`x`, invisibly.

## Examples

``` r
print(estimate_dental_age(ExampleScores[1, ], verbose = FALSE))
#> Dental age: 9.04 years (mode of the fitted distribution).
#> Central 95% interval: 7.61 to 10.90 years.
#> Estimated from 5 teeth.
#> Not scored: M3.
```
