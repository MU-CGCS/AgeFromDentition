# Terminal-stage information from a dental age estimate

Accessor for the completion-threshold detail attached to the result of
[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md).

## Usage

``` r
ac_info(x)
```

## Arguments

- x:

  a `dental_age` object.

## Value

A list with the threshold, the binding tooth, the convention used (`q`
and `method`), the full per-tooth table, the `low_precision` and
`tied_transition` flags, the tooth names in each category, the interval
level and type, the compatibility code, and the rendered report.

## Details

The information travels as an attribute, so it does not survive
arithmetic, [`c()`](https://rdrr.io/r/base/c.html), or row-wise
[`sapply()`](https://rdrr.io/r/base/lapply.html) over several
individuals. Those operations keep the numbers and drop the metadata.
Call `ac_info()` on each result before combining.

## See also

[`estimate_dental_age()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_dental_age.md),
[`ac_completion_threshold()`](https://mu-cgcs.github.io/AgeFromDentition/reference/ac_completion_threshold.md)

## Examples

``` r
x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                M1 = "Ac", M2 = "Ac", M3 = NA)
est <- estimate_dental_age(x, verbose = FALSE)
ac_info(est)$threshold
#> [1] 12.19381
ac_info(est)$binding_tooth
#> [1] "M2"
```
