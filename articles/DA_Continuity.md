# Dental Age Continuity

``` r

library(AgeFromDentition)
library(dplyr)
library(tibble)
```

Dental age estimates should increase monotonically as tooth stages
advance. This vignette constructs example progressions for females and
males, starting with only the canine scored and gradually adding teeth
at developmentally appropriate stages, ending with all teeth at `Ac`
except M3 at `A.5`.

## Female progression

``` r

female <- tribble(
  ~Sex, ~Canine, ~P3,    ~P4,    ~M1,    ~M2,    ~M3,
  "F",  "C.oc",  NA,     NA,     NA,     NA,     NA,
  "F",  "Cr.75", NA,     NA,     NA,     NA,     NA,
  "F",  "Cr.c",  "C.co", NA,     NA,     NA,     NA,
  "F",  "R.i",   "Cr.5", NA,     "Cr.c", NA,     NA,
  "F",  "R.i",   "Cr.5", "C.co", "Cr.c", NA,     NA,
  "F",  "R.25",  "Cr.c", "Cr.5", "R.i",  "C.co", NA,
  "F",  "R.5",   "R.i",  "Cr.c", "Cl.i", "Cr.5", NA,
  "F",  "R.75",  "R.25", "R.i",  "R.5",  "Cr.c", NA,
  "F",  "R.c",   "R.5",  "R.25", "R.c",  "R.25", "C.i",
  "F",  "A.5",   "R.75", "R.5",  "A.5",  "R.5",  "C.co",
  "F",  "Ac",    "R.c",  "R.75", "Ac",   "R.75", "Cr.5",
  "F",  "Ac",    "A.5",  "R.c",  "Ac",   "R.c",  "Cr.c",
  "F",  "Ac",    "Ac",   "A.5",  "Ac",   "A.5",  "R.i",
  "F",  "Ac",    "Ac",   "Ac",   "Ac",   "Ac",   "R.25",
  "F",  "Ac",    "Ac",   "Ac",   "Ac",   "Ac",   "A.5",
)
```

``` r

female_results <- purrr::map_dfr(seq_len(nrow(female)), \(i) {
  est <- estimate_dental_age(female[i, ], verbose = FALSE)
  info <- ac_info(est)
  tibble(
    row = i,
    dental_age = est[["dental_age"]],
    ci_lower = est[["ci_lower"]],
    ci_upper = est[["ci_upper"]],
    n_estimable = info$n_estimable,
    compatibility = info$compatibility
  )
})

female |>
  mutate(row = row_number()) |>
  left_join(female_results, by = "row") |>
  select(-Sex, -row) |>
  mutate(across(c(dental_age, ci_lower, ci_upper), \(x) round(x, 2)))
```

    # A tibble: 15 × 11
       Canine P3    P4    M1    M2    M3    dental_age ci_lower ci_upper n_estimable
       <chr>  <chr> <chr> <chr> <chr> <chr>      <dbl>    <dbl>    <dbl>       <int>
     1 C.oc   <NA>  <NA>  <NA>  <NA>  <NA>        1.93     1.46     2.69           1
     2 Cr.75  <NA>  <NA>  <NA>  <NA>  <NA>        3.52     2.62     4.99           1
     3 Cr.c   C.co  <NA>  <NA>  <NA>  <NA>        3.01     1.63     7.53           2
     4 R.i    Cr.5  <NA>  Cr.c  <NA>  <NA>        3.38     1.74     9.59           3
     5 R.i    Cr.5  C.co  Cr.c  <NA>  <NA>        3.63     2.05     8.27           4
     6 R.25   Cr.c  Cr.5  R.i   C.co  <NA>        4.6      2.68     9.84           5
     7 R.5    R.i   Cr.c  Cl.i  Cr.5  <NA>        5.78     3.43    11.9            5
     8 R.75   R.25  R.i   R.5   Cr.c  <NA>        7.56     5.27    11.8            5
     9 R.c    R.5   R.25  R.c   R.25  C.i         9.47     7.79    11.8            6
    10 A.5    R.75  R.5   A.5   R.5   C.co       10.4      8.64    12.9            6
    11 Ac     R.c   R.75  Ac    R.75  Cr.5       11.8     10.6     13.3            4
    12 Ac     A.5   R.c   Ac    R.c   Cr.c       13.2     11.8     14.8            4
    13 Ac     Ac    A.5   Ac    A.5   R.i        14.2     12.5     16.1            3
    14 Ac     Ac    Ac    Ac    Ac    R.25       15.7     12.9     19.5            1
    15 Ac     Ac    Ac    Ac    Ac    A.5        NA       NA       NA              0
    # ℹ 1 more variable: compatibility <chr>

## Male progression

``` r

male <- tribble(
  ~Sex, ~Canine, ~P3,    ~P4,    ~M1,    ~M2,    ~M3,
  "M",  "C.oc",  NA,     NA,     NA,     NA,     NA,
  "M",  "Cr.75", NA,     NA,     NA,     NA,     NA,
  "M",  "Cr.c",  "C.co", NA,     NA,     NA,     NA,
  "M",  "R.i",   "Cr.5", NA,     "Cr.c", NA,     NA,
  "M",  "R.i",   "Cr.5", "C.co", "Cr.c", NA,     NA,
  "M",  "R.25",  "Cr.c", "Cr.5", "R.i",  "C.co", NA,
  "M",  "R.5",   "R.i",  "Cr.c", "Cl.i", "Cr.5", NA,
  "M",  "R.75",  "R.25", "R.i",  "R.5",  "Cr.c", NA,
  "M",  "R.c",   "R.5",  "R.25", "R.c",  "R.25", "C.i",
  "M",  "A.5",   "R.75", "R.5",  "A.5",  "R.5",  "C.co",
  "M",  "Ac",    "R.c",  "R.75", "Ac",   "R.75", "Cr.5",
  "M",  "Ac",    "A.5",  "R.c",  "Ac",   "R.c",  "Cr.c",
  "M",  "Ac",    "Ac",   "A.5",  "Ac",   "A.5",  "R.i",
  "M",  "Ac",    "Ac",   "Ac",   "Ac",   "Ac",   "R.25",
  "M",  "Ac",    "Ac",   "Ac",   "Ac",   "Ac",   "A.5",
)
```

``` r

male_results <- purrr::map_dfr(seq_len(nrow(male)), \(i) {
  est <- estimate_dental_age(male[i, ], verbose = FALSE)
  info <- ac_info(est)
  tibble(
    row = i,
    dental_age = est[["dental_age"]],
    ci_lower = est[["ci_lower"]],
    ci_upper = est[["ci_upper"]],
    n_estimable = info$n_estimable,
    compatibility = info$compatibility
  )
})

male |>
  mutate(row = row_number()) |>
  left_join(male_results, by = "row") |>
  select(-Sex, -row) |>
  mutate(across(c(dental_age, ci_lower, ci_upper), \(x) round(x, 2)))
```

    # A tibble: 15 × 11
       Canine P3    P4    M1    M2    M3    dental_age ci_lower ci_upper n_estimable
       <chr>  <chr> <chr> <chr> <chr> <chr>      <dbl>    <dbl>    <dbl>       <int>
     1 C.oc   <NA>  <NA>  <NA>  <NA>  <NA>        1.97     1.49     2.71           1
     2 Cr.75  <NA>  <NA>  <NA>  <NA>  <NA>        4.04     2.98     5.78           1
     3 Cr.c   C.co  <NA>  <NA>  <NA>  <NA>        2.89     1.4      9.78           2
     4 R.i    Cr.5  <NA>  Cr.c  <NA>  <NA>        3.54     1.73    11.6            3
     5 R.i    Cr.5  C.co  Cr.c  <NA>  <NA>        3.74     2.01     9.53           4
     6 R.25   Cr.c  Cr.5  R.i   C.co  <NA>        4.71     2.63    11.0            5
     7 R.5    R.i   Cr.c  Cl.i  Cr.5  <NA>        6        3.42    13.4            5
     8 R.75   R.25  R.i   R.5   Cr.c  <NA>        7.82     5.22    13.1            5
     9 R.c    R.5   R.25  R.c   R.25  C.i         9.83     7.71    13.0            6
    10 A.5    R.75  R.5   A.5   R.5   C.co       10.8      8.64    14              6
    11 Ac     R.c   R.75  Ac    R.75  Cr.5       12.0     10.7     13.7            4
    12 Ac     A.5   R.c   Ac    R.c   Cr.c       13.3     11.9     14.9            4
    13 Ac     Ac    A.5   Ac    A.5   R.i        14.1     12.3     16.2            3
    14 Ac     Ac    Ac    Ac    Ac    R.25       15.3     12.6     19.0            1
    15 Ac     Ac    Ac    Ac    Ac    A.5        17.3     14.8     20.6            1
    # ℹ 1 more variable: compatibility <chr>

## Notes

- Rows 1–2 use only the canine. With a single tooth, the estimate tracks
  that tooth’s reference age directly.
- M1 is added at row 4. When M1 is scored at `C.i`, it is dropped
  because that stage is left-censored in the reference model.
- M3 first appears at row 9. For females, late M3 stages (`R.5`, `R.75`,
  `R.c`, `A.5`) have no published log-normal parameters and are
  excluded.
- From row 11 onward, some teeth reach `Ac` and shift from the age
  estimate to the completion threshold. The `compatibility` column shows
  whether the two agree.
- Row 15 has all teeth `Ac` except M3 at `A.5`. For females, M3 `A.5` is
  unparameterized, so there is no point estimate – only the completion
  threshold. For males, M3 `A.5` has parameters and produces an
  estimate.
