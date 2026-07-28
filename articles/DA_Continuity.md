# Dental Age Continuity

``` r

library(AgeFromDentition)
library(dplyr)
library(tibble)
```

Dental age estimates generally increase as tooth stages advance. This
vignette constructs fine-scale example progressions for females and
males, starting with only the canine scored and gradually adding teeth,
ending with all teeth at `A.c` except M3 at `A.5`. Each row advances at
most a few teeth by a single stage — no tooth ever skips a stage.

## Female progression

``` r

female <- tribble(
    ~Sex , ~Canine , ~P3     , ~P4     , ~M1    , ~M2     , ~M3     ,
    "F"  , "C.oc"  , NA      , NA      , NA     , NA      , NA      ,
    "F"  , "Cr.5"  , NA      , NA      , NA     , NA      , NA      ,
    "F"  , "Cr.5"  , "C.i"   , NA      , NA     , NA      , NA      ,
    "F"  , "Cr.5"  , "C.co"  , NA      , NA     , NA      , NA      ,
    "F"  , "Cr.75" , "C.oc"  , NA      , NA     , NA      , NA      ,
    "F"  , "Cr.75" , "C.oc"  , NA      , "Cr.c" , NA      , NA      ,
    "F"  , "Cr.75" , "Cr.5"  , NA      , "R.i"  , NA      , NA      ,
    "F"  , "Cr.75" , "Cr.5"  , NA      , "R.i"  , "C.i"   , NA      ,
    "F"  , "Cr.75" , "Cr.5"  , "C.i"   , "Cl.i" , "C.i"   , NA      ,
    "F"  , "Cr.c"  , "Cr.75" , "C.co"  , "R.25" , "C.co"  , NA      ,
    "F"  , "Cr.c"  , "Cr.c"  , "C.oc"  , "R.25" , "C.oc"  , NA      ,
    "F"  , "Cr.c"  , "Cr.c"  , "Cr.5"  , "R.5"  , "Cr.5"  , NA      ,
    "F"  , "R.i"   , "Cr.c"  , "Cr.75" , "R.75" , "Cr.75" , NA      ,
    "F"  , "R.i"   , "R.i"   , "Cr.c"  , "R.75" , "Cr.c"  , NA      ,
    "F"  , "R.25"  , "R.i"   , "R.i"   , "R.75" , "R.i"   , NA      ,
    "F"  , "R.25"  , "R.25"  , "R.i"   , "R.c"  , "Cl.i"  , NA      ,
    "F"  , "R.5"   , "R.25"  , "R.25"  , "R.c"  , "R.25"  , NA      ,
    "F"  , "R.5"   , "R.5"   , "R.25"  , "A.5"  , "R.25"  , NA      ,
    "F"  , "R.75"  , "R.5"   , "R.5"   , "A.c"  , "R.5"   , NA      ,
    "F"  , "R.75"  , "R.5"   , "R.5"   , "A.c"  , "R.5"   , "C.i"   ,
    "F"  , "R.c"   , "R.75"  , "R.75"  , "A.c"  , "R.75"  , "C.co"  ,
    "F"  , "A.5"   , "R.c"   , "R.c"   , "A.c"  , "R.c"   , "C.oc"  ,
    "F"  , "A.c"   , "A.5"   , "A.5"   , "A.c"  , "A.5"   , "Cr.5"  ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "Cr.75" ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "Cr.c"  ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.i"   ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "Cl.i"  ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.25"  ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.5"   ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.75"  ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.c"   ,
    "F"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "A.5"   ,
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

    # A tibble: 32 × 11
       Canine P3    P4    M1    M2    M3    dental_age ci_lower ci_upper n_estimable
       <chr>  <chr> <chr> <chr> <chr> <chr>      <dbl>    <dbl>    <dbl>       <int>
     1 C.oc   <NA>  <NA>  <NA>  <NA>  <NA>        1.93     1.46     2.69           1
     2 Cr.5   <NA>  <NA>  <NA>  <NA>  <NA>        2.58     1.93     3.62           1
     3 Cr.5   C.i   <NA>  <NA>  <NA>  <NA>        2.51     2.08     3.09           2
     4 Cr.5   C.co  <NA>  <NA>  <NA>  <NA>        2.79     2.24     3.57           2
     5 Cr.75  C.oc  <NA>  <NA>  <NA>  <NA>        3.44     2.86     4.23           2
     6 Cr.75  C.oc  <NA>  Cr.c  <NA>  <NA>        3.18     2.41     4.39           3
     7 Cr.75  Cr.5  <NA>  R.i   <NA>  <NA>        3.7      3.06     4.57           3
     8 Cr.75  Cr.5  <NA>  R.i   C.i   <NA>        3.7      3.16     4.39           4
     9 Cr.75  Cr.5  C.i   Cl.i  C.i   <NA>        3.84     3.31     4.51           5
    10 Cr.c   Cr.75 C.co  R.25  C.co  <NA>        4.52     3.7      5.66           5
    # ℹ 22 more rows
    # ℹ 1 more variable: compatibility <chr>

## Male progression

``` r

male <- tribble(
    ~Sex , ~Canine , ~P3     , ~P4     , ~M1    , ~M2     , ~M3     ,
    "M"  , "C.oc"  , NA      , NA      , NA     , NA      , NA      ,
    "M"  , "Cr.5"  , NA      , NA      , NA     , NA      , NA      ,
    "M"  , "Cr.5"  , "C.i"   , NA      , NA     , NA      , NA      ,
    "M"  , "Cr.5"  , "C.co"  , NA      , NA     , NA      , NA      ,
    "M"  , "Cr.75" , "C.oc"  , NA      , NA     , NA      , NA      ,
    "M"  , "Cr.75" , "C.oc"  , NA      , "Cr.c" , NA      , NA      ,
    "M"  , "Cr.75" , "Cr.5"  , NA      , "R.i"  , NA      , NA      ,
    "M"  , "Cr.75" , "Cr.5"  , NA      , "R.i"  , "C.i"   , NA      ,
    "M"  , "Cr.75" , "Cr.5"  , "C.i"   , "Cl.i" , "C.i"   , NA      ,
    "M"  , "Cr.c"  , "Cr.75" , "C.co"  , "R.25" , "C.co"  , NA      ,
    "M"  , "Cr.c"  , "Cr.c"  , "C.oc"  , "R.25" , "C.oc"  , NA      ,
    "M"  , "Cr.c"  , "Cr.c"  , "Cr.5"  , "R.5"  , "Cr.5"  , NA      ,
    "M"  , "R.i"   , "Cr.c"  , "Cr.75" , "R.75" , "Cr.75" , NA      ,
    "M"  , "R.i"   , "R.i"   , "Cr.c"  , "R.75" , "Cr.c"  , NA      ,
    "M"  , "R.25"  , "R.i"   , "R.i"   , "R.75" , "R.i"   , NA      ,
    "M"  , "R.25"  , "R.25"  , "R.i"   , "R.c"  , "Cl.i"  , NA      ,
    "M"  , "R.5"   , "R.25"  , "R.25"  , "R.c"  , "R.25"  , NA      ,
    "M"  , "R.5"   , "R.5"   , "R.25"  , "A.5"  , "R.25"  , NA      ,
    "M"  , "R.75"  , "R.5"   , "R.5"   , "A.c"  , "R.5"   , NA      ,
    "M"  , "R.75"  , "R.5"   , "R.5"   , "A.c"  , "R.5"   , "C.i"   ,
    "M"  , "R.c"   , "R.75"  , "R.75"  , "A.c"  , "R.75"  , "C.co"  ,
    "M"  , "A.5"   , "R.c"   , "R.c"   , "A.c"  , "R.c"   , "C.oc"  ,
    "M"  , "A.c"   , "A.5"   , "A.5"   , "A.c"  , "A.5"   , "Cr.5"  ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "Cr.75" ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "Cr.c"  ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.i"   ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "Cl.i"  ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.25"  ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.5"   ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.75"  ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "R.c"   ,
    "M"  , "A.c"   , "A.c"   , "A.c"   , "A.c"  , "A.c"   , "A.5"   ,
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

    # A tibble: 32 × 11
       Canine P3    P4    M1    M2    M3    dental_age ci_lower ci_upper n_estimable
       <chr>  <chr> <chr> <chr> <chr> <chr>      <dbl>    <dbl>    <dbl>       <int>
     1 C.oc   <NA>  <NA>  <NA>  <NA>  <NA>        1.97     1.49     2.71           1
     2 Cr.5   <NA>  <NA>  <NA>  <NA>  <NA>        2.75     2.03     3.93           1
     3 Cr.5   C.i   <NA>  <NA>  <NA>  <NA>        2.58     2.08     3.31           2
     4 Cr.5   C.co  <NA>  <NA>  <NA>  <NA>        2.9      2.42     3.53           2
     5 Cr.75  C.oc  <NA>  <NA>  <NA>  <NA>        3.56     2.67     4.99           2
     6 Cr.75  C.oc  <NA>  Cr.c  <NA>  <NA>        3.27     2.31     4.99           3
     7 Cr.75  Cr.5  <NA>  R.i   <NA>  <NA>        3.89     3.14     4.94           3
     8 Cr.75  Cr.5  <NA>  R.i   C.i   <NA>        3.86     3.21     4.73           4
     9 Cr.75  Cr.5  C.i   Cl.i  C.i   <NA>        3.95     3.39     4.67           5
    10 Cr.c   Cr.75 C.co  R.25  C.co  <NA>        4.65     3.47     6.56           5
    # ℹ 22 more rows
    # ℹ 1 more variable: compatibility <chr>

## Notes

- Rows 1–2 use only the canine. With a single tooth, the estimate tracks
  that tooth’s reference age directly.
- P3 first appears at row 3, M1 at row 6 (already at `Cr.c` because it
  develops earliest), M2 at row 8, P4 at row 9.
- M3 first appears at row 20. For females, late M3 stages (`R.5` through
  `A.5`) have no published log-normal parameters and are excluded from
  the age estimate.
- From row 19 on, M1 reaches `A.c` and shifts from the age estimate to
  the completion threshold. The `compatibility` column shows whether the
  two agree.
- Rows 24–32 have all teeth at `A.c` except M3; only M3 contributes to
  the age estimate. For females, M3 has no parameters for `R.5` through
  `A.5`, so the final rows produce no point estimate — only the
  completion threshold. For males, all M3 stages have parameters.
- Row 32 has all teeth `A.c` except M3 at `A.5`.
- Small dips in dental age occur when a new tooth is added at an early
  stage (e.g., M3 at `C.i` in row 20) or when several teeth transition
  to `Ac` at once (row 23 to 24).
