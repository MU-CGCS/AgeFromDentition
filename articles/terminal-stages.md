# Terminal apex closed stages

``` r

library(AgeFromDentition)
```

## Apex closed (A_(c)) stages cannot be inverted

The method presented in Šešelj et al. (2019) estimates age by inverting
a tooth’s stage. Given that a tooth is observed e.g., at `R.5`, Šešelj
et al.tables 8–13 supply a log-normal distribution for the individual’s
age.

Apex closure (tooth stage coded `A.c`) breaks that logic, because it is
terminal (no exit point). Once a tooth reaches it, it stays there.
Observing A_(c) therefore tells you only that the individual is older
than the completion age (which has a distribution). It does not localize
age further. Šešelj et al. tables 8–13 have no A_(c) row for this
reason.

For a non-terminal stage $`s`$ the likelihood is localized between a
lower bound ($`F_{t,s}(a)`$) and an upper bound ($`F_{t,s+1}(a)`$):

``` math
P(s \mid a) = F_{t,s}(a) - F_{t,s+1}(a)
```

The difference $`F_{t,s}(a) - F_{t,s+1}(a)`$ is the probability of being
in stage $`s`$ at age $`a`$, having entered $`s`$ but not yet entered
$`s+1`$. This is the likelihood used to invert the stage into an age
estimate.

whereas for the terminal stage it is one-sided, rising with age towards
1:

``` math
P(\mathrm{A_c} \mid a) = F_{t,\mathrm{A_c}}(a)
```

An A_(c) observation is weakly informative about age, because that
observation has an age at attainment (Šešelj et al. tables 2-7), which
itself has a distribution.

## What is reported

Teeth at A_(c) are excluded from the weighted age estimate and used to
compute a *reference completion threshold*: a percentile of the
sex-specific distribution of age upon *entering* A_(c), taken from
Tables 2–7.

``` r

ac_completion_threshold("F", "M2")$threshold
```

    [1] 12.19381

The threshold value represents that 2.5% of the female reference sample
had M2 at A_(c) by about 12.2 years.

### What it is not

It is not a lower confidence limit for the individual being assessed. It
describes the reference sample, not the person. It is also not a minimum
age. Individuals below the threshold exist in the reference sample: 2.5%
by definition.

## The all-A_(c) case

When every scored tooth has completed, there is no point estimate:

``` r

x <- data.frame(
    Sex = "F",
    Canine = "A.c",
    P3 = "A.c",
    P4 = "A.c",
    M1 = "A.c",
    M2 = "A.c",
    M3 = NA
)

estimate_dental_age(x, verbose = FALSE)
```

    Dental age: no finite point estimate.
    All scored teeth are at terminal stage Ac (Canine, P3, P4, M1, M2).
    Reference completion threshold: 12.19 years - the 2.5th percentile of the sex-specific predictive distribution for attaining Ac in M2 (q = 0.025, method = "predictive").
    This is a descriptive completion threshold for the reference sample, not a lower confidence limit for this individual.
    Not scored: M3.

The threshold still exists, drawn from the five scored teeth:

``` r

ac_info(estimate_dental_age(x, verbose = FALSE))$threshold
```

    [1] 12.19381

An unscored M3 is not treated as completed. Scoring it `A.c` adds a
sixth tooth to the threshold calculation, which changes the binding
tooth:

``` r

y <- x
y$M3 <- "A.c"

ac_info(estimate_dental_age(y, verbose = FALSE))$threshold
```

    [1] 14.10746

## Predictive interval vs. known mean

By default the threshold is computed predictively, widening the
reference standard deviation by the standard error of the fitted mean:

``` math
\sigma_{\text{eff}} = \sqrt{\mathrm{log\_sd}^2 + \mathrm{se\_log\_mu}^2}
```

Šešelj et al. treat the fitted mean as known. The difference is small
for most teeth and important for one:

``` r

teeth <- c("Canine", "P3", "P4", "M1", "M2", "M3")

data.frame(
    Tooth = teeth,
    predictive = vapply(
        teeth,
        \(t) {
            ac_completion_threshold("F", t)$threshold
        },
        numeric(1)
    ),
    plugin = vapply(
        teeth,
        \(t) {
            ac_completion_threshold("F", t, method = "plugin")$threshold
        },
        numeric(1)
    )
) |>
    transform(difference = round(predictive - plugin, 3))
```

            Tooth predictive    plugin difference
    Canine Canine   9.881288  9.941052     -0.060
    P3         P3  10.864996 10.902844     -0.038
    P4         P4  11.674082 11.712259     -0.038
    M1         M1   7.729936  7.738813     -0.009
    M2         M2  12.193812 12.228676     -0.035
    M3         M3  14.107460 14.595947     -0.488

Female M3 moves by almost half a year, because its A_(c) transition is
estimated from a single individual. Since M3 attains A_(c) latest of any
tooth, it binds the threshold whenever it is scored.

Use `method = "plugin"` if you need the original form. Both `q` and
`method` appear in the printed output so that the number can be
reproduced from the report alone.

### Uncertainty in standard deviation is not propagated

Uncertainty in the scale parameter (`se_ln_sd` in the source tables,
around 0.003–0.004) is *not* propagated. The predictive form is more
predictive than plug-in, but not fully predictive.

## Female M3 stage tie

``` r

StageTies
```

      Sex Tooth Stage_lo Stage_hi log_mu tie_class
    1   F    M3      R.5     R.75 2.8156  interior
    2   F    M3      A.5       Ac 2.8934  terminal

For females, the fitted age of attaining M3 A_(c) is identical to the
age of attaining A_(1/2). Observing apex closure in a female M3
therefore supplies no information beyond the preceding stage, under this
reference model. The package flags it:

``` r

info <- ac_info(estimate_dental_age(y, verbose = FALSE))
c(low_precision = info$low_precision, tied = info$tied_transition)
```

    low_precision          tied
             TRUE          TRUE 

Neither flag causes the tooth to be dropped.

## Compatibility codes

When some teeth are still developing and others have completed, you get
an estimate, a threshold, and a code relating them:

| Code                        | Condition                           |
|:----------------------------|:------------------------------------|
| `no_terminal_information`   | no tooth at `A.c`                   |
| `completion_threshold_only` | teeth at `A.c`, none estimable      |
| `compatible`                | `ci_lower >= threshold`             |
| `overlap`                   | threshold falls inside the interval |
| `discordant`                | `ci_upper < threshold`              |

The interval is an equal-tailed **central 95%** interval, computed
analytically. It is not an HDI and not from MCMC.
[`estimate_age_hdi()`](https://mu-cgcs.github.io/AgeFromDentition/reference/estimate_age_hdi.md)
uses MCMC, which is reported to the user.

`discordant` raises a warning. The age-given-stage estimator and the
attainment threshold come from related but different models, combined
approximately, so disagreement can reflect the method as much as the
data.

## Combining several A_(c) teeth

With more than one completed tooth, the *largest* per-tooth threshold is
reported. It ignores the joint probability of the observed completion
pattern and any correlation between teeth within a person.

The full per-tooth table is always returned:

``` r

ac_completion_threshold("F", c("Canine", "P3", "P4", "M1"))$per_tooth
```

       Tooth log_mu log_sd se_log_mu    sd_eff   n threshold
    1 Canine 2.5597 0.1342    0.0289 0.1372765  23  9.881288
    2     P3 2.5909 0.1030    0.0192 0.1047742  31 10.864996
    3     P4 2.6827 0.1133    0.0195 0.1149658  35 11.674082
    4     M1 2.2787 0.1186    0.0118 0.1191856 112  7.729936

Note that the threshold tooth here is P4, not the canine.

## Reference

Šešelj M, Sherwood RJ, Konigsberg LW. 2019. Timing of Development of the
Permanent Mandibular Dentition: New Reference Values from the Fels
Longitudinal Study. *Anat Rec* 302:1733–1753.
