# Reference completion threshold for terminal (Ac) teeth

A tooth at stage `Ac` (apex closure) is complete. That observation
cannot be inverted into an age, because apex closure is terminal: it
says only that the individual is older than the completion age. This
function reports the alternative — a descriptive percentile of the
reference distribution of age at completion.

## Usage

``` r
ac_completion_threshold(
  sex,
  teeth,
  q = 0.025,
  method = c("predictive", "plugin")
)
```

## Arguments

- sex:

  character, `"F"` or `"M"`.

- teeth:

  character vector of tooth names observed at stage `Ac`. May be length
  0, in which case the threshold is `NA`.

- q:

  lower-tail probability, in `(0, 0.5)`. Defaults to `0.025`.

- method:

  `"predictive"` (default) or `"plugin"`. See Details.

## Value

A list with components:

- threshold:

  numeric, the largest per-tooth threshold, or `NA_real_` if `teeth` is
  empty

- binding_tooth:

  character, the tooth that produced it

- q:

  the lower-tail probability used

- method:

  the method used

- per_tooth:

  data frame of the per-tooth calculation

- low_precision:

  logical, binding tooth poorly estimated

- tied_transition:

  logical, binding tooth's `Ac` is tied with the preceding stage

## What the number means

For a tooth \\t\\, the threshold is the \\q\\-th percentile of the
sex-specific reference distribution of age at *entering* `Ac`:

\$\$L(t, q) = \exp(\mu_t + z_q \sigma_t)\$\$

It says that \\q\\ of the reference sample had completed that tooth by
that age. It is a **descriptive completion threshold for the reference
sample**. It is *not* a \\1 - q\\ lower confidence or credible limit for
the individual being assessed, and must never be reported as one.

## Predictive versus plug-in

`method = "plugin"` takes \\\sigma_t\\ to be `log_sd`, treating the
fitted `log_mu` as known.

`method = "predictive"`, the default, uses \\\sigma_t =
\sqrt{\mathrm{log\\sd}^2 + \mathrm{se\\log\\mu}^2}\\, giving the
percentile of the predictive distribution for a further individual drawn
from the reference population. `log_mu` is estimated, and for the
terminal transitions it is sometimes estimated from very few
individuals.

The choice is immaterial for most teeth and decisive for one. Ten of the
twelve `Ac` transitions differ by less than 0.06 years between methods;
female M3 differs by 0.489 and male M3 by 0.304, because those two rest
on n = 1 and n = 5. Since M3 attains `Ac` latest of any tooth, it sets
the threshold whenever it is scored — so the two methods disagree most
in exactly the case this function exists to serve.

Both are reported in the return value, so a stated convention always
reproduces a specific number.

## Combining several teeth

With more than one `Ac` tooth the largest per-tooth threshold is
reported. This is a transparent **reporting convention**, not a
statistical combination: it ignores the joint probability of the
observed completion pattern and any correlation between teeth within a
person. The full `per_tooth` table is always returned so that the
convention can be inspected rather than trusted.

## Flags

`low_precision` marks a binding tooth estimated from few individuals (n
\< 10) or with a large standard error (`se_log_mu` \> 0.04). Under
`method = "predictive"` the imprecision is already reflected in the
number; the flag drives reporting.

`tied_transition` marks a binding tooth whose `Ac` transition is tied
with the stage below it in
[StageTies](https://mu-cgcs.github.io/AgeFromDentition/reference/StageTies.md).
For female M3 this is the case: `A.5` and `Ac` are fitted at the same
age, so observing apex closure supplies no information beyond `A.5`.

No tooth is ever dropped on account of either flag. Discarding the
binding tooth would understate the threshold and throw away the very
observation being reported on.

## See also

[AttainmentTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md),
[StageTies](https://mu-cgcs.github.io/AgeFromDentition/reference/StageTies.md)

## Examples

``` r
# A single completed second molar, in a female
ac_completion_threshold("F", "M2")$threshold
#> [1] 12.19381

# The plug-in convention gives a slightly different number
ac_completion_threshold("F", "M2", method = "plugin")$threshold
#> [1] 12.22868

# With several teeth complete, the latest-forming one binds
res <- ac_completion_threshold("F", c("Canine", "P3", "P4", "M1"))
res$binding_tooth
#> [1] "P4"
res$per_tooth
#>    Tooth log_mu log_sd se_log_mu    sd_eff   n threshold
#> 1 Canine 2.5597 0.1342    0.0289 0.1372765  23  9.881288
#> 2     P3 2.5909 0.1030    0.0192 0.1047742  31 10.864996
#> 3     P4 2.6827 0.1133    0.0195 0.1149658  35 11.674082
#> 4     M1 2.2787 0.1186    0.0118 0.1191856 112  7.729936
```
