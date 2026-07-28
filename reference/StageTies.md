# Tied stage transitions

Adjacent stages whose fitted ages of attainment are identical in
[AttainmentTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md),
meaning the intervening stage has zero width under the reference model.

## Usage

``` r
StageTies
```

## Format

A data frame with 2 rows and 6 variables:

- Sex:

  character of sex: "F" or "M"

- Tooth:

  character ID of tooth

- Stage_lo:

  character, the earlier stage of the tied pair

- Stage_hi:

  character, the later stage of the tied pair

- log_mu:

  numeric mean log age shared by both transitions

- tie_class:

  character: "terminal" if `Stage_hi` is `Ac`, otherwise "interior"

## Source

Derived from Tables 2-7 of Šešelj M, Sherwood RJ, Konigsberg LW. 2019.
Timing of Development of the Permanent Mandibular Dentition: New
Reference Values from the Fels Longitudinal Study. Anat Rec
302:1733-1753.

## Details

Both ties are female M3, and the `terminal` one has a practical
consequence: because the fitted age of attaining M3 `Ac` equals the age
of attaining M3 `A.5`, observing apex closure in a female M3 supplies no
information beyond `A.5` under this reference model. Any completion
threshold that a female M3 `Ac` produces should be reported as
model-tied.

The `interior` tie explains why female M3 `R.5` has no parameters in
[AgeTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AgeTables.md).

## See also

[AttainmentTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AttainmentTables.md),
[AgeTables](https://mu-cgcs.github.io/AgeFromDentition/reference/AgeTables.md)
