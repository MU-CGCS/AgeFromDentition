# Reference data pipeline

Builds the four datasets in `data/` from the tables in Šešelj M,
Sherwood RJ, Konigsberg LW. 2019. *Timing of Development of the
Permanent Mandibular Dentition: New Reference Values from the Fels
Longitudinal Study.* Anat Rec 302:1733-1753.

Nothing here ships with the package: `data-raw/` is listed in
`.Rbuildignore`.

## Source of record

Two files are the archive:

| File | Contents |
| :-- | :-- |
| `fels-attainment.csv` | Tables 2-7, ages of attainment, 162 rows |
| `fels-age-given-stage.csv` | Tables 8-13, ages given stage, 150 rows |

They were transcribed from the article's text layer by a parser that has
since been removed, along with the article PDF. **There is no upstream
artifact left to regenerate them from.** Treat them as read-only:
`build-reference-data.R` never writes to them, and nothing else should.

`pdftotext/page-03.txt` … `page-14.txt` are the text layers those CSVs
were transcribed from. Nothing reads them any more. They are kept as the
most primary surviving record of the tables — without them, the CSVs
cannot be checked against anything outside themselves.

## Building

Run `build-reference-data.R` from the package root. It needs the
packages listed under `Config/Needs/data-raw` in `DESCRIPTION`.

Everything except the two archive files is regenerated on every run and
can be deleted and rebuilt at will:

| Output | Contents |
| :-- | :-- |
| `data/AgeTables.rda` | Tables 8-13, re-keyed, 150 rows |
| `data/AttainmentTables.rda` | Tables 2-7, re-keyed, 162 rows |
| `data/StageTies.rda` | tied adjacent transitions, 2 rows |
| `data/ExampleScores.rda` | re-serialized only; **contents untouched** |
| `fels-ties.csv` | derived tie registry |
| `attainment-*.csv`, `age-given-stage-*.csv` | per-tooth views of the archive |

The per-tooth files are pure duplication of the combined ones, kept for
convenience and rewritten each run so they cannot drift.

## Vocabulary

The CSVs use the source tables' names; the `.rda` datasets use the
package's. The adapter at the end of `build-reference-data.R` maps
between them.

| Source | Package |
| :-- | :-- |
| `C` | `Canine` |
| `female` / `male` | `F` / `M` |
| `Ci`, `Cco`, `Coc` | `C.i`, `C.co`, `C.oc` |
| `Cr1_2`, `Cr3_4`, `Crc` | `Cr.5`, `Cr.75`, `Cr.c` |
| `Ri`, `Cli` | `R.i`, `Cl.i` |
| `R1_4`, `R1_2`, `R3_4`, `Rc` | `R.25`, `R.5`, `R.75`, `R.c` |
| `A1_2`, `Ac` | `A.5`, `Ac` |

`Stage` is stored as **character**, not an ordered factor, because
`validate_score()` compares against it with `%in%`.

Row order is Tooth (anatomical), then Sex, then stage index
(developmental). Do not sort on `Stage`: it is character, so that would
give alphabetical order — `A.5` before `C.i` — and scramble the
sequence.

## Validation

The archive can no longer be checked against an upstream source, so the
script checks it against itself and against the package. It aborts
rather than writing bad data:

- **Closed-form log-normal identities.** Every row must reconcile to the
  published mode, median, mean, and SD columns. Those identities are
  exact, so corruption is caught and localized.
- **Structural checks.** Monotone `theta`; complete stage sets; exactly
  two sex levels; valid counts; the expected sex ordering; monotone age
  summaries; that M1's `Ci` theta is negative in both sexes; and that
  boys' M1 A1/2 is 2.2621 — the one value Šešelj et al. print twice, in
  Table 11a and again in the worked example on p. 16, so it has an
  independent published cross-check.
- **Tie detection.** Exactly two ties, both female M3, one interior and
  one terminal.
- **Reproduction of `AgeTables`.** The regenerated table must be
  `all.equal()` to the version already shipped, compared without
  re-sorting so that row order is checked too.

That last one is the most informative check in the file. `AgeTables` has
shipped since v0.1 and predates this pipeline, so reproducing all 150
values ties the archive to a dataset that was not derived from it. If it
fails, do not overwrite — the shipped values are what every existing
result was computed from.

## Provenance

The pipeline was adapted from a parallel implementation of the same
paper. It is maintained here now; the two copies will drift.
