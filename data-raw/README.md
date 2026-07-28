# Reference data pipeline

Builds the four datasets in `data/` from the tables in Šešelj M,
Sherwood RJ, Konigsberg LW. 2019. *Timing of Development of the
Permanent Mandibular Dentition: New Reference Values from the Fels
Longitudinal Study.* Anat Rec 302:1733-1753.

Nothing here ships with the package: `data-raw/` is listed in
`.Rbuildignore`.

## Two steps

Run both from the package root.

### 1. `extract-text-layers.R` — PDF to text

Extracts the text layer of pages 3-14 with `pdftotext -layout`, writing
`pdftotext/page-03.txt` … `page-14.txt`.

Requires the source PDF in the package root and `pdftotext` on the
`PATH` (`brew install poppler`).

**You rarely need this.** The extracted text is committed, so step 2 is
reproducible without the PDF or poppler. Re-run it only to verify the
committed text against the PDF, in which case the twelve output files
should come back byte-identical.

Digits are never read off rendered page images. A 6/8 misread from an
image produced a spurious discrepancy in the boys' M1 A1/2 parameter
during planning, which is why the pipeline goes through the text layer
and why `build-reference-data.R` pins that specific value.

### 2. `build-reference-data.R` — text to datasets

Parses, validates, re-keys, and writes. Needs the packages listed under
`Config/Needs/data-raw` in `DESCRIPTION`.

Parsing is position-aware rather than whitespace-splitting: Table 13a
has genuinely empty cells for girls, and a naive split would silently
shift the HPD/Opt values left into the parameter columns.

Only panel (a) of each table is used — the sex-specific Girls and Boys
columns. Panel (b), the combined-sex sample, is deliberately not
transcribed, because sex is always known in the intended use.

## What it writes

| Output | Contents |
| :-- | :-- |
| `data/AgeTables.rda` | Tables 8-13, age given stage, 150 rows |
| `data/AttainmentTables.rda` | Tables 2-7, age of attainment, 162 rows |
| `data/StageTies.rda` | tied adjacent transitions, 2 rows |
| `data/ExampleScores.rda` | re-serialized only; **contents untouched** |
| `fels-*.csv`, `attainment-*.csv`, `age-given-stage-*.csv` | validated intermediates, in source vocabulary |

The CSVs are kept because they are the checkable artifact: they carry
every column the paper prints, including the ones the package does not
currently use (`mode`, `median`, `mean`, `sd`, `hpd_low`, `opt`,
`hpd_high`).

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

The script aborts rather than writing bad data. It checks:

- **Closed-form log-normal identities.** Every parsed row must reconcile
  to the published mode, median, mean, and SD columns. Those identities
  are exact, so a transcription error is caught and localized.
- **Structural checks.** Monotone `theta`; complete stage sets; exactly
  two sex levels; valid counts; the expected sex ordering; monotone age
  summaries; that M1's negative values survived Unicode-minus
  normalization; and that boys' M1 A1/2 is 2.2621 as printed.
- **Tie detection.** Exactly two ties, both female M3, one interior and
  one terminal.
- **Reproduction of `AgeTables`.** The regenerated table must be
  `all.equal()` to the version already shipped, compared without
  re-sorting so that row order is checked too.

That last one is the most informative check in the file. `AgeTables` has
shipped since v0.1 and predates this pipeline, so reproducing all 150
values from an independent parse ties the extraction to a dataset that
was not derived from it. If it fails, do not overwrite — the shipped
values are what every existing result was computed from.

## Provenance

The extraction pipeline was adapted from a parallel implementation of
the same paper. It is maintained here now; the two copies will drift.
