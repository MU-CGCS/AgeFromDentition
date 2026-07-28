# Extract the text layer of the Šešelj et al. (2019) reference tables.
#
# Rationale: digits must never be read off rendered page images. A 6/8
# misread from an image produced a spurious "discrepancy" for the boys'
# M1 A1/2 parameter during planning. `pdftotext -layout` preserves both
# the digits and the column alignment, and the latter is required
# because Table 13a has genuinely empty cells (see PLAN.md §3.2).
#
# Output lands in data-raw/pdftotext/ and is committed, so
# build-reference-data.R is reproducible without the source PDF.

library(fs)
library(glue)
library(cli)

pdf_name <- paste0(
  "Šešelj et al. 2019 - Timing of Development of the Permanent ",
  "Mandibular Dentition - New Reference Values from the Fels ",
  "Longitudinal Study.pdf"
)

# Run from the package root.
project_root <- path_wd()
source_pdf <- path(project_root, pdf_name)
out_dir <- path(project_root, "data-raw", "pdftotext")

dir_create(out_dir)

if (!file_exists(source_pdf)) {
  cli_abort("Source PDF not found at {.path {source_pdf}}.")
}

if (Sys.which("pdftotext") == "") {
  cli_abort(c(
    "{.code pdftotext} not found on PATH.",
    i = "Install poppler, e.g. {.code brew install poppler}."
  ))
}

# PDF pages holding the reference tables. Tables 2-7 are ages of
# attainment; Tables 8-13 are ages given stage.
pages <- 3:14

for (p in pages) {
  out_file <- path(out_dir, glue("page-{sprintf('%02d', p)}"), ext = "txt")
  status <- system2(
    "pdftotext",
    args = c(
      "-layout",
      "-f", p,
      "-l", p,
      shQuote(source_pdf),
      shQuote(out_file)
    )
  )
  if (status != 0L) {
    cli_abort("pdftotext failed on page {p} (exit status {status}).")
  }
  cli_alert_success("Extracted page {p} -> {.path {path_file(out_file)}}")
}

cli_alert_info(
  "Extracted {length(pages)} pages to {.path {out_dir}}."
)
