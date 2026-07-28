# Build the AgeFromDentition reference data from the Šešelj et al.
# (2019) Fels Longitudinal Study tables.
#
#   Tables 2-7  (pages 3-8)  -> ages of attainment (transition analysis)
#   Tables 8-13 (pages 9-14) -> ages given stage
#
# Only panel (a) of each table is used: the sex-specific Girls and Boys
# columns. Panel (b), the combined-sex sample, is deliberately not
# transcribed, because sex is always known in the intended use.
#
# Parsing is position-aware rather than whitespace-splitting, because
# Table 13a has genuinely empty cells for girls: a naive split would
# silently shift the Low HPD / Opt / High HPD values into the ln_mu /
# ln_sigma / mode columns. Those four rows are described in PLAN.md
# §3.2.
#
# Every parsed row is validated against the closed-form log-normal
# identities relating theta/sigma to the published mode, median, mean
# and SD columns. Those identities are exact, so any transcription
# error is caught and localized rather than shipped.

library(fs)
library(glue)
library(cli)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

project_root <- path_wd()
txt_dir <- path(project_root, "data-raw", "pdftotext")
out_dir <- path(project_root, "data-raw")

# ---------------------------------------------------------------------
# Table registry
# ---------------------------------------------------------------------

# Canonical stage vocabulary. "crypt" (Šešelj et al.'s stage zero) has
# no attainment row; it is the open interval below Ci and is handled in
# the likelihood, not in the reference tables.
stage_levels_molar <- c(
  "Ci", "Cco", "Coc", "Cr1_2", "Cr3_4", "Crc", "Ri", "Cli",
  "R1_4", "R1_2", "R3_4", "Rc", "A1_2", "Ac"
)

# Single-rooted teeth omit Cli (root cleft initiation).
stage_levels_single <- setdiff(stage_levels_molar, "Cli")

tables <- tribble(
  ~kind,          ~tooth, ~page, ~table_no,
  "attainment",   "C",        3,  2,
  "attainment",   "P3",       4,  3,
  "attainment",   "P4",       5,  4,
  "attainment",   "M1",       6,  5,
  "attainment",   "M2",       7,  6,
  "attainment",   "M3",       8,  7,
  "age_given_stage", "C",     9,  8,
  "age_given_stage", "P3",   10,  9,
  "age_given_stage", "P4",   11, 10,
  "age_given_stage", "M1",   12, 11,
  "age_given_stage", "M2",   13, 12,
  "age_given_stage", "M3",   14, 13
)

# Per-sex column names, in printed order.
cols_attainment <- c("theta", "se_theta", "n", "mode", "median", "mean")
cols_ags <- c(
  "ln_mu", "ln_sigma", "mode", "median", "mean", "sd",
  "hpd_low", "opt", "hpd_high"
)

is_molar <- function(tooth) {
  return(tooth %in% c("M1", "M2", "M3"))
}

expected_stages <- function(tooth, kind) {
  levels <- if (is_molar(tooth)) stage_levels_molar else stage_levels_single
  # The age-given-stage tables have no Ac row: Ac is open to the right
  # and has no finite mean age in stage.
  if (kind == "age_given_stage") {
    levels <- setdiff(levels, "Ac")
  }
  return(levels)
}

# ---------------------------------------------------------------------
# Position-aware parsing
# ---------------------------------------------------------------------

# Normalize the Unicode minus (U+2212) used for M1's negative theta
# values. Left as-is it parses to NA, or to 0 under a lax parser, which
# would manufacture spurious ties in M1.
normalize_minus <- function(x) {
  return(str_replace_all(x, "−", "-"))
}

# Locate every whitespace-delimited token in a line together with its
# character span.
tokenize_with_spans <- function(line) {
  m <- str_locate_all(line, "\\S+")[[1]]
  if (nrow(m) == 0L) {
    return(tibble(text = character(), start = integer(), end = integer()))
  }
  return(tibble(
    text = str_sub(line, m[, "start"], m[, "end"]),
    start = as.integer(m[, "start"]),
    end = as.integer(m[, "end"])
  ))
}

# Derive column bounds from rows that carry the full complement of
# tokens. In such a row the j-th token is by construction the j-th
# column, so no clustering heuristic is needed: the bound for column j
# is simply the extent of the j-th token across all complete rows.
#
# This matters because pdftotext's column alignment drifts in the
# narrower tables (Table 8 in particular), which defeats naive
# overlap-based clustering.
column_bounds_from_complete <- function(tokens, n_cols, page) {
  complete <- keep(tokens, \(x) nrow(x) == n_cols)
  if (length(complete) == 0L) {
    cli_abort(
      "No complete rows on page {page}; cannot derive column bounds."
    )
  }
  starts <- map(complete, \(x) x$start) |> reduce(pmin)
  ends <- map(complete, \(x) x$end) |> reduce(pmax)

  # Bounds for adjacent columns can overlap where pdftotext's
  # alignment drifts. That is only a problem if a token from a ragged
  # row actually lands in the ambiguous zone, which the per-token
  # uniqueness check downstream detects exactly. Note it and continue.
  overlap <- which(starts[-1] <= ends[-n_cols])
  if (length(overlap) > 0L) {
    cli_alert_info(
      "Page {page}: column bounds touch between column{?s} \\
       {overlap} and the next; relying on per-token checks."
    )
  }
  return(tibble(column = seq_len(n_cols), start = starts, end = ends))
}

# Parse panel (a) of one table into a wide tibble with one row per
# stage and 2 * length(value_cols) value columns.
parse_panel_a <- function(page, tooth, kind) {
  file <- path(txt_dir, glue("page-{sprintf('%02d', page)}"), ext = "txt")
  lines <- read_lines(file) |> normalize_minus()

  stages <- expected_stages(tooth, kind)
  value_cols <- if (kind == "attainment") cols_attainment else cols_ags
  n_cols <- 2L * length(value_cols)

  # Printed stage labels use "/" where canonical names use "_".
  printed <- str_replace_all(stages, "_", "/")

  # Panel (a) precedes panel (b). Take the first occurrence of each
  # stage label at the start of a line, in printed order, and stop
  # before the combined-sex panel.
  # Anchor on the standalone panel header, not on the table title,
  # which also contains the string "(b) combined".
  panel_b_start <- str_which(lines, "^\\s*\\(b\\)\\s+Combined sex sample\\s*$")
  if (length(panel_b_start) == 0L) {
    cli_abort("Could not locate panel (b) header on page {page}.")
  }
  panel_a <- lines[seq_len(panel_b_start[1] - 1L)]

  row_index <- map_int(printed, function(label) {
    hits <- str_which(panel_a, glue("^\\s*{str_escape(label)}\\s"))
    if (length(hits) == 0L) {
      cli_abort("Stage {label} not found in panel (a) of page {page}.")
    }
    return(hits[1])
  })

  if (is.unsorted(row_index, strictly = TRUE)) {
    cli_abort(c(
      "Stage rows on page {page} are not in the expected order.",
      i = "Found row indices: {row_index}"
    ))
  }

  rows <- panel_a[row_index]

  # Drop the stage label from each row, keep the numeric tokens.
  tokens <- map2(rows, printed, function(line, label) {
    spans <- tokenize_with_spans(line)
    return(spans |> filter(text != label))
  })

  # Position matching, and the stricter column-separation requirement
  # it depends on, are only needed when a row has missing cells.
  # pdftotext's column alignment drifts in the narrower tables, so
  # bounds are not always well separated -- that is harmless as long as
  # every row is complete, since then token j is column j.
  n_ragged <- sum(map_int(tokens, nrow) != n_cols)
  bounds <- NULL

  if (n_ragged > 0L) {
    cli_alert_warning(
      "Page {page}: {n_ragged} row{?s} with missing cells; assigning \\
       tokens by column position."
    )
    bounds <- column_bounds_from_complete(tokens, n_cols, page)
  }

  values <- map(tokens, function(spans) {
    # Complete rows need no position matching: token j is column j.
    if (nrow(spans) == n_cols) {
      return(spans$text)
    }
    out <- rep(NA_character_, n_cols)
    assigned <- integer(nrow(spans))
    for (i in seq_len(nrow(spans))) {
      # Character overlap between this token and each column's extent.
      # Where adjacent column bounds touch, the token belongs to the
      # column it overlaps most; a tie is genuinely ambiguous.
      width <- pmin(spans$end[i], bounds$end) -
        pmax(spans$start[i], bounds$start) + 1L
      width[width < 0L] <- 0L
      best <- which(width == max(width) & width > 0L)
      if (length(best) != 1L) {
        cli_abort(c(
          "Token {.val {spans$text[i]}} on page {page} could not be \\
           assigned to a single column.",
          i = "Token span {spans$start[i]}-{spans$end[i]}, \\
               {length(best)} best-overlap candidate{?s}."
        ))
      }
      assigned[i] <- best
      out[best] <- spans$text[i]
    }
    if (is.unsorted(assigned, strictly = TRUE)) {
      cli_abort(c(
        "Token-to-column assignment is not increasing on page {page}.",
        i = "Assigned columns: {assigned}"
      ))
    }
    return(out)
  })

  wide <- do.call(rbind, values) |>
    as_tibble(.name_repair = "minimal") |>
    set_names(c(
      paste0("female_", value_cols),
      paste0("male_", value_cols)
    )) |>
    mutate(across(everything(), as.numeric)) |>
    mutate(
      tooth = tooth,
      stage = factor(stages, levels = stages),
      stage_index = seq_along(stages),
      .before = 1
    )

  return(wide)
}

# The per-tooth common ln SD lives in a trailing row of the attainment
# tables, not in the per-stage rows.
parse_ln_sd <- function(page, tooth) {
  file <- path(txt_dir, glue("page-{sprintf('%02d', page)}"), ext = "txt")
  lines <- read_lines(file) |> normalize_minus()
  # Anchor on the standalone panel header, not on the table title,
  # which also contains the string "(b) combined".
  panel_b_start <- str_which(lines, "^\\s*\\(b\\)\\s+Combined sex sample\\s*$")
  panel_a <- lines[seq_len(panel_b_start[1] - 1L)]

  hit <- str_which(panel_a, "^\\s*ln_SD")
  if (length(hit) == 0L) {
    cli_abort("No ln_SD row found in panel (a) of page {page}.")
  }
  nums <- tokenize_with_spans(panel_a[hit[1]]) |>
    filter(str_detect(text, "^-?[0-9]")) |>
    pull(text) |>
    as.numeric()

  if (length(nums) != 4L) {
    cli_abort(c(
      "Expected 4 numbers in the ln_SD row on page {page}, got \\
       {length(nums)}.",
      i = "Values: {nums}"
    ))
  }
  return(tibble(
    tooth = tooth,
    female_ln_sd = nums[1],
    female_se_ln_sd = nums[2],
    male_ln_sd = nums[3],
    male_se_ln_sd = nums[4]
  ))
}

# ---------------------------------------------------------------------
# Parse all tables and reshape to long format
# ---------------------------------------------------------------------

cli_h1("Parsing reference tables")

attainment_wide <- tables |>
  filter(kind == "attainment") |>
  pmap(function(kind, tooth, page, table_no) {
    cli_alert_info("Table {table_no} (page {page}): {tooth} attainment")
    return(parse_panel_a(page, tooth, kind))
  }) |>
  list_rbind()

ln_sd_wide <- tables |>
  filter(kind == "attainment") |>
  pmap(function(kind, tooth, page, table_no) {
    return(parse_ln_sd(page, tooth))
  }) |>
  list_rbind()

ags_wide <- tables |>
  filter(kind == "age_given_stage") |>
  pmap(function(kind, tooth, page, table_no) {
    cli_alert_info("Table {table_no} (page {page}): {tooth} age given stage")
    return(parse_panel_a(page, tooth, kind))
  }) |>
  list_rbind()

pivot_by_sex <- function(wide, value_cols) {
  out <- wide |>
    pivot_longer(
      cols = matches("^(female|male)_"),
      names_to = c("sex", ".value"),
      names_pattern = "^(female|male)_(.*)$"
    ) |>
    mutate(sex = factor(sex, levels = c("female", "male"))) |>
    relocate(tooth, sex, stage, stage_index) |>
    arrange(tooth, sex, stage_index)
  return(out)
}

fels_attainment <- attainment_wide |>
  pivot_by_sex(cols_attainment) |>
  left_join(
    ln_sd_wide |>
      pivot_longer(
        cols = matches("^(female|male)_"),
        names_to = c("sex", ".value"),
        names_pattern = "^(female|male)_(.*)$"
      ) |>
      mutate(sex = factor(sex, levels = c("female", "male"))),
    by = c("tooth", "sex")
  ) |>
  mutate(n = as.integer(n))

fels_age_given_stage <- ags_wide |>
  pivot_by_sex(cols_ags)

# ---------------------------------------------------------------------
# QA: closed-form log-normal identities
# ---------------------------------------------------------------------

cli_h1("Validating against closed-form log-normal identities")

# For attainment ages ~ lognormal(theta, ln_sd):
#   median = exp(theta)
#   mode   = exp(theta - ln_sd^2)
#   mean   = exp(theta + ln_sd^2 / 2)
check_attainment <- fels_attainment |>
  mutate(
    median_hat = exp(theta),
    mode_hat = exp(theta - ln_sd^2),
    mean_hat = exp(theta + ln_sd^2 / 2),
    rel_median = abs(median_hat - median) / median,
    rel_mode = abs(mode_hat - mode) / mode,
    rel_mean = abs(mean_hat - mean) / mean
  )

# For age given stage ~ lognormal(ln_mu, ln_sigma), additionally:
#   sd = mean * sqrt(exp(ln_sigma^2) - 1)
check_ags <- fels_age_given_stage |>
  mutate(
    median_hat = exp(ln_mu),
    mode_hat = exp(ln_mu - ln_sigma^2),
    mean_hat = exp(ln_mu + ln_sigma^2 / 2),
    sd_hat = mean_hat * sqrt(exp(ln_sigma^2) - 1),
    rel_median = abs(median_hat - median) / median,
    rel_mode = abs(mode_hat - mode) / mode,
    rel_mean = abs(mean_hat - mean) / mean,
    rel_sd = abs(sd_hat - sd) / sd
  )

tolerance <- 1e-3

report_failures <- function(checks, cols, label) {
  failures <- checks |>
    filter(if_any(all_of(cols), \(x) !is.na(x) & x > tolerance))
  if (nrow(failures) > 0L) {
    print(failures |> select(tooth, sex, stage, all_of(cols)), n = 40)
    cli_abort(
      "{nrow(failures)} {label} row{?s} failed the log-normal identity \\
       check at tolerance {tolerance}."
    )
  }
  n_checked <- checks |>
    filter(if_all(all_of(cols), \(x) !is.na(x))) |>
    nrow()
  cli_alert_success(
    "{label}: {n_checked} row{?s} reconcile to < {tolerance} relative."
  )
  return(invisible(NULL))
}

report_failures(
  check_attainment,
  c("rel_median", "rel_mode", "rel_mean"),
  "attainment"
)
report_failures(
  check_ags,
  c("rel_median", "rel_mode", "rel_mean", "rel_sd"),
  "age-given-stage"
)

# Rows where the published parameters are absent. In Table 13a these
# are real: girls' M3 R1/2 and A1/2 have zero-width stages (tied
# transitions), and R3/4 and Rc are reported as HPD/Opt only.
missing_params <- fels_age_given_stage |>
  filter(is.na(ln_mu)) |>
  select(tooth, sex, stage, ln_mu, hpd_low, opt, hpd_high)

if (nrow(missing_params) > 0L) {
  cli_alert_warning(
    "{nrow(missing_params)} age-given-stage row{?s} lack log-normal \\
     parameters:"
  )
  print(missing_params)
}

# ---------------------------------------------------------------------
# QA: structural checks
# ---------------------------------------------------------------------

cli_h1("Structural checks")

# 1. theta non-decreasing within (tooth, sex).
decreasing <- fels_attainment |>
  arrange(tooth, sex, stage_index) |>
  mutate(drop = theta < lag(theta), .by = c(tooth, sex)) |>
  filter(drop)

if (nrow(decreasing) > 0L) {
  print(decreasing |> select(tooth, sex, stage, theta))
  cli_abort("theta decreases within a tooth/sex series.")
}
cli_alert_success("theta is non-decreasing within every tooth and sex.")

# 2. Stage sets complete.
walk2(
  tables$tooth, tables$kind,
  function(tooth_i, kind_i) {
    reference <- if (kind_i == "attainment") {
      fels_attainment
    } else {
      fels_age_given_stage
    }
    for (sex_i in c("female", "male")) {
      got <- reference |>
        filter(tooth == tooth_i, sex == sex_i) |>
        pull(stage) |>
        as.character()
      want <- expected_stages(tooth_i, kind_i)
      if (!identical(got, want)) {
        cli_abort(c(
          "Stage set mismatch for {tooth_i} / {sex_i} / {kind_i}.",
          i = "Got:  {paste(got, collapse = ', ')}",
          i = "Want: {paste(want, collapse = ', ')}"
        ))
      }
    }
  }
)
cli_alert_success("Stage sets match the expected vocabulary everywhere.")

# 3. Exactly two sex levels, no combined-sex leakage.
for (reference in list(fels_attainment, fels_age_given_stage)) {
  got_levels <- levels(reference$sex)
  if (!identical(got_levels, c("female", "male"))) {
    cli_abort(
      "Unexpected sex levels: {paste(got_levels, collapse = ', ')}."
    )
  }
}
cli_alert_success("Sex has exactly the levels female and male.")

# 4. Counts are non-negative integers.
bad_n <- fels_attainment |> filter(is.na(n) | n < 0L)
if (nrow(bad_n) > 0L) {
  print(bad_n |> select(tooth, sex, stage, n))
  cli_abort("Invalid participant counts.")
}
cli_alert_success("All attainment counts are non-negative integers.")

# 5. Soft check: girls form teeth faster, so female theta should sit
#    below male theta for most stages. M3 is the documented exception
#    (Šešelj et al.: boys younger). A wholesale reversal would indicate
#    the Girls and Boys column blocks were swapped during parsing.
sex_order <- fels_attainment |>
  select(tooth, sex, stage, theta) |>
  pivot_wider(names_from = sex, values_from = theta) |>
  summarize(
    n_stages = n(),
    n_female_earlier = sum(female < male),
    .by = tooth
  ) |>
  mutate(prop_female_earlier = n_female_earlier / n_stages)

print(sex_order)

non_m3 <- sex_order |> filter(tooth != "M3")
if (any(non_m3$prop_female_earlier < 0.5)) {
  cli_alert_warning(
    "A non-M3 tooth has female theta above male theta for most \\
     stages; check for a Girls/Boys column swap."
  )
} else {
  cli_alert_success(
    "Female attainment precedes male for most stages in all teeth \\
     except M3, as expected."
  )
}

# 6. Age summaries increase monotonically with stage within a tooth
#    and sex: later formation stages are reached at older ages. This
#    is the only independent check available for the four Table 13a
#    rows that were assigned by column position and carry no
#    log-normal parameters to reconcile (girls' M3 R3_4 and Rc), so it
#    is what guards against a silent column shift there.
check_monotone <- function(reference, cols, label) {
  violations <- reference |>
    arrange(tooth, sex, stage_index) |>
    select(tooth, sex, stage, stage_index, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "quantity") |>
    filter(!is.na(value)) |>
    arrange(tooth, sex, quantity, stage_index) |>
    mutate(drop = value < lag(value), .by = c(tooth, sex, quantity)) |>
    filter(drop)

  if (nrow(violations) > 0L) {
    print(violations, n = 30)
    cli_abort("{label}: age summaries decrease with advancing stage.")
  }
  cli_alert_success(
    "{label}: {paste(cols, collapse = ', ')} increase with stage in \\
     every tooth and sex."
  )
  return(invisible(NULL))
}

check_monotone(
  fels_attainment,
  c("mode", "median", "mean"),
  "attainment"
)
check_monotone(
  fels_age_given_stage,
  c("mode", "median", "mean", "hpd_low", "opt", "hpd_high"),
  "age-given-stage"
)

# 7. M1's negative theta values survived Unicode-minus normalization.
m1_ci <- fels_attainment |>
  filter(tooth == "M1", stage == "Ci") |>
  select(sex, theta)
stopifnot(all(m1_ci$theta < 0))
cli_alert_success(
  "M1 Ci theta is negative for both sexes \\
   ({paste(m1_ci$theta, collapse = ', ')})."
)

# 8. The boys' M1 A1/2 age-given-stage parameter must be 2.2621, the
#    value printed in both Table 11a and the worked example on p. 16.
#    This is the value a 6/8 image misread corrupted during planning;
#    see the rationale in extract-text-layers.R.
m1_male_a12 <- fels_age_given_stage |>
  filter(tooth == "M1", sex == "male", stage == "A1_2") |>
  pull(ln_mu)
stopifnot(isTRUE(all.equal(m1_male_a12, 2.2621)))
cli_alert_success("M1 male A1_2 ln_mu is 2.2621, as printed.")

# ---------------------------------------------------------------------
# QA: tie detection (see PLAN.md §3.3)
# ---------------------------------------------------------------------

cli_h1("Tie detection")

fels_ties <- fels_attainment |>
  arrange(tooth, sex, stage_index) |>
  mutate(
    prev_stage = lag(stage),
    prev_theta = lag(theta),
    .by = c(tooth, sex)
  ) |>
  filter(!is.na(prev_theta), theta == prev_theta) |>
  transmute(
    tooth,
    sex,
    stage_lo = prev_stage,
    stage_hi = stage,
    theta,
    tie_class = if_else(
      stage_hi == "Ac", "terminal", "interior"
    )
  )

print(fels_ties)

cli_alert_info(
  "Found {nrow(fels_ties)} tied adjacent stage{?s}."
)

# Exactly two ties, both female M3. The terminal one (A1_2 / Ac) means
# that for girls, observing M3 Ac adds no information beyond A1_2 under
# the fitted model; PLAN.md §3.3 explains why that matters downstream.
stopifnot(nrow(fels_ties) == 2L)
stopifnot(all(fels_ties$tooth == "M3"))
stopifnot(all(fels_ties$sex == "female"))
stopifnot(identical(sort(fels_ties$tie_class), c("interior", "terminal")))
cli_alert_success(
  "Exactly two ties, both female M3: one interior, one terminal."
)

# Explicit regression guard: males have no ties, and male M3 R1_2 and
# R3_4 are distinct (2.7743 vs 2.8080).
male_m3 <- fels_attainment |>
  filter(tooth == "M3", sex == "male", stage %in% c("R1_2", "R3_4")) |>
  pull(theta)
stopifnot(!isTRUE(all.equal(male_m3[1], male_m3[2])))
cli_alert_success(
  "Male M3 R1_2 ({male_m3[1]}) and R3_4 ({male_m3[2]}) are distinct."
)

# ---------------------------------------------------------------------
# Write CSVs
# ---------------------------------------------------------------------

cli_h1("Writing CSV files")

write_tooth_csv <- function(reference, kind_label) {
  walk(unique(reference$tooth), function(tooth_i) {
    out_file <- path(
      out_dir,
      glue("{kind_label}-{str_to_lower(tooth_i)}"),
      ext = "csv"
    )
    reference |>
      filter(tooth == tooth_i) |>
      write_csv(out_file, na = "")
    cli_alert_success("Wrote {.path {path_file(out_file)}}")
  })
  return(invisible(NULL))
}

write_tooth_csv(fels_attainment, "attainment")
write_tooth_csv(fels_age_given_stage, "age-given-stage")

write_csv(fels_attainment, path(out_dir, "fels-attainment.csv"), na = "")
write_csv(
  fels_age_given_stage,
  path(out_dir, "fels-age-given-stage.csv"),
  na = ""
)
write_csv(fels_ties, path(out_dir, "fels-ties.csv"), na = "")

cli_alert_success("Wrote combined CSVs and the tie registry.")

cli_h1("Summary")
cli_alert_info(
  "fels_attainment: {nrow(fels_attainment)} rows, \\
   {n_distinct(fels_attainment$tooth)} teeth, 2 sexes."
)
cli_alert_info(
  "fels_age_given_stage: {nrow(fels_age_given_stage)} rows, \\
   {sum(is.na(fels_age_given_stage$ln_mu))} without log-normal \\
   parameters."
)
cli_alert_info("fels_ties: {nrow(fels_ties)} tied stage pairs.")

# ---------------------------------------------------------------------
# Adapter: re-key to the AgeFromDentition vocabulary
# ---------------------------------------------------------------------

# Everything above works in the vocabulary of the source tables. The
# package itself uses different names, and existing code compares
# against them directly, so the shipped datasets are re-keyed here
# rather than the package being rewritten around the source names.
#
# Stage is stored as **character**, not an ordered factor: AgeTables$Stage
# is character and validate_score() compares against it with %in%.

cli_h1("Re-keying to package vocabulary")

stage_map <- c(
  Ci = "C.i", Cco = "C.co", Coc = "C.oc", Cr1_2 = "Cr.5",
  Cr3_4 = "Cr.75", Crc = "Cr.c", Ri = "R.i", Cli = "Cl.i",
  R1_4 = "R.25", R1_2 = "R.5", R3_4 = "R.75", Rc = "R.c",
  A1_2 = "A.5", Ac = "Ac"
)

tooth_map <- c(C = "Canine", P3 = "P3", P4 = "P4",
               M1 = "M1", M2 = "M2", M3 = "M3")

sex_map <- c(female = "F", male = "M")

# Row order is Tooth (anatomical), then Sex, then stage_index
# (developmental). It reproduces the order AgeTables has shipped in
# since v0.1, which lets the reproduction check below compare row by row
# and so verify ordering as well as values.
#
# Do not sort on Stage itself. It is character, so sorting it would give
# alphabetical order -- A.5 before C.i -- which scrambles the
# developmental sequence the tables are meant to be read in.
tooth_order <- c("Canine", "P3", "P4", "M1", "M2", "M3")

rekey <- function(reference) {
  out <- reference |>
    mutate(
      Sex = unname(sex_map[as.character(sex)]),
      Tooth = unname(tooth_map[tooth]),
      Stage = unname(stage_map[as.character(stage)])
    )
  if (anyNA(out$Sex) || anyNA(out$Tooth) || anyNA(out$Stage)) {
    cli_abort("Re-keying produced NA in Sex, Tooth, or Stage.")
  }
  return(out |> arrange(match(Tooth, tooth_order), Sex, stage_index))
}

# Age given stage (Tables 8-13). log_mu is the mean log age *given* that
# the tooth is observed in the stage; log_sd is its log-scale SD.
AgeTables <- fels_age_given_stage |>
  rekey() |>
  transmute(Sex, Tooth, Stage, log_mu = ln_mu, log_sd = ln_sigma) |>
  as.data.frame()

# Ages of attainment (Tables 2-7). log_mu is the mean log age at
# *entering* the stage -- a different quantity from AgeTables$log_mu.
# se_log_mu is required downstream: the predictive completion threshold
# widens log_sd by it (PLAN.md D4), which is what distinguishes the
# well-estimated transitions from M3's n = 1 and n = 5 terminal rows.
AttainmentTables <- fels_attainment |>
  rekey() |>
  transmute(
    Sex, Tooth, Stage,
    log_mu = theta,
    log_sd = ln_sd,
    se_log_mu = se_theta,
    n
  ) |>
  as.data.frame()

# Tied adjacent transitions. A terminal tie means the stage below Ac and
# Ac itself are fitted at the same age, so observing Ac adds nothing
# beyond the preceding stage (PLAN.md §3.3).
StageTies <- fels_ties |>
  mutate(
    Sex = unname(sex_map[as.character(sex)]),
    Tooth = unname(tooth_map[tooth]),
    Stage_lo = unname(stage_map[as.character(stage_lo)]),
    Stage_hi = unname(stage_map[as.character(stage_hi)])
  ) |>
  transmute(Sex, Tooth, Stage_lo, Stage_hi, log_mu = theta, tie_class) |>
  as.data.frame()

# ---------------------------------------------------------------------
# QA: the adapter must be a no-op for data already shipped
# ---------------------------------------------------------------------

# AgeTables has been shipped since v0.1. Regenerating it from the
# extraction must reproduce it exactly, or the extraction is wrong
# somewhere the identity checks above did not reach. This is the single
# most informative check in the file: it ties 150 independently parsed
# values to a dataset that predates this pipeline.
shipped_path <- path(project_root, "data", "AgeTables.rda")

if (file_exists(shipped_path)) {
  shipped_env <- new.env()
  load(shipped_path, envir = shipped_env)
  shipped <- get("AgeTables", envir = shipped_env) |>
    as.data.frame()
  rownames(shipped) <- NULL

  regenerated <- AgeTables
  rownames(regenerated) <- NULL

  # Compared as-is, with no re-sorting on either side, so this checks
  # row order as well as values.
  identical_check <- all.equal(shipped, regenerated)
  if (!isTRUE(identical_check)) {
    print(identical_check)
    cli_abort(
      "Regenerated AgeTables differs from the shipped dataset. Resolve \\
       before overwriting: the shipped values are the ones every \\
       existing result was computed from."
    )
  }
  cli_alert_success(
    "Regenerated AgeTables is identical to the shipped dataset \\
     ({nrow(shipped)} rows)."
  )
} else {
  cli_alert_warning(
    "No shipped AgeTables found; skipping the reproduction check."
  )
}

# Structural assertions on the new datasets.
stopifnot(nrow(AttainmentTables) == 162L)
stopifnot(sum(AttainmentTables$Stage == "Ac") == 12L)
stopifnot(!anyNA(AttainmentTables$log_mu))
stopifnot(!anyNA(AttainmentTables$log_sd))
stopifnot(!anyNA(AttainmentTables$se_log_mu))
stopifnot(all(AttainmentTables$n > 0L))
cli_alert_success(
  "AttainmentTables: 162 rows, 12 Ac transitions, no missing parameters."
)

stopifnot(nrow(AgeTables) == 150L)
stopifnot(sum(is.na(AgeTables$log_mu)) == 4L)
cli_alert_success("AgeTables: 150 rows, 4 without log-normal parameters.")

stopifnot(nrow(StageTies) == 2L)
stopifnot(all(StageTies$Tooth == "M3"))
stopifnot(all(StageTies$Sex == "F"))
stopifnot(setequal(StageTies$tie_class, c("interior", "terminal")))
cli_alert_success("StageTies: 2 ties, both female M3.")

# AttainmentTables must cover every AgeTables key, adding only the 12 Ac
# rows. If it did not, a tooth could be scorable but have no transition
# to derive a threshold from.
ags_keys <- AgeTables |> select(Sex, Tooth, Stage)
att_keys <- AttainmentTables |> select(Sex, Tooth, Stage)
stopifnot(nrow(anti_join(ags_keys, att_keys,
                         by = c("Sex", "Tooth", "Stage"))) == 0L)
extra <- anti_join(att_keys, ags_keys, by = c("Sex", "Tooth", "Stage"))
stopifnot(nrow(extra) == 12L, all(extra$Stage == "Ac"))
cli_alert_success(
  "AttainmentTables is a strict superset of AgeTables, adding only Ac."
)

# ---------------------------------------------------------------------
# Write package data objects
# ---------------------------------------------------------------------

cli_h1("Writing package data")

# ExampleScores is re-serialized only, so that all four shipped
# datasets use the same format. Its contents are deliberately untouched.
example_env <- new.env()
load(path(project_root, "data", "ExampleScores.rda"), envir = example_env)
ExampleScores <- get("ExampleScores", envir = example_env)

usethis::use_data(
  AgeTables, AttainmentTables, StageTies, ExampleScores,
  overwrite = TRUE,
  compress = "bzip2",
  version = 3
)

cli_h1("Summary")
cli_alert_info("AgeTables: {nrow(AgeTables)} rows.")
cli_alert_info("AttainmentTables: {nrow(AttainmentTables)} rows.")
cli_alert_info("StageTies: {nrow(StageTies)} rows.")
cli_alert_info("ExampleScores: {nrow(ExampleScores)} rows (re-serialized).")
