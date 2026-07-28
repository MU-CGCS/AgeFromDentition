# Build the AgeFromDentition reference data from the Šešelj et al. (2019)
# Fels Longitudinal Study tables.
#
#   Tables 2-7  -> ages of attainment (transition analysis)
#   Tables 8-13 -> ages given stage
#
# Only panel (a) of each table is represented: the sex-specific Girls and
# Boys columns. Panel (b), the combined-sex sample, was deliberately not
# transcribed, because sex is always known in the intended use.
#
# ---------------------------------------------------------------------
# Source of record
# ---------------------------------------------------------------------
#
# This script reads two CSV files:
#
#   fels-attainment.csv        Tables 2-7,  162 rows
#   fels-age-given-stage.csv   Tables 8-13, 150 rows
#
# **Those two files are the archive.** They were transcribed from the
# source article's text layer by a parser that has since been removed,
# along with the article PDF itself. There is no longer any upstream
# artifact to regenerate them from, so treat them as read-only: this
# script never writes to them, and nothing else should either.
#
# Everything else under data-raw/ is derived from them on every run and
# can be deleted and rebuilt at will.
#
# The QA sections below are what makes that arrangement safe. Every row
# is checked against closed-form log-normal identities relating the
# parameters to the published mode, median, mean and SD columns. Those
# identities are exact, so corruption of the archive is caught and
# localized rather than propagated into data/.

library(fs)
library(glue)
library(cli)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)

project_root <- path_wd()
raw_dir <- path(project_root, "data-raw")

# ---------------------------------------------------------------------
# Stage vocabulary
# ---------------------------------------------------------------------

# "crypt" (Šešelj et al.'s stage zero) has no attainment row; it is the
# open interval below Ci and is handled in the likelihood, not in the
# reference tables.
stage_levels_molar <- c(
  "Ci", "Cco", "Coc", "Cr1_2", "Cr3_4", "Crc", "Ri", "Cli",
  "R1_4", "R1_2", "R3_4", "Rc", "A1_2", "Ac"
)

# Single-rooted teeth omit Cli (root cleft initiation).
stage_levels_single <- setdiff(stage_levels_molar, "Cli")

teeth <- c("C", "P3", "P4", "M1", "M2", "M3")

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
# Read the archive
# ---------------------------------------------------------------------

cli_h1("Reading reference CSVs")

read_reference <- function(file, required) {
  path_csv <- path(raw_dir, file)
  if (!file_exists(path_csv)) {
    cli_abort(c(
      "Reference file {.path {file}} not found.",
      i = "It is the source of record and cannot be regenerated."
    ))
  }

  out <- read_csv(path_csv, show_col_types = FALSE) |>
    mutate(
      sex = factor(sex, levels = c("female", "male")),
      stage = factor(stage, levels = stage_levels_molar)
    ) |>
    arrange(tooth, sex, stage_index)

  missing_cols <- setdiff(required, names(out))
  if (length(missing_cols) > 0L) {
    cli_abort(
      "{.path {file}} is missing column{?s}: \\
       {paste(missing_cols, collapse = ', ')}."
    )
  }
  if (anyNA(out$stage)) {
    cli_abort("{.path {file}} contains an unrecognized stage label.")
  }

  cli_alert_info("{.path {file}}: {nrow(out)} rows")
  return(out)
}

fels_attainment <- read_reference(
  "fels-attainment.csv",
  c("tooth", "sex", "stage", "stage_index", "theta", "se_theta", "n",
    "mode", "median", "mean", "ln_sd", "se_ln_sd")
) |>
  mutate(n = as.integer(n))

fels_age_given_stage <- read_reference(
  "fels-age-given-stage.csv",
  c("tooth", "sex", "stage", "stage_index", "ln_mu", "ln_sigma", "mode",
    "median", "mean", "sd", "hpd_low", "opt", "hpd_high")
)

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

# Rows where the published parameters are absent. In Table 13a these are
# real: girls' M3 R1/2 and A1/2 have zero-width stages (tied
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
kinds <- expand_grid(
  tooth = teeth,
  kind = c("attainment", "age_given_stage")
)

pwalk(kinds, function(tooth_i, kind_i) {
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
}, .progress = FALSE)
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
#    the Girls and Boys column blocks were swapped.
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

# 6. Age summaries increase monotonically with stage within a tooth and
#    sex: later formation stages are reached at older ages. This is the
#    only independent check available for the four Table 13a rows that
#    carry no log-normal parameters to reconcile (girls' M3 R3_4 and
#    Rc), so it is what guards against a silent column shift there.
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

# 7. M1's theta is negative at Ci in both sexes -- the tooth begins
#    forming before one year of age. A lost minus sign would be
#    invisible to every other check here, because the series would still
#    be monotone.
m1_ci <- fels_attainment |>
  filter(tooth == "M1", stage == "Ci") |>
  select(sex, theta)
stopifnot(all(m1_ci$theta < 0))
cli_alert_success(
  "M1 Ci theta is negative for both sexes \\
   ({paste(m1_ci$theta, collapse = ', ')})."
)

# 8. The boys' M1 A1/2 age-given-stage parameter must be 2.2621. Šešelj
#    et al. print it twice -- in Table 11a and again in the worked
#    example on p. 16 -- so it is the one value in the tables with an
#    independent published cross-check.
m1_male_a12 <- fels_age_given_stage |>
  filter(tooth == "M1", sex == "male", stage == "A1_2") |>
  pull(ln_mu)
stopifnot(isTRUE(all.equal(m1_male_a12, 2.2621)))
cli_alert_success("M1 male A1_2 ln_mu is 2.2621, as printed.")

# ---------------------------------------------------------------------
# QA: tie detection
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
# Adapter: re-key to the AgeFromDentition vocabulary
# ---------------------------------------------------------------------

# Everything above works in the vocabulary of the source tables. The
# package itself uses different names, and existing code compares
# against them directly, so the shipped datasets are re-keyed here
# rather than the package being rewritten around the source names.
#
# Stage is stored as **character**, not an ordered factor: AgeTables$Stage
# is character and validate_score() compares against it.

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

# AgeTables has been shipped since v0.1. Regenerating it must reproduce
# it exactly, or something has gone wrong that the identity checks above
# did not reach. This is the single most informative check in the file:
# it ties 150 values in the archive to a dataset that predates it.
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
# Write derived files
# ---------------------------------------------------------------------

cli_h1("Writing derived CSVs")

# Per-tooth views of the archive, for reading. Regenerated every run so
# they cannot drift from the combined files they come from.
#
# The two combined files are NOT written here: they are the source of
# record, and this script must never be able to overwrite them.
write_tooth_csv <- function(reference, kind_label) {
  walk(unique(reference$tooth), function(tooth_i) {
    out_file <- path(
      raw_dir,
      glue("{kind_label}-{tolower(tooth_i)}"),
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

write_csv(fels_ties, path(raw_dir, "fels-ties.csv"), na = "")
cli_alert_success("Wrote {.path fels-ties.csv} (derived tie registry).")

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
