all_ac_female <- function(m3 = NA) {
  data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
             M1 = "Ac", M2 = "Ac", M3 = m3)
}

test_that("all six teeth at Ac gives no estimate and an M3 threshold", {
  est <- estimate_dental_age(all_ac_female("Ac"), verbose = FALSE)
  info <- ac_info(est)

  expect_true(is.na(est["dental_age"]))
  expect_true(is.na(est["log_age"]))
  expect_true(is.na(est["ci_lower"]))
  expect_true(is.na(est["ci_upper"]))

  expect_equal(info$compatibility, "completion_threshold_only")
  expect_equal(info$binding_tooth, "M3")
  expect_equal(round(info$threshold, 3), 14.107)
  expect_true(info$low_precision)
  expect_true(info$tied_transition)
  expect_length(info$terminal_teeth, 6L)
})

test_that("no value anywhere in an all-Ac result is near 18", {
  # The failure this feature exists to prevent: treating terminal stages
  # as evidence of age 18.
  est <- estimate_dental_age(all_ac_female("Ac"), verbose = FALSE)
  info <- ac_info(est)

  numbers <- c(
    as.numeric(est),
    info$threshold,
    info$per_tooth$threshold
  )
  numbers <- numbers[!is.na(numbers)]

  expect_true(all(numbers < 16))
  expect_false(any(abs(numbers - 18) < 1))

  # Nor may the printed report contain it.
  expect_false(grepl("18", info$message))
})

test_that("M3 unscored moves the binding tooth to M2", {
  est <- estimate_dental_age(all_ac_female(NA), verbose = FALSE)
  info <- ac_info(est)

  expect_true(is.na(est["dental_age"]))
  expect_equal(info$compatibility, "completion_threshold_only")
  expect_equal(info$binding_tooth, "M2")
  expect_equal(round(info$threshold, 3), 12.194)
  expect_false(info$low_precision)
  expect_false(info$tied_transition)
  expect_equal(info$missing_teeth, "M3")
})

test_that("an unscored M3 is never treated as complete", {
  # The two differ by nearly two years, so conflating them is not subtle.
  with_m3 <- ac_info(estimate_dental_age(all_ac_female("Ac"),
                                         verbose = FALSE))
  without_m3 <- ac_info(estimate_dental_age(all_ac_female(NA),
                                            verbose = FALSE))

  expect_gt(with_m3$threshold, without_m3$threshold)
  expect_false("M3" %in% without_m3$terminal_teeth)
  expect_true("M3" %in% without_m3$missing_teeth)
})

test_that("with only the early teeth complete, P4 binds", {
  x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                  M1 = "Ac", M2 = NA, M3 = NA)
  info <- ac_info(estimate_dental_age(x, verbose = FALSE))

  expect_equal(info$binding_tooth, "P4")
  expect_equal(round(info$threshold, 3), 11.674)
})

test_that("a mixed case gives a normal estimate flagged compatible", {
  x <- data.frame(Sex = "F", Canine = "R.c", P3 = "R.c", P4 = "R.75",
                  M1 = "Ac", M2 = "R.5", M3 = NA)
  est <- estimate_dental_age(x, verbose = FALSE)
  info <- ac_info(est)

  expect_false(is.na(est["dental_age"]))
  expect_equal(info$terminal_teeth, "M1")
  expect_equal(info$compatibility, "compatible")
  expect_gte(as.numeric(est["ci_lower"]), info$threshold)
})

test_that("an implausibly young estimate is flagged discordant", {
  # M2 complete, but every other tooth scored at an early crown stage.
  x <- data.frame(Sex = "F", Canine = "C.i", P3 = "C.i", P4 = "C.i",
                  M1 = "C.co", M2 = "Ac", M3 = NA)

  expect_warning(
    est <- estimate_dental_age(x, verbose = FALSE),
    "lies below the Ac completion threshold"
  )

  info <- ac_info(est)
  expect_equal(info$compatibility, "discordant")
  expect_lt(as.numeric(est["ci_upper"]), info$threshold)
})

test_that("the discordance warning does not assert a cause", {
  x <- data.frame(Sex = "F", Canine = "C.i", P3 = "C.i", P4 = "C.i",
                  M1 = "C.co", M2 = "Ac", M3 = NA)
  warning_text <- tryCatch(
    {
      estimate_dental_age(x, verbose = FALSE)
      ""
    },
    warning = function(w) conditionMessage(w)
  )

  # It may suggest checks, but must not declare the input wrong: the two
  # models being compared are related but not the same.
  expect_match(warning_text, "reference-sample variation")
  expect_match(warning_text, "separate-estimator stopgap")
  expect_false(grepl("data.entry error", warning_text))
})

test_that("no terminal teeth means no threshold and no flag", {
  est <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)
  info <- ac_info(est)

  expect_equal(info$compatibility, "no_terminal_information")
  expect_true(is.na(info$threshold))
  expect_length(info$terminal_teeth, 0L)
})

test_that("the compatibility boundaries are inclusive below, exclusive above", {
  # Constructed directly, since a real scored row rarely lands on an
  # endpoint. Documented convention: >= lower is compatible, < upper is
  # discordant.
  expect_equal(
    classify_compatibility(10, 12, 10, n_terminal = 1, n_estimable = 3),
    "compatible"
  )
  expect_equal(
    classify_compatibility(10, 12, 12, n_terminal = 1, n_estimable = 3),
    "overlap"
  )
  expect_equal(
    classify_compatibility(10, 12, 12.001, n_terminal = 1, n_estimable = 3),
    "discordant"
  )
  expect_equal(
    classify_compatibility(10, 12, 11, n_terminal = 1, n_estimable = 3),
    "overlap"
  )
})

test_that("q and method carry through the estimator to the threshold", {
  x <- all_ac_female("Ac")

  default <- ac_info(estimate_dental_age(x, verbose = FALSE))
  plugin <- ac_info(estimate_dental_age(x, method = "plugin",
                                        verbose = FALSE))
  wider <- ac_info(estimate_dental_age(x, q = 0.05, verbose = FALSE))

  expect_equal(round(default$threshold, 3), 14.107)
  expect_equal(round(plugin$threshold, 3), 14.596)
  expect_gt(wider$threshold, default$threshold)

  expect_equal(default$method, "predictive")
  expect_equal(plugin$method, "plugin")
  expect_equal(wider$q, 0.05)
})

test_that("male and female thresholds differ for the same scores", {
  female <- ac_info(estimate_dental_age(all_ac_female("Ac"),
                                        verbose = FALSE))
  male_scores <- all_ac_female("Ac")
  male_scores$Sex <- "M"
  male <- ac_info(estimate_dental_age(male_scores, verbose = FALSE))

  expect_gt(male$threshold, female$threshold)
  expect_false(male$tied_transition)
})

test_that("teeth are classified into the four categories", {
  # M3 = R.75 for a female is scored but has no published parameters.
  x <- data.frame(Sex = "F", Canine = "R.c", P3 = "R.c", P4 = "Ac",
                  M1 = "C.i", M2 = NA, M3 = "R.75")
  info <- ac_info(estimate_dental_age(x, verbose = FALSE))

  expect_equal(info$terminal_teeth, "P4")
  expect_equal(info$unparameterized_teeth, "M3")
  expect_setequal(info$missing_teeth, c("M1", "M2"))
  expect_equal(info$n_estimable, 2L)
})

test_that("anatomically impossible stages are rejected, not dropped", {
  # Cl.i does not exist for single-rooted teeth. Before the tooth-aware
  # check it looked up to NA and the tooth vanished from the estimate.
  x <- data.frame(Sex = "F", Canine = "Cl.i", P3 = "R.c", P4 = "R.c",
                  M1 = "R.c", M2 = "R.5", M3 = NA)

  expect_error(
    estimate_dental_age(x, verbose = FALSE),
    "Invalid stage.*Cl\\.i.*for.*Canine"
  )
})

test_that("verbose messages distinguish the exclusion reasons", {
  x <- data.frame(Sex = "F", Canine = "R.c", P3 = "R.c", P4 = "Ac",
                  M1 = "C.i", M2 = NA, M3 = "R.75")

  # Captured in one run, so that each reason is checked against the same
  # set of messages rather than re-triggering them.
  messages <- capture_messages(prepare_scores(x, verbose = TRUE))
  combined <- paste(messages, collapse = "")

  expect_match(combined, "terminal stage Ac \\(P4\\)")
  expect_match(combined, "M1 is stage C.i or missing")
  expect_match(combined, "M3 stage R.75 has no published log-normal")

  expect_silent(prepare_scores(x, verbose = FALSE))
})
