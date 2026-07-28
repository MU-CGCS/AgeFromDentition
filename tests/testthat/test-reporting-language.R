# The threshold is a percentile of a reference distribution. These tests
# guard the wording against the two ways it is easy to overclaim: dressing
# it up as a statement about the individual, and inventing a reason for a
# tooth that simply was not scored.

# The one sentence in which probability language may legitimately appear.
# It exists to deny the inference a reader arrives with -- "so it is a
# 97.5% lower bound on their age" -- which BACKGROUND.md itself made.
#
# Both wording tests below are defined against this single constant, so
# they cannot drift apart: the ban exempts exactly this text, and the
# requirement demands exactly this text. Rewriting the sentence into an
# affirmative claim therefore fails both at once, rather than slipping
# through a list of banned phrases that happens not to match the new
# wording.
DISCLAIMER <- paste(
  "This is a descriptive completion threshold for the reference",
  "sample, not a lower confidence limit for this individual."
)

all_reports <- function() {
  scored <- list(
    ordinary = ExampleScores[1, ],
    all_ac = data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                        M1 = "Ac", M2 = "Ac", M3 = "Ac"),
    all_ac_no_m3 = data.frame(Sex = "F", Canine = "Ac", P3 = "Ac",
                              P4 = "Ac", M1 = "Ac", M2 = "Ac", M3 = NA),
    mixed = data.frame(Sex = "M", Canine = "R.c", P3 = "R.c", P4 = "R.75",
                       M1 = "Ac", M2 = "R.5", M3 = NA),
    unparameterized = data.frame(Sex = "F", Canine = "R.c", P3 = "R.c",
                                 P4 = "Ac", M1 = "C.i", M2 = NA,
                                 M3 = "R.75")
  )

  reports <- lapply(scored, function(x) {
    est <- suppressWarnings(estimate_dental_age(x, verbose = FALSE))
    ac_info(est)$message
  })

  return(reports)
}

test_that("no report claims a confidence or credible bound", {
  # Strip the one legitimate use, then forbid the bare words in whatever
  # survives. A list of specific banned phrases would not do: "97.5%
  # confidence limit" matches neither "95% confidence" nor "confidence
  # interval", so an affirmative rewrite of the disclaimer would pass.
  forbidden <- "confidence|credible|posterior|probabilit"

  reports <- all_reports()
  for (name in names(reports)) {
    stripped <- gsub(DISCLAIMER, "", reports[[name]], fixed = TRUE)

    expect_false(
      grepl(forbidden, stripped, ignore.case = TRUE),
      info = paste0(name, ": probability language outside the disclaimer")
    )
  }
})

test_that("the ban catches an affirmative rewrite of the disclaimer", {
  # Guards the guard. If this ever passes, the test above has stopped
  # discriminating and the wording is unprotected.
  overclaim <- paste(
    "Reference completion threshold: 12.19 years.",
    "This is a 97.5% confidence limit for this individual."
  )
  stripped <- gsub(DISCLAIMER, "", overclaim, fixed = TRUE)

  expect_true(grepl("confidence|credible|posterior|probabilit", stripped,
                    ignore.case = TRUE))

  # And the real disclaimer is still exempt.
  expect_false(
    grepl("confidence|credible|posterior|probabilit",
          gsub(DISCLAIMER, "", DISCLAIMER, fixed = TRUE),
          ignore.case = TRUE)
  )
})

test_that("no report uses the phrase 'minimum age'", {
  for (report in all_reports()) {
    expect_false(grepl("minimum age", report, ignore.case = TRUE))
  }
})

test_that("no report infers a reason for an unscored tooth", {
  # The input format records presence or absence only, so every one of
  # these would be an invention.
  forbidden <- c(
    "not assessable", "unassessable", "agenesis", "extracted",
    "congenitally absent", "unscorable", "not radiographed"
  )

  for (report in all_reports()) {
    for (phrase in forbidden) {
      expect_false(grepl(phrase, report, ignore.case = TRUE))
    }
  }
})

test_that("unscored teeth are named plainly", {
  x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                  M1 = "Ac", M2 = "Ac", M3 = NA)
  report <- ac_info(estimate_dental_age(x, verbose = FALSE))$message

  expect_match(report, "Not scored: M3\\.")
})

test_that("every threshold report carries the disclaimer", {
  # The stronger half of the pair. A ban alone is satisfied by deleting
  # the sentence, which is the realistic failure: an editor trimming
  # verbose output would drop it long before anyone added a false claim.
  with_threshold <- c("all_ac", "all_ac_no_m3", "mixed", "unparameterized")

  reports <- all_reports()
  for (name in with_threshold) {
    expect_true(
      grepl(DISCLAIMER, reports[[name]], fixed = TRUE),
      info = paste0(name, ": disclaimer missing or altered")
    )
  }
})

test_that("the report names the convention that produced the number", {
  # Without q and method, the printed number cannot be reproduced from the
  # printed output alone.
  x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                  M1 = "Ac", M2 = "Ac", M3 = NA)

  default <- ac_info(estimate_dental_age(x, verbose = FALSE))$message
  expect_match(default, 'q = 0.025, method = "predictive"')
  expect_match(default, "predictive distribution")

  plugin <- ac_info(
    estimate_dental_age(x, method = "plugin", verbose = FALSE)
  )$message
  expect_match(plugin, 'q = 0.025, method = "plugin"')
  expect_match(plugin, "reference distribution")
  expect_false(grepl("predictive distribution", plugin))
})

test_that("the unstable-threshold caveat appears exactly when warranted", {
  unstable <- ac_info(estimate_dental_age(
    data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
               M1 = "Ac", M2 = "Ac", M3 = "Ac"),
    verbose = FALSE
  ))$message

  expect_match(unstable, "Threshold unstable")
  expect_match(unstable, "rests on 1 individual")
  expect_match(unstable, "tied with A\\.5")

  stable <- ac_info(estimate_dental_age(
    data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
               M1 = "Ac", M2 = "Ac", M3 = NA),
    verbose = FALSE
  ))$message

  expect_false(grepl("Threshold unstable", stable))
})

test_that("the interval is called central, never highest-density", {
  report <- ac_info(
    estimate_dental_age(ExampleScores[1, ], verbose = FALSE)
  )$message

  expect_match(report, "Central 95% interval")
  expect_false(grepl("HDI|highest.density|highest density", report,
                     ignore.case = TRUE))
})

test_that("print emits the report and returns invisibly", {
  est <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)

  expect_output(print(est), "Dental age: 9\\.04 years")

  # capture.output() swallows the report; expect_invisible() still sees
  # the visibility of the print() call itself.
  invisible(capture.output(expect_invisible(print(est))))
})
