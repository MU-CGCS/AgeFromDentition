# Frozen outputs of the v0.1 estimator, captured before any terminal-stage
# work began. None of the ExampleScores rows contains an Ac stage, so these
# values must survive every change made for Ac handling. If one of them
# moves, the refactor has altered the estimator rather than extending it.

test_that("estimate_dental_age reproduces the v0.1 values exactly", {
  expected_log_age <- c(
    2.209567291566, 1.594432601493, 1.644552701627,
    2.177143382444, 2.298981747836
  )
  expected_total_var <- c(
    0.008394728163, 1.141043452770, 1.160062779864,
    0.009551565069, 0.061027130217
  )
  expected_dental_age <- c(
    9.035602106673, 1.573636445989, 1.623346763772,
    8.737217869061, 9.374137968651
  )

  for (i in seq_len(nrow(ExampleScores))) {
    means <- get_means_for_scores(ExampleScores[i, ], verbose = FALSE)
    est <- estimate_dental_age(means, verbose = FALSE)

    expect_equal(
      as.numeric(est["log_age"]), expected_log_age[i],
      tolerance = 1e-10
    )
    expect_equal(
      as.numeric(est["log_total_var"]), expected_total_var[i],
      tolerance = 1e-10
    )
    expect_equal(
      as.numeric(est["dental_age"]), expected_dental_age[i],
      tolerance = 1e-10
    )
  }
})

test_that("get_means_for_scores is unchanged for every ExampleScores row", {
  # The lookup table itself, not just the estimate derived from it.
  for (i in seq_len(nrow(ExampleScores))) {
    means <- get_means_for_scores(ExampleScores[i, ], verbose = FALSE)

    expect_s3_class(means, "data.frame")
    expect_equal(dim(means), c(6L, 2L))
    expect_named(means, c("log_mu", "log_sd"))
    expect_equal(
      rownames(means),
      c("Canine", "P3", "P4", "M1", "M2", "M3")
    )
  }

  # Row 5 has M1 = "C.i", which the estimator drops on purpose.
  means_5 <- get_means_for_scores(ExampleScores[5, ], verbose = FALSE)
  expect_true(is.na(means_5["M1", "log_mu"]))
})
