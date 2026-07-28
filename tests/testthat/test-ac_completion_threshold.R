# Both threshold conventions are pinned to three decimal places. Neither may
# drift into the other: the predictive and plug-in values differ by less than
# 0.06 years for ten of the twelve teeth, so a silent swap would be almost
# invisible everywhere except M3 -- which is exactly the tooth that binds.

expected_predictive <- c(
  "F Canine" = 9.881, "F P3" = 10.865, "F P4" = 11.674,
  "F M1" = 7.730, "F M2" = 12.194, "F M3" = 14.107,
  "M Canine" = 10.964, "M P3" = 11.475, "M P4" = 11.747,
  "M M1" = 8.088, "M M2" = 11.726, "M M3" = 14.690
)

expected_plugin <- c(
  "F Canine" = 9.941, "F P3" = 10.903, "F P4" = 11.712,
  "F M1" = 7.739, "F M2" = 12.229, "F M3" = 14.596,
  "M Canine" = 11.001, "M P3" = 11.504, "M P4" = 11.780,
  "M M1" = 8.097, "M M2" = 11.756, "M M3" = 14.994
)

test_that("predictive thresholds match the published reference values", {
  for (key in names(expected_predictive)) {
    parts <- strsplit(key, " ", fixed = TRUE)[[1]]
    result <- ac_completion_threshold(parts[1], parts[2])

    expect_equal(
      round(result$threshold, 3), unname(expected_predictive[key]),
      info = key
    )
  }
})

test_that("plug-in thresholds match the published reference values", {
  for (key in names(expected_plugin)) {
    parts <- strsplit(key, " ", fixed = TRUE)[[1]]
    result <- ac_completion_threshold(parts[1], parts[2], method = "plugin")

    expect_equal(
      round(result$threshold, 3), unname(expected_plugin[key]),
      info = key
    )
  }
})

test_that("the default is predictive, not plug-in", {
  # The single most consequential guard in this file. Female M3 is where the
  # two conventions diverge most, so it is the one worth naming explicitly.
  result <- ac_completion_threshold("F", "M3")

  expect_equal(round(result$threshold, 3), 14.107)
  expect_false(isTRUE(all.equal(round(result$threshold, 3), 14.596)))
  expect_equal(result$method, "predictive")
})

test_that("predictive is never larger than plug-in", {
  # Adding parameter uncertainty widens the distribution, so the lower-tail
  # percentile can only move down.
  for (sex in c("F", "M")) {
    for (tooth in c("Canine", "P3", "P4", "M1", "M2", "M3")) {
      pred <- ac_completion_threshold(sex, tooth)$threshold
      plug <- ac_completion_threshold(sex, tooth,
                                      method = "plugin")$threshold
      expect_lte(pred, plug)
    }
  }
})

test_that("sd_eff distinguishes the two methods for every tooth", {
  teeth <- c("Canine", "P3", "P4", "M1", "M2", "M3")

  pred <- ac_completion_threshold("F", teeth)$per_tooth
  plug <- ac_completion_threshold("F", teeth, method = "plugin")$per_tooth

  expect_true(all(pred$sd_eff > plug$sd_eff))
  expect_equal(plug$sd_eff, plug$log_sd)
})

test_that("q and method are echoed back in the result", {
  result <- ac_completion_threshold("M", "M2", q = 0.05, method = "plugin")

  expect_equal(result$q, 0.05)
  expect_equal(result$method, "plugin")
})

test_that("a smaller lower tail gives a larger threshold", {
  q_025 <- ac_completion_threshold("F", "M2", q = 0.025)$threshold
  q_050 <- ac_completion_threshold("F", "M2", q = 0.05)$threshold

  expect_gt(q_050, q_025)
  expect_equal(round(q_050, 3), 12.598)
})

test_that("the largest per-tooth threshold binds", {
  result <- ac_completion_threshold("F", c("M1", "M2", "M3"))

  expect_equal(result$binding_tooth, "M3")
  expect_equal(result$threshold, max(result$per_tooth$threshold))
})

test_that("the binding tooth is not always a molar", {
  # With only the earlier-forming teeth complete, P4 binds -- not the
  # canine, which is the intuitive but wrong answer.
  result <- ac_completion_threshold("F", c("Canine", "P3", "P4", "M1"))

  expect_equal(result$binding_tooth, "P4")
  expect_equal(round(result$threshold, 3), 11.674)
})

test_that("the result does not depend on the order teeth are supplied", {
  teeth <- c("Canine", "P3", "P4", "M1", "M2", "M3")
  reference <- ac_completion_threshold("F", teeth)

  set.seed(42)
  for (i in 1:20) {
    shuffled <- ac_completion_threshold("F", sample(teeth))

    expect_equal(shuffled$threshold, reference$threshold)
    expect_equal(shuffled$binding_tooth, reference$binding_tooth)
    expect_equal(shuffled$per_tooth, reference$per_tooth)
  }
})

test_that("per_tooth is returned in full, even for one tooth", {
  single <- ac_completion_threshold("F", "M2")

  expect_s3_class(single$per_tooth, "data.frame")
  expect_equal(nrow(single$per_tooth), 1L)
  expect_named(
    single$per_tooth,
    c("Tooth", "log_mu", "log_sd", "se_log_mu", "sd_eff", "n", "threshold")
  )

  several <- ac_completion_threshold("F", c("M1", "M2", "M3"))
  expect_equal(nrow(several$per_tooth), 3L)
})

test_that("no Ac teeth gives no threshold", {
  result <- ac_completion_threshold("F", character(0))

  expect_true(is.na(result$threshold))
  expect_true(is.na(result$binding_tooth))
  expect_equal(nrow(result$per_tooth), 0L)
  expect_false(result$low_precision)
  expect_false(result$tied_transition)
})

test_that("low precision is flagged for both M3 transitions only", {
  for (sex in c("F", "M")) {
    expect_true(ac_completion_threshold(sex, "M3")$low_precision)

    for (tooth in c("Canine", "P3", "P4", "M1", "M2")) {
      expect_false(ac_completion_threshold(sex, tooth)$low_precision)
    }
  }
})

test_that("the female M3 tie is flagged and the male one is not", {
  # For girls, A.5 and Ac are fitted at the same age, so apex closure adds
  # nothing beyond the preceding stage.
  expect_true(ac_completion_threshold("F", "M3")$tied_transition)
  expect_false(ac_completion_threshold("M", "M3")$tied_transition)

  # The flag follows the binding tooth, not merely the presence of M3.
  expect_true(
    ac_completion_threshold("F", c("M1", "M2", "M3"))$tied_transition
  )
  expect_false(
    ac_completion_threshold("F", c("M1", "M2"))$tied_transition
  )
})

test_that("flags never cause a tooth to be dropped", {
  # Excluding a low-precision binding tooth would understate the threshold
  # and discard the observation being reported on.
  with_m3 <- ac_completion_threshold("F", c("M2", "M3"))
  without_m3 <- ac_completion_threshold("F", "M2")

  expect_equal(nrow(with_m3$per_tooth), 2L)
  expect_equal(with_m3$binding_tooth, "M3")
  expect_gt(with_m3$threshold, without_m3$threshold)
})

test_that("invalid arguments are rejected", {
  expect_error(ac_completion_threshold("X", "M2"), "either")
  expect_error(ac_completion_threshold(c("F", "M"), "M2"), "single value")
  expect_error(ac_completion_threshold(NA, "M2"), "single value")

  expect_error(ac_completion_threshold("F", "M2", q = 0), "in \\(0, 0.5\\)")
  expect_error(ac_completion_threshold("F", "M2", q = 0.5), "in \\(0, 0.5\\)")
  expect_error(ac_completion_threshold("F", "M2", q = -0.1), "in \\(0, 0.5\\)")
  expect_error(
    ac_completion_threshold("F", "M2", q = c(0.025, 0.05)),
    "single number"
  )

  expect_error(ac_completion_threshold("F", "M2", method = "bayes"))

  expect_error(ac_completion_threshold("F", "Molar2"), "Unknown tooth name")
  expect_error(ac_completion_threshold("F", c("M2", "M2")), "duplicates")
  expect_error(ac_completion_threshold("F", c("M2", NA)), "must not contain NA")
})

test_that("thresholds are well below the age Ac is casually assumed to mean", {
  # Guards against the failure this whole feature exists to prevent:
  # treating terminal stages as evidence of age 18.
  all_teeth <- c("Canine", "P3", "P4", "M1", "M2", "M3")

  for (sex in c("F", "M")) {
    result <- ac_completion_threshold(sex, all_teeth)
    expect_lt(result$threshold, 16)
  }
})
