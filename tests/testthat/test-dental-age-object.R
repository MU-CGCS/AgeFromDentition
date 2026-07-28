# The result stays a named numeric vector. A list would have broken
# as.numeric(), arithmetic, length(), and c() on every existing caller;
# these tests hold that line.

test_that("the result behaves as a numeric vector", {
  est <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)

  expect_true(is.numeric(est))
  expect_length(est, 5L)
  expect_named(
    est,
    c("log_age", "log_total_var", "dental_age", "ci_lower", "ci_upper")
  )

  expect_silent(as.numeric(est))
  expect_length(as.numeric(est), 5L)
  expect_silent(unname(est))
  expect_equal(as.numeric(est["dental_age"]) * 2,
               as.numeric(est["dental_age"]) + as.numeric(est["dental_age"]))
})

test_that("named extraction still works for downstream callers", {
  est <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)

  expect_false(is.na(as.numeric(est["log_age"])))
  expect_false(is.na(as.numeric(est["log_total_var"])))
  expect_false(is.na(as.numeric(est["dental_age"])))
})

test_that("the class and attribute are attached", {
  est <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)

  expect_s3_class(est, "dental_age")
  expect_type(ac_info(est), "list")
  expect_equal(ac_info(est)$ci_level, 0.95)
  expect_equal(ac_info(est)$ci_type, "central")
})

test_that("ac_info rejects objects it did not come from", {
  expect_error(ac_info(1:5), "must be a.*dental_age.*object")
  expect_error(ac_info(ExampleScores), "must be a.*dental_age.*object")
})

test_that("the central interval is analytic and reproducible", {
  # Two calls must agree exactly. If the interval were ever sourced from
  # the sampler, this would fail.
  a <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)
  b <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)

  expect_identical(as.numeric(a["ci_lower"]), as.numeric(b["ci_lower"]))
  expect_identical(as.numeric(a["ci_upper"]), as.numeric(b["ci_upper"]))

  # And it matches qlnorm() directly.
  expect_equal(
    as.numeric(a["ci_lower"]),
    stats::qlnorm(0.025, as.numeric(a["log_age"]),
                  sqrt(as.numeric(a["log_total_var"])))
  )
})

test_that("the compatibility code does not depend on the RNG", {
  x <- data.frame(Sex = "F", Canine = "R.c", P3 = "R.c", P4 = "R.75",
                  M1 = "Ac", M2 = "R.5", M3 = NA)

  set.seed(1)
  first <- ac_info(estimate_dental_age(x, verbose = FALSE))$compatibility
  set.seed(999)
  second <- ac_info(estimate_dental_age(x, verbose = FALSE))$compatibility
  third <- ac_info(estimate_dental_age(x, verbose = FALSE))$compatibility

  expect_equal(first, second)
  expect_equal(first, third)
})

test_that("the compatibility code is unaffected by the HDI interval", {
  # estimate_age_hdi()'s `interval` argument must have no bearing on the
  # flag, which is the point of computing a separate central interval.
  x <- data.frame(Sex = "F", Canine = "R.c", P3 = "R.c", P4 = "R.75",
                  M1 = "Ac", M2 = "R.5", M3 = NA)
  est <- estimate_dental_age(x, verbose = FALSE)
  before <- ac_info(est)$compatibility

  invisible(estimate_age_hdi(est, n = 1000, interval = 0.5))
  invisible(estimate_age_hdi(est, n = 1000, interval = 0.99))

  expect_equal(ac_info(est)$compatibility, before)
})

test_that("the samplers survive an estimate with no finite value", {
  x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
                  M1 = "Ac", M2 = "Ac", M3 = NA)
  est <- estimate_dental_age(x, verbose = FALSE)

  samples <- age_samples(est, 100)
  expect_length(samples, 100L)
  expect_true(all(is.na(samples)))

  hdi <- estimate_age_hdi(est, n = 1000)
  expect_true(is.na(hdi["lower_bound"]))
  expect_true(is.na(hdi["upper_bound"]))
})

test_that("estimate_age_hdi carries the threshold through", {
  x <- data.frame(Sex = "F", Canine = "R.c", P3 = "R.c", P4 = "R.75",
                  M1 = "Ac", M2 = "R.5", M3 = NA)
  est <- estimate_dental_age(x, verbose = FALSE)
  hdi <- estimate_age_hdi(est, n = 1000)

  expect_named(
    hdi, c("lower_bound", "upper_bound", "completion_threshold")
  )
  expect_equal(
    as.numeric(hdi["completion_threshold"]), ac_info(est)$threshold
  )
})

test_that("get_means_for_scores result works with estimate_dental_age", {
  scores <- get_means_for_scores(ExampleScores[1, ], verbose = FALSE)
  est <- estimate_dental_age(scores, verbose = FALSE)

  expect_false(is.na(est["dental_age"]))
  expect_equal(ac_info(est)$compatibility, "no_terminal_information")
})

test_that("get_means_for_scores returns a dental_scores object", {
  scores <- get_means_for_scores(ExampleScores[1, ], verbose = FALSE)

  expect_s3_class(scores, "dental_scores")
  expect_s3_class(scores$means, "data.frame")
  expect_equal(dim(scores$means), c(6L, 2L))
  expect_named(scores$means, c("log_mu", "log_sd"))
  expect_equal(rownames(scores$means),
               c("Canine", "P3", "P4", "M1", "M2", "M3"))
})

test_that("prepare_scores returns a classed structured object", {
  prepared <- prepare_scores(ExampleScores[1, ], verbose = FALSE)

  expect_s3_class(prepared, "dental_scores")
  expect_named(
    prepared,
    c("means", "sex", "estimable_teeth", "terminal_teeth",
      "missing_teeth", "unparameterized_teeth")
  )
  expect_equal(prepared$sex, "M")

  # Metadata is carried in the object, not as attributes on `means`, so
  # reshaping the means cannot strip it.
  expect_null(attr(prepared$means, "sex"))
})

test_that("a prepared object can be passed straight to the estimator", {
  prepared <- prepare_scores(ExampleScores[1, ], verbose = FALSE)
  from_prepared <- estimate_dental_age(prepared, verbose = FALSE)
  from_row <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)

  expect_equal(as.numeric(from_prepared), as.numeric(from_row))
})
