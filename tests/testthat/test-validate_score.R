test_that("Scores are checked properly", {
  expect_true(validate_score(NA))
  expect_true(validate_score("C.i"))
  expect_true(validate_score("Cr.5"))

  expect_false(validate_score("Cr 1/2"))
  expect_false(validate_score("Rc"))
})

test_that("Ac is a valid stage", {
  # AgeTables has no Ac row, so validating against it alone would reject a
  # legitimately completed tooth.
  expect_true(validate_score("Ac"))
})

test_that("validation is vectorized", {
  result <- validate_score(c("C.i", "nonsense", NA, "Ac"))

  expect_equal(result, c(TRUE, FALSE, TRUE, TRUE))
  expect_length(validate_score(character(0)), 0L)
})

test_that("tooth-aware validation rejects Cl.i on single-rooted teeth", {
  # Cl.i is root cleft initiation and exists only for the molars. Without
  # the tooth argument it passes the global vocabulary check, matches no
  # reference row, and the tooth is dropped from the estimate in silence.
  expect_true(validate_score("Cl.i"))

  expect_false(validate_score("Cl.i", tooth = "Canine"))
  expect_false(validate_score("Cl.i", tooth = "P3"))
  expect_false(validate_score("Cl.i", tooth = "P4"))

  expect_true(validate_score("Cl.i", tooth = "M1"))
  expect_true(validate_score("Cl.i", tooth = "M2"))
  expect_true(validate_score("Cl.i", tooth = "M3"))
})

test_that("tooth-aware validation accepts ordinary stages everywhere", {
  teeth <- c("Canine", "P3", "P4", "M1", "M2", "M3")
  for (tooth in teeth) {
    expect_true(validate_score("C.i", tooth = tooth))
    expect_true(validate_score("A.5", tooth = tooth))
    expect_true(validate_score("Ac", tooth = tooth))
    expect_true(validate_score(NA, tooth = tooth))
  }
})

test_that("tooth recycles over a vector of scores", {
  expect_equal(
    validate_score(c("C.i", "Cl.i", "Ac"), tooth = "Canine"),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    validate_score(c("Cl.i", "Cl.i"), tooth = c("M1", "P3")),
    c(TRUE, FALSE)
  )
})

test_that("malformed tooth arguments are rejected", {
  expect_error(
    validate_score(c("C.i", "C.co", "C.oc"), tooth = c("M1", "M2")),
    "length 1 or the same length"
  )
  expect_error(
    validate_score("C.i", tooth = "Molar1"),
    "Unknown tooth name"
  )
})
