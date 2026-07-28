test_that("recoding works", {
  expect_equal(recode_score("zero"), NA_character_)
  expect_equal(recode_score("Cr 1/2"), "Cr.5")
  expect_equal(recode_score("Ri"), "R.i")
  expect_equal(recode_score("R 3/4"), "R.75")

  # Check pass through for valid score
  expect_equal(recode_score("R.75"), "R.75")
})

test_that("Ac is preserved rather than discarded", {
  # Previously "Ac" was recoded to NA, which destroyed the distinction
  # between a completed tooth and an unscored one.
  expect_equal(recode_score("Ac"), "Ac")
  expect_equal(recode_score("ac"), "Ac")
})

test_that("zero still means missing", {
  expect_equal(recode_score("zero"), NA_character_)
})

test_that("recoded scores are valid stages", {
  raw <- c("Ci", "Cco", "Cco", "Cr 1/2", "Cr 3/4", "Crc", "Ri", "Cli",
           "R 1/4", "R 1/2", "R 3/4", "Rc", "A 1/2", "Ac")
  recoded <- recode_score(raw)

  expect_false(anyNA(recoded))
  expect_true(all(validate_score(recoded)))
})
