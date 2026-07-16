test_that("recoding works", {
  expect_equal(recode_score("zero"), NA_character_)
  expect_equal(recode_score("Cr 1/2"), "Cr.5")
  expect_equal(recode_score("Ri"), "R.i")
  expect_equal(recode_score("R 3/4"), "R.75")

  # Check pass through for valid score
  expect_equal(recode_score("R.75"), "R.75")
})
