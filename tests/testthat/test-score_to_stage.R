test_that("numeric scores map to the expected stages", {
  expect_equal(score_to_stage(1), "C.i")
  expect_equal(score_to_stage(8), "Cl.i")
  expect_equal(score_to_stage(13), "A.5")
})

test_that("score 14 becomes Ac, not NA", {
  # Ac is a terminal stage, not a missing observation. Collapsing it to NA
  # would make a completed tooth indistinguishable from an unscored one.
  expect_equal(score_to_stage(14), "Ac")
})

test_that("crypt and missing scores become NA", {
  expect_equal(score_to_stage(0), NA_character_)
  expect_equal(score_to_stage(NA), NA_character_)
})

test_that("the full 0:14 range converts without gaps", {
  stages <- score_to_stage(0:14)

  expect_length(stages, 15L)
  expect_equal(sum(is.na(stages)), 1L)          # score 0 only
  expect_equal(length(unique(stages[-1])), 14L) # 14 distinct stages

  # Every converted stage must exist in the reference vocabulary.
  expect_true(all(stages[!is.na(stages)] %in% AttainmentTables$Stage))
})
