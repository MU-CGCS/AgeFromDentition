# Guards on the shipped reference data. These pin values transcribed from
# Šešelj et al. (2019) so that a future rebuild of data-raw/ cannot change
# a published number without a test failing.

test_that("AgeTables has the expected shape", {
  expect_s3_class(AgeTables, "data.frame")
  expect_equal(nrow(AgeTables), 150L)
  expect_named(
    AgeTables,
    c("Sex", "Tooth", "Stage", "log_mu", "log_sd",
      "mode", "median", "mean", "sd", "hpd_low", "opt", "hpd_high")
  )
  expect_setequal(AgeTables$Sex, c("F", "M"))
  expect_setequal(
    AgeTables$Tooth,
    c("Canine", "P3", "P4", "M1", "M2", "M3")
  )
})

test_that("the published summaries agree with the log-normal parameters", {
  # median = exp(log_mu), mode = exp(log_mu - log_sd^2),
  # mean = exp(log_mu + log_sd^2 / 2). Exact identities, so any drift
  # between the two halves of the table is a transcription error.
  fitted <- AgeTables[!is.na(AgeTables$log_mu), ]

  expect_equal(exp(fitted$log_mu), fitted$median, tolerance = 1e-3)
  expect_equal(
    exp(fitted$log_mu - fitted$log_sd^2), fitted$mode,
    tolerance = 1e-3
  )
  expect_equal(
    exp(fitted$log_mu + fitted$log_sd^2 / 2), fitted$mean,
    tolerance = 1e-3
  )
})

test_that("the published HPD interval brackets the optimal age", {
  bounded <- AgeTables[!is.na(AgeTables$hpd_low), ]

  expect_true(all(bounded$hpd_low <= bounded$opt))
  expect_true(all(bounded$opt <= bounded$hpd_high))
})

test_that("AgeTables has no Ac row", {
  # Ac is terminal, so Tables 8-13 give it no finite age-in-stage.
  expect_false("Ac" %in% AgeTables$Stage)
})

test_that("AgeTables' missing parameters are exactly the four female M3 rows", {
  missing_rows <- AgeTables[is.na(AgeTables$log_mu), ]

  expect_equal(nrow(missing_rows), 4L)
  expect_true(all(missing_rows$Sex == "F"))
  expect_true(all(missing_rows$Tooth == "M3"))
  expect_setequal(missing_rows$Stage, c("R.5", "R.75", "R.c", "A.5"))

  # log_mu and log_sd are missing together, never one without the other.
  expect_equal(is.na(AgeTables$log_mu), is.na(AgeTables$log_sd))
})

test_that("two of those four rows still carry a published interval", {
  # The reason for shipping the HPD columns: girls' M3 R.75 and R.c have
  # no log-normal fit but do have a published interval, which was
  # unreachable while the dataset held only log_mu and log_sd.
  hpd_only <- AgeTables[is.na(AgeTables$log_mu) & !is.na(AgeTables$hpd_low), ]

  expect_equal(nrow(hpd_only), 2L)
  expect_setequal(hpd_only$Stage, c("R.75", "R.c"))
  expect_true(all(hpd_only$Sex == "F"))
  expect_true(all(hpd_only$Tooth == "M3"))

  # Wide intervals, and worth being visible: over seven years each.
  expect_true(all(hpd_only$hpd_high - hpd_only$hpd_low > 7))
})

test_that("zero-width stages are missing in every column", {
  # R.5 and A.5 are tied with the following stage, so nothing at all can
  # be estimated for them -- unlike the HPD-only pair above.
  zero_width <- AgeTables[
    AgeTables$Sex == "F" &
      AgeTables$Tooth == "M3" &
      AgeTables$Stage %in% c("R.5", "A.5"),
  ]

  expect_equal(nrow(zero_width), 2L)
  value_cols <- c("log_mu", "log_sd", "mode", "median", "mean", "sd",
                  "hpd_low", "opt", "hpd_high")
  expect_true(all(is.na(zero_width[, value_cols])))
})

test_that("AgeTables rows are in developmental, not alphabetical, order", {
  # Sorting Stage as character would put A.5 first and scramble the
  # sequence the tables are meant to be read in.
  female_canine <- AgeTables[
    AgeTables$Sex == "F" & AgeTables$Tooth == "Canine",
  ]
  expect_equal(female_canine$Stage[1:3], c("C.i", "C.co", "C.oc"))
  expect_true(all(diff(female_canine$log_mu) > 0))
})

test_that("AttainmentTables has the expected shape", {
  expect_s3_class(AttainmentTables, "data.frame")
  expect_equal(nrow(AttainmentTables), 162L)
  expect_named(
    AttainmentTables,
    c("Sex", "Tooth", "Stage", "log_mu", "log_sd", "se_log_mu", "n")
  )
  expect_equal(sum(AttainmentTables$Stage == "Ac"), 12L)
})

test_that("AttainmentTables has no missing parameters", {
  # Unlike AgeTables, every attainment row is estimable. The completion
  # threshold depends on all three parameter columns.
  expect_false(anyNA(AttainmentTables$log_mu))
  expect_false(anyNA(AttainmentTables$log_sd))
  expect_false(anyNA(AttainmentTables$se_log_mu))
  expect_true(all(AttainmentTables$n > 0L))
})

test_that("log_sd is common within each sex and tooth", {
  # Šešelj et al. fit one log-scale SD per tooth and sex, shared across
  # stages. If this ever fails, the ln_SD row was parsed per stage.
  by_tooth <- split(
    AttainmentTables$log_sd,
    list(AttainmentTables$Sex, AttainmentTables$Tooth)
  )
  expect_true(all(vapply(by_tooth, \(x) length(unique(x)) == 1L, logical(1))))
})

test_that("Cl.i exists only for the molars", {
  # The single-rooted teeth have no root cleft. Anything relying on a
  # global stage vocabulary will accept Cl.i for a canine, which is
  # anatomically impossible.
  cleft <- AttainmentTables[AttainmentTables$Stage == "Cl.i", ]
  expect_setequal(cleft$Tooth, c("M1", "M2", "M3"))
  expect_equal(nrow(cleft), 6L)
})

test_that("the twelve Ac transition parameters match the published values", {
  ac <- AttainmentTables[AttainmentTables$Stage == "Ac", ]
  key <- paste(ac$Sex, ac$Tooth)

  expected_log_mu <- c(
    "F Canine" = 2.5597, "F P3" = 2.5909, "F P4" = 2.6827,
    "F M1" = 2.2787, "F M2" = 2.7037, "F M3" = 2.8934,
    "M Canine" = 2.6465, "M P3" = 2.6371, "M P4" = 2.6855,
    "M M1" = 2.3186, "M M2" = 2.7092, "M M3" = 2.9156
  )
  expected_log_sd <- c(
    "F Canine" = 0.1342, "F P3" = 0.1030, "F P4" = 0.1133,
    "F M1" = 0.1186, "F M2" = 0.1020, "F M3" = 0.1085,
    "M Canine" = 0.1268, "M P3" = 0.0992, "M P4" = 0.1118,
    "M M1" = 0.1159, "M M2" = 0.1249, "M M3" = 0.1061
  )
  expected_se <- c(
    "F Canine" = 0.0289, "F P3" = 0.0192, "F P4" = 0.0195,
    "F M1" = 0.0118, "F M2" = 0.0173, "F M3" = 0.0638,
    "M Canine" = 0.0208, "M P3" = 0.0161, "M P4" = 0.0178,
    "M M1" = 0.0114, "M M2" = 0.0181, "M M3" = 0.0482
  )
  expected_n <- c(
    "F Canine" = 23L, "F P3" = 31L, "F P4" = 35L,
    "F M1" = 112L, "F M2" = 37L, "F M3" = 1L,
    "M Canine" = 39L, "M P3" = 40L, "M P4" = 42L,
    "M M1" = 114L, "M M2" = 49L, "M M3" = 5L
  )

  expect_equal(ac$log_mu, unname(expected_log_mu[key]))
  expect_equal(ac$log_sd, unname(expected_log_sd[key]))
  expect_equal(ac$se_log_mu, unname(expected_se[key]))
  expect_equal(ac$n, unname(expected_n[key]))
})

test_that("M3 Ac is the worst-estimated transition in the table", {
  # This is why the completion threshold is computed predictively rather
  # than plug-in: the imprecision is concentrated in exactly the tooth
  # that binds the threshold whenever it is scored.
  ac <- AttainmentTables[AttainmentTables$Stage == "Ac", ]
  m3 <- ac[ac$Tooth == "M3", ]
  others <- ac[ac$Tooth != "M3", ]

  expect_true(all(m3$se_log_mu > max(others$se_log_mu)))
  expect_true(all(m3$n < min(others$n)))
})

test_that("AttainmentTables is a strict superset of AgeTables", {
  key <- function(x) paste(x$Sex, x$Tooth, x$Stage)

  ags <- key(AgeTables)
  att <- key(AttainmentTables)

  # Every scorable stage has a transition to derive a threshold from.
  expect_true(all(ags %in% att))

  extra <- AttainmentTables[!att %in% ags, ]
  expect_equal(nrow(extra), 12L)
  expect_true(all(extra$Stage == "Ac"))
})

test_that("StageTies holds exactly the two female M3 ties", {
  expect_s3_class(StageTies, "data.frame")
  expect_equal(nrow(StageTies), 2L)
  expect_named(
    StageTies,
    c("Sex", "Tooth", "Stage_lo", "Stage_hi", "log_mu", "tie_class")
  )
  expect_true(all(StageTies$Sex == "F"))
  expect_true(all(StageTies$Tooth == "M3"))
  expect_setequal(StageTies$tie_class, c("interior", "terminal"))
})

test_that("the terminal tie means female M3 Ac adds nothing beyond A.5", {
  terminal <- StageTies[StageTies$tie_class == "terminal", ]
  expect_equal(terminal$Stage_lo, "A.5")
  expect_equal(terminal$Stage_hi, "Ac")

  # The tie must be real in the data it is derived from, not just asserted
  # in the registry.
  pair <- AttainmentTables[
    AttainmentTables$Sex == "F" &
      AttainmentTables$Tooth == "M3" &
      AttainmentTables$Stage %in% c("A.5", "Ac"),
  ]
  expect_equal(length(unique(pair$log_mu)), 1L)
  expect_equal(unique(pair$log_mu), terminal$log_mu)
})

test_that("every tie corresponds to a missing AgeTables row", {
  # A zero-width stage cannot have age-given-stage parameters, so each
  # interior tie should show up as an NA row in AgeTables.
  interior <- StageTies[StageTies$tie_class == "interior", ]
  for (i in seq_len(nrow(interior))) {
    row <- AgeTables[
      AgeTables$Sex == interior$Sex[i] &
        AgeTables$Tooth == interior$Tooth[i] &
        AgeTables$Stage == interior$Stage_lo[i],
    ]
    expect_true(is.na(row$log_mu))
  }
})

test_that("ExampleScores is unchanged by the data rebuild", {
  expect_equal(nrow(ExampleScores), 5L)
  expect_named(
    ExampleScores,
    c("ID", "Sex", "Canine", "P3", "P4", "M1", "M2", "M3")
  )
  expect_equal(ExampleScores$Sex, c("M", "F", "M", "F", "F"))
  expect_equal(
    ExampleScores$Canine,
    c("R.25", "C.co", "C.co", "R.25", "R.75")
  )
  expect_equal(ExampleScores$M3, c(NA, "C.co", "C.co", NA, NA))
})
