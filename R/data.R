#' Example dental scores
#'
#' A dataset containing five example dental scores for use in calculating
#' dental age.
#'
#' @format A data frame with 5 rows and 8 variables:
#' \describe{
#'   \item{ID}{individual ID}
#'   \item{Sex}{character of sex: "F" or "M"}
#'   \item{Canine}{character score for canine}
#'   \item{P3}{character score for P3}
#'   \item{P4}{character score for P4}
#'   \item{M1}{character score for M1}
#'   \item{M2}{character score for M2}
#'   \item{M3}{character score for M3}
#' }
#'
"ExampleScores"

#' Age given stage: log-normal parameters for dental stages
#'
#' Sex-specific age-given-stage reference values from Tables 8-13 of Šešelj
#' et al. (2019). Each row gives the log-normal approximation to the
#' distribution of age *conditional on observing a tooth in a stage*.
#'
#' @format A data frame with 150 rows and 12 variables:
#' \describe{
#'   \item{Sex}{character of sex: "F" or "M"}
#'   \item{Tooth}{character ID of tooth: "Canine", "P3", "P4", "M1", "M2",
#'     or "M3"}
#'   \item{Stage}{character for tooth stage}
#'   \item{log_mu}{numeric mean log age given the stage, on the log scale}
#'   \item{log_sd}{numeric log-scale standard deviation}
#'   \item{mode}{numeric mode of the fitted distribution, in years}
#'   \item{median}{numeric median of the fitted distribution, in years}
#'   \item{mean}{numeric mean of the fitted distribution, in years}
#'   \item{sd}{numeric standard deviation of the fitted distribution, in
#'     years}
#'   \item{hpd_low}{numeric lower bound of the published 95% highest
#'     posterior density interval, in years}
#'   \item{opt}{numeric published optimal age estimate for the stage, in
#'     years}
#'   \item{hpd_high}{numeric upper bound of the published 95% highest
#'     posterior density interval, in years}
#' }
#'
#' @details
#' Mind the mixed scales. `log_mu` and `log_sd` are on the log scale;
#' every other numeric column is in years. The summary columns are retained
#' under the names Šešelj et al. print, so a row can be checked against
#' Tables 8a-13a directly.
#'
#' The summary columns are redundant with `log_mu` and `log_sd` for most
#' rows -- `median = exp(log_mu)`, `mode = exp(log_mu - log_sd^2)`, and so
#' on, and the build script verifies exactly that. They are shipped because
#' of the rows where they are *not* redundant; see below.
#'
#' Note the contrast with [AttainmentTables]: here `log_mu` is the mean log
#' age *given* that the tooth is observed in the stage, whereas in
#' `AttainmentTables` it is the mean log age at *entering* the stage. The
#' two are different quantities and are not interchangeable.
#'
#' There is deliberately no `Ac` row. Apex closure is a terminal, absorbing
#' stage: once a tooth reaches it, the observation says only that the
#' individual is older than the completion age, so there is no finite mean
#' age in stage to report. Use [AttainmentTables] for `Ac`.
#'
#' # Rows without log-normal parameters
#'
#' Four rows have `NA` for `log_mu` and `log_sd`, all of them female M3, and
#' they are not the same kind of gap:
#' \describe{
#'   \item{`R.5` and `A.5`}{zero-width stages. The fitted transition ages
#'     are tied with the following stage (see [StageTies]), so the stage has
#'     no width and nothing can be estimated. Every column is `NA`.}
#'   \item{`R.75` and `R.c`}{Šešelj et al. publish no log-normal fit, but
#'     they do publish an interval: `hpd_low`, `opt`, and `hpd_high` are
#'     populated. Those intervals are wide -- 13.73 to 21.09 years and 14.28
#'     to 21.92 years respectively -- which is itself informative about late
#'     M3 development.}
#' }
#'
#' A tooth scored into any of these four rows still contributes nothing to
#' [estimate_dental_age()], which works from `log_mu` and `log_sd`. The two
#' HPD-only rows are shipped so that the published information is at least
#' reachable rather than silently absent.
#'
#' @source Šešelj M, Sherwood RJ, Konigsberg LW. 2019. Timing of Development
#' of the Permanent Mandibular Dentition: New Reference Values from the Fels
#' Longitudinal Study. Anat Rec 302:1733-1753. Tables 8-13, panel (a).
#'
#' @seealso [AttainmentTables], [StageTies]
#'
"AgeTables"

#' Age of attainment: log-normal transition parameters
#'
#' Sex-specific ages of attainment from Tables 2-7 of Šešelj et al. (2019),
#' estimated by transition analysis. Each row describes the distribution of
#' the age at which a tooth *enters* a stage.
#'
#' @format A data frame with 162 rows and 7 variables:
#' \describe{
#'   \item{Sex}{character of sex: "F" or "M"}
#'   \item{Tooth}{character ID of tooth: "Canine", "P3", "P4", "M1", "M2",
#'     or "M3"}
#'   \item{Stage}{character for tooth stage}
#'   \item{log_mu}{numeric mean log age at entering the stage}
#'   \item{log_sd}{numeric log-scale standard deviation, common to all
#'     stages within a sex and tooth}
#'   \item{se_log_mu}{numeric standard error of `log_mu`}
#'   \item{n}{integer number of individuals contributing to the transition}
#' }
#'
#' @details
#' This table is a strict superset of [AgeTables]: the same sex, tooth, and
#' stage combinations, plus twelve `Ac` (apex closure) rows that
#' `AgeTables` cannot represent. It is therefore the source for anything
#' involving the terminal stage.
#'
#' `log_mu` here is **not** comparable to `log_mu` in [AgeTables]. This is
#' the age at *entering* a stage; that is the age *given* the stage.
#'
#' `se_log_mu` varies by more than an order of magnitude across rows and
#' should not be ignored. The two M3 `Ac` transitions rest on n = 1 (female)
#' and n = 5 (male) individuals and have `se_log_mu` of 0.0638 and 0.0482,
#' against 0.011-0.029 for the well-estimated transitions.
#'
#' `Stage` uses the full 14-value vocabulary, but not every stage exists for
#' every tooth: the single-rooted teeth (Canine, P3, P4) have no `Cl.i`
#' (root cleft initiation) row. Check tooth and stage together, not stage
#' alone.
#'
#' @source Šešelj M, Sherwood RJ, Konigsberg LW. 2019. Timing of Development
#' of the Permanent Mandibular Dentition: New Reference Values from the Fels
#' Longitudinal Study. Anat Rec 302:1733-1753. Tables 2-7, panel (a).
#'
#' @seealso [AgeTables], [StageTies]
#'
"AttainmentTables"

#' Tied stage transitions
#'
#' Adjacent stages whose fitted ages of attainment are identical in
#' [AttainmentTables], meaning the intervening stage has zero width under
#' the reference model.
#'
#' @format A data frame with 2 rows and 6 variables:
#' \describe{
#'   \item{Sex}{character of sex: "F" or "M"}
#'   \item{Tooth}{character ID of tooth}
#'   \item{Stage_lo}{character, the earlier stage of the tied pair}
#'   \item{Stage_hi}{character, the later stage of the tied pair}
#'   \item{log_mu}{numeric mean log age shared by both transitions}
#'   \item{tie_class}{character: "terminal" if `Stage_hi` is `Ac`,
#'     otherwise "interior"}
#' }
#'
#' @details
#' Both ties are female M3, and the `terminal` one has a practical
#' consequence: because the fitted age of attaining M3 `Ac` equals the age
#' of attaining M3 `A.5`, observing apex closure in a female M3 supplies no
#' information beyond `A.5` under this reference model. Any completion
#' threshold that a female M3 `Ac` produces should be reported as
#' model-tied.
#'
#' The `interior` tie explains why female M3 `R.5` has no parameters in
#' [AgeTables].
#'
#' @source Derived from Tables 2-7 of Šešelj M, Sherwood RJ, Konigsberg LW.
#' 2019. Timing of Development of the Permanent Mandibular Dentition: New
#' Reference Values from the Fels Longitudinal Study. Anat Rec
#' 302:1733-1753.
#'
#' @seealso [AttainmentTables], [AgeTables]
#'
"StageTies"
