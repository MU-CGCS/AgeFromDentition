#' Classify and look up one row of dental scores
#'
#' Internal workhorse behind [get_means_for_scores()] and
#' [estimate_dental_age()]. It looks up reference parameters and, just as
#' importantly, records *why* each tooth did or did not contribute.
#'
#' @param x data frame with one row, containing `Sex` and one column per
#'   tooth.
#' @param verbose boolean flag for printing diagnostic messages.
#'
#' @return An object of class `dental_scores`: a list with the 6 x 2 `means`
#'   data frame, the `sex`, and the tooth names in each of four categories.
#'
#' @details
#' Every tooth falls into exactly one category:
#' \describe{
#'   \item{estimable}{scored, and the reference table supplies parameters.
#'     These are the teeth the age estimate is built from.}
#'   \item{terminal}{scored `"Ac"`. Apex closure is absorbing, so there is
#'     no finite age in stage to invert; these teeth go to
#'     [ac_completion_threshold()] instead.}
#'   \item{missing}{not scored. Also covers M1 at `"C.i"`, which the
#'     estimator has always dropped because it is left-censored.}
#'   \item{unparameterized}{scored, but the reference table has no usable
#'     parameters. In practice these are the four female M3 rows described
#'     in [AgeTables].}
#' }
#'
#' The categories exist because the previous implementation collapsed all
#' four to `NA` and dropped them without comment. A completed tooth, an
#' unscored tooth, and a tooth whose reference row is empty are different
#' observations and are reported differently.
#'
#' @seealso [get_means_for_scores()], [estimate_dental_age()]
#'
#' @noRd
prepare_scores <- function(x, verbose = TRUE) {
  x <- as.data.frame(x) # Need data.frame for proper subsetting below

  sex <- x[1, "Sex"]
  if (!(sex %in% c("F", "M"))) {
    cli::cli_abort("{.arg Sex} must be {.val F} or {.val M}.")
  }

  Teeth <- c("Canine", "P3", "P4", "M1", "M2", "M3")

  # Check that all teeth columns are present
  if (!all(Teeth %in% names(x))) {
    cli::cli_abort("Teeth name mismatch in column names.")
  }

  means <- matrix(NA_real_, ncol = 2, nrow = 6)
  status <- character(6)

  # Classify each tooth into one of four categories:
  # estimable, terminal (Ac), missing (NA), or unparameterized
  # (scored but no reference parameters).
  for (jj in seq_along(Teeth)) {
    tooth <- Teeth[jj]
    stage <- recode_score(x[1, tooth])
    x[1, tooth] <- stage

    # Validation is tooth-aware, so a stage that does not exist for this
    # tooth -- Cl.i on a single-rooted tooth, say -- is an error rather
    # than a row that quietly looks up to nothing.
    if (!validate_score(stage, tooth = tooth)) {
      cli::cli_abort("Invalid stage {.val {stage}} for {.val {tooth}}.")
    }

    if (is.na(stage)) {
      status[jj] <- "missing"
      next
    }

    if (stage == "Ac") {
      status[jj] <- "terminal"
      next
    }

    # Look up the age-given-stage parameters. A missing or NA row
    # means the reference table has no usable fit for this combination.
    row <- AgeTables |>
      dplyr::filter(Sex == sex, Tooth == tooth, Stage == stage)

    if (nrow(row) != 1L || is.na(row$log_mu) || is.na(row$log_sd)) {
      status[jj] <- "unparameterized"
      next
    }

    means[jj, ] <- c(row$log_mu, row$log_sd)
    status[jj] <- "estimable"
  }

  # M1 at C.i is left-censored: the stage is open below and the fitted
  # parameters describe a range the individual has already passed through.
  # The estimator has dropped it since v0.1 and continues to.
  m1_dropped <- isTRUE(x$M1 == "C.i")
  if (m1_dropped) {
    means[4, ] <- c(NA_real_, NA_real_)
    status[4] <- "missing"
  }

  if (verbose) {
    if (m1_dropped || is.na(x$M1)) {
      cli::cli_inform(
        "M1 is stage C.i or missing, dropping from age estimation."
      )
    }

    terminal <- Teeth[status == "terminal"]
    if (length(terminal) > 0L) {
      n_terminal <- length(terminal)
      cli::cli_inform(paste0(
        "{n_terminal} ",
        "{cli::qty(n_terminal)}{?tooth/teeth} at terminal ",
        "stage Ac ({paste(terminal, collapse = ', ')}); excluded from ",
        "the age estimate and used for the completion threshold."
      ))
    }

    for (tooth in Teeth[status == "unparameterized"]) {
      stage <- x[1, tooth]
      sex_label <- ifelse(sex == "F", "females", "males")
      cli::cli_inform(
        "{tooth} stage {stage} has no published log-normal parameters for {sex_label}; excluded."
      )
    }
  }

  colnames(means) <- c("log_mu", "log_sd")
  rownames(means) <- Teeth
  means <- as.data.frame(means)

  out <- list(
    means = means,
    sex = sex,
    estimable_teeth = Teeth[status == "estimable"],
    terminal_teeth = Teeth[status == "terminal"],
    missing_teeth = Teeth[status == "missing"],
    unparameterized_teeth = Teeth[status == "unparameterized"]
  )
  class(out) <- "dental_scores"

  return(out)
}
