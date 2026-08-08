#' Estimate dental age
#'
#' Estimate dental age from a set of scored teeth, following the method
#' described in Šešelj M, Sherwood RJ, Konigsberg LW. 2019. Timing of
#' Development of the Permanent Mandibular Dentition: New Reference Values
#' from the Fels Longitudinal Study. *Anat Rec* 302:1733-1753.
#'
#' Teeth at the terminal stage `"A.c"` are excluded from the estimate and
#' reported separately as a completion threshold. See Details.
#'
#' @param scores a scored row (a data frame with `Sex` and one column per
#'   tooth), or a 6 x 2 data frame of means and standard deviations as
#'   returned by [get_means_for_scores()].
#' @param q lower-tail probability for the completion threshold. Defaults to
#'   `0.025`.
#' @param method `"predictive"` (default) or `"plugin"`; passed to
#'   [ac_completion_threshold()].
#' @param sex `"F"` or `"M"`. Only needed when `scores` is a bare means data
#'   frame, which carries no sex of its own.
#' @param verbose boolean flag for printing diagnostic messages.
#'
#' @return A named numeric vector of length 5 with class `dental_age`:
#'   `log_age`, `log_total_var`, `dental_age`, `ci_lower`, `ci_upper`. All
#'   five are `NA` when no tooth supplies a usable estimate.
#'   Terminal-stage information is attached as an attribute; retrieve it
#'   with [ac_info()].
#'
#' @details
#' # Terminal stages
#'
#' A tooth at `"A.c"` has completed, which says only that the individual is
#' older than the completion age. It carries no finite age in stage and is
#' therefore excluded from the weighted estimate. Instead, the `A.c` teeth
#' produce a **reference completion threshold** via
#' [ac_completion_threshold()], reported alongside the estimate.
#'
#' When every scored tooth is at `"A.c"`, there is no point estimate at all
#' and the threshold is the only result.
#'
#' # The interval
#'
#' `ci_lower` and `ci_upper` are an equal-tailed **central 95% interval**,
#' computed with [stats::qlnorm()]. It is not a highest-density interval.
#' For an HDI, use [estimate_age_hdi()], which is a separate, Monte Carlo,
#' function.
#'
#' The compatibility code below is defined from this interval only, so that
#' it is deterministic and reproducible.
#'
#' # Compatibility codes
#'
#' \describe{
#'   \item{no_terminal_information}{no tooth at `"Ac"`; an ordinary
#'     estimate}
#'   \item{completion_threshold_only}{teeth at `"Ac"` but none estimable;
#'     no finite age estimate}
#'   \item{compatible}{`ci_lower >= threshold`}
#'   \item{overlap}{the threshold falls inside the interval}
#'   \item{discordant}{`ci_upper < threshold`; raises a warning}
#' }
#'
#' `discordant` does not necessarily indicate a scoring error. The
#' age-given-stage estimator and the attainment threshold come from related
#' but different models, combined approximately, so disagreement can also
#' reflect reference-sample variation.
#'
#' @seealso [ac_completion_threshold()], [ac_info()], [estimate_age_hdi()]
#'
#' @export
#'
#' @examples
#' # An ordinary estimate
#' estimate_dental_age(ExampleScores[1, ])
#'
#' # Every scored tooth complete: a threshold, and no point estimate
#' x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
#'                 M1 = "Ac", M2 = "Ac", M3 = NA)
#' estimate_dental_age(x)
#'
estimate_dental_age <- function(
  scores,
  q = 0.025,
  method = c("predictive", "plugin"),
  sex = NULL,
  verbose = TRUE
) {
  method <- match.arg(method)

  if (inherits(scores, "dental_scores")) {
    prepared <- scores
  } else if (all(c("log_mu", "log_sd") %in% names(scores))) {
    # Degraded path: a bare means data frame carries no sex and no record
    # of which teeth were terminal, so no threshold can be computed unless
    # the caller supplies `sex` -- and even then, terminal teeth are
    # indistinguishable from unscored ones and contribute nothing.
    means <- as.data.frame(scores)
    prepared <- list(
      means = means,
      sex = sex,
      estimable_teeth = rownames(means)[!is.na(means$log_mu)],
      terminal_teeth = character(0),
      missing_teeth = rownames(means)[is.na(means$log_mu)],
      unparameterized_teeth = character(0)
    )
  } else {
    prepared <- prepare_scores(scores, verbose = verbose)
  }

  means <- prepared$means
  if (is.null(prepared$sex)) {
    prepared$sex <- sex
  }

  ##########################################################################
  # Point estimate using bounds
  m <- means$log_mu[!is.na(means$log_mu)]
  s <- means$log_sd[!is.na(means$log_sd)]
  n_estimable <- length(m)

  if (n_estimable == 0L) {
    age <- NA_real_
    vv <- NA_real_
  } else {
    if (n_estimable == 1L && verbose) {
      cli::cli_warn("Estimating from only 1 tooth.")
    }

    # Precision weighting: each tooth's variance (log_sd^2) is inverted
    # to a precision. The weighted mean of the log-scale means is the
    # point estimate on the log scale.
    precision <- 1 / s^2
    total_precision <- sum(precision)
    rel_precision <- precision / total_precision

    # Weighted mean is the log_mu prediction
    age <- sum(m * rel_precision)

    # Total variance
    var_tot <- 1 / total_precision

    # Between-tooth variance captures disagreement among teeth.
    # When all teeth agree perfectly, this is zero.
    var_between_tooth <- dplyr::if_else(is.na(var(m)), 0, var(m))

    # Total variance
    vv <- var_tot + var_between_tooth
  }

  # Back-transform to the age scale. exp(mu) / exp(var) is the
  # mode of the fitted log-normal.
  dental_age <- exp(age) / exp(vv)

  # Central interval, analytic and deterministic
  ci_level <- 0.95
  if (is.na(age)) {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  } else {
    tail <- (1 - ci_level) / 2
    ci_lower <- stats::qlnorm(tail, age, sqrt(vv))
    ci_upper <- stats::qlnorm(1 - tail, age, sqrt(vv))
  }

  # Completion threshold from the terminal teeth
  if (is.null(prepared$sex)) {
    ac <- empty_threshold(q, method)
  } else {
    ac <- ac_completion_threshold(
      prepared$sex,
      prepared$terminal_teeth,
      q,
      method
    )
  }

  # Ac-derived estimate: when all scored teeth are terminal and M2 or M3
  # is among the terminal teeth, use the latest-completing one's Ac
  # attainment distribution as the point estimate. M3 > M2 by log_mu;
  # the selection is by tooth identity, not by threshold rank.
  ac_derived <- FALSE
  ac_derived_tooth <- if ("M3" %in% prepared$terminal_teeth) {
    "M3"
  } else if ("M2" %in% prepared$terminal_teeth) {
    "M2"
  } else {
    NA_character_
  }

  if (
    n_estimable == 0L &&
      !is.null(prepared$sex) &&
      !is.na(ac_derived_tooth)
  ) {
    params <- AttainmentTables[
      AttainmentTables$Sex == prepared$sex &
        AttainmentTables$Tooth == ac_derived_tooth &
        AttainmentTables$Stage == "Ac",
    ]
    if (nrow(params) == 1L) {
      sd_eff_ac <- if (method == "predictive") {
        sqrt(params$log_sd^2 + params$se_log_mu^2)
      } else {
        params$log_sd
      }
      age <- params$log_mu
      vv <- sd_eff_ac^2
      dental_age <- exp(age) / exp(vv)
      ac_tail <- (1 - ci_level) / 2
      ci_lower <- stats::qlnorm(ac_tail, age, sqrt(vv))
      ci_upper <- stats::qlnorm(1 - ac_tail, age, sqrt(vv))
      ac_derived <- TRUE
    }
  }

  if (n_estimable == 0L && !ac_derived && verbose) {
    cli::cli_warn("No age estimate.")
  }

  # Classify the relationship between the analytic central interval
  # and the completion threshold. Uses the central interval (not
  # the HDI).
  compatibility <- classify_compatibility(
    ci_lower,
    ci_upper,
    ac$threshold,
    n_terminal = length(prepared$terminal_teeth),
    n_estimable = n_estimable,
    ac_derived = ac_derived
  )

  if (compatibility == "discordant") {
    cli::cli_warn(c(
      "Nonterminal-stage estimate lies below the Ac completion threshold.",
      "i" = paste(
        "Review scoring, sex assignment, and tooth identity;",
        "this may also reflect reference-sample variation or",
        "limitations of the separate-estimator stopgap."
      )
    ))
  }

  info <- c(
    ac,
    list(
      terminal_teeth = prepared$terminal_teeth,
      missing_teeth = prepared$missing_teeth,
      unparameterized_teeth = prepared$unparameterized_teeth,
      n_estimable = n_estimable,
      sex = prepared$sex,
      ci_level = ci_level,
      ci_type = "central",
      compatibility = compatibility,
      ac_derived = ac_derived,
      ac_derived_tooth = ac_derived_tooth
    )
  )

  info$message <- render_dental_age(
    dental_age,
    ci_lower,
    ci_upper,
    info
  )

  out <- c(
    log_age = age,
    log_total_var = vv,
    dental_age = dental_age,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  attr(out, "ac_info") <- info
  class(out) <- c("dental_age", "numeric")

  return(out)
}


# Threshold placeholder for the case where sex is unknown, so that the
# result has the same shape whether or not a threshold could be computed.
empty_threshold <- function(q, method) {
  return(list(
    threshold = NA_real_,
    binding_tooth = NA_character_,
    q = q,
    method = method,
    per_tooth = AttainmentTables[0, ],
    low_precision = FALSE,
    tied_transition = FALSE
  ))
}


# Compatibility is judged from the central interval alone. The point
# estimate is deliberately not used: `dental_age` is the mode of the fitted
# log-normal, not a central summary, so it is the wrong quantity to compare
# against a threshold.
#
# Boundaries are inclusive at the lower limit and exclusive at the upper.
classify_compatibility <- function(
  ci_lower,
  ci_upper,
  threshold,
  n_terminal,
  n_estimable,
  ac_derived = FALSE
) {
  # Order matters: discordant before compatible, because the
  # boundary cases (ci_upper == threshold) fall to overlap.
  if (n_terminal == 0L) {
    return("no_terminal_information")
  }
  if (n_estimable == 0L && !ac_derived) {
    return("completion_threshold_only")
  }
  if (is.na(threshold) || is.na(ci_lower) || is.na(ci_upper)) {
    return("no_terminal_information")
  }
  if (ci_upper < threshold) {
    return("discordant")
  }
  if (ci_lower >= threshold) {
    return("compatible")
  }
  return("overlap")
}
