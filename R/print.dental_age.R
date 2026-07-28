# Rendering of dental age results.
#
# Three rules bind every branch below, and the tests in
# test-reporting-language.R enforce them:
#
#   1. The completion threshold is never described as a confidence,
#      credible, or probability bound on the individual's age. It is a
#      percentile of a reference distribution and nothing more.
#   2. No reason is ever given for a tooth being unscored. The input format
#      records only presence or absence, so "not assessable", "absent",
#      "agenesis", and "extracted" are inferences the data cannot support.
#   3. The phrase "minimum age" is never used. The threshold is not a
#      minimum.


# Format a tooth vector for prose, in anatomical order.
format_teeth <- function(teeth) {
  return(paste(teeth, collapse = ", "))
}


# The sentence describing the threshold, plus its mandatory disclaimer.
threshold_lines <- function(info) {
  distribution <- if (info$method == "predictive") {
    "predictive distribution"
  } else {
    "reference distribution"
  }

  percentile <- format(info$q * 100, trim = TRUE)

  lines <- c(
    glue::glue(
      "Reference completion threshold: ",
      "{format(round(info$threshold, 2), nsmall = 2)} years - the ",
      "{percentile}th percentile of the sex-specific {distribution} for ",
      "attaining Ac in {info$binding_tooth} ",
      "(q = {info$q}, method = \"{info$method}\")."
    ),
    paste(
      "This is a descriptive completion threshold for the reference",
      "sample, not a lower confidence limit for this individual."
    )
  )

  caveats <- character(0)
  if (info$low_precision) {
    binding <- info$per_tooth[info$per_tooth$Tooth == info$binding_tooth, ]
    caveats <- c(caveats, glue::glue(
      "it rests on {binding$n} ",
      "{ifelse(binding$n == 1, 'individual', 'individuals')} ",
      "(se = {binding$se_log_mu})"
    ))
  }
  if (info$tied_transition) {
    caveats <- c(caveats, glue::glue(
      "the {info$binding_tooth} Ac transition is tied with A.5 in the ",
      "fitted model, so Ac provides no distinction from A.5 for this sex"
    ))
  }
  if (length(caveats) > 0L) {
    lines <- c(lines, glue::glue(
      "Threshold unstable: {paste(caveats, collapse = '; ')}."
    ))
  }

  return(lines)
}


compatibility_line <- function(info) {
  text <- switch(
    info$compatibility,
    compatible = paste(
      "Compatibility: compatible - the interval lies at or above the",
      "completion threshold."
    ),
    overlap = paste(
      "Compatibility: overlap - the completion threshold falls inside",
      "the interval."
    ),
    discordant = paste(
      "Compatibility: discordant - the whole interval lies below the",
      "completion threshold. Review scoring, sex assignment, and tooth",
      "identity; this may also reflect reference-sample variation or",
      "limitations of the separate-estimator stopgap."
    ),
    NULL
  )
  return(text)
}


render_dental_age <- function(dental_age, ci_lower, ci_upper, info) {
  lines <- character(0)

  if (is.na(dental_age)) {
    if (info$compatibility == "completion_threshold_only") {
      lines <- c(
        lines,
        "Dental age: no finite point estimate.",
        glue::glue(
          "All scored teeth are at terminal stage Ac ",
          "({format_teeth(info$terminal_teeth)})."
        ),
        threshold_lines(info)
      )
    } else {
      lines <- c(
        lines,
        "Dental age: no estimate.",
        "No tooth supplied usable reference parameters."
      )
    }
  } else {
    lines <- c(
      lines,
      glue::glue(
        "Dental age: {format(round(dental_age, 2), nsmall = 2)} years ",
        "(mode of the fitted distribution)."
      ),
      glue::glue(
        "Central {info$ci_level * 100}% interval: ",
        "{format(round(ci_lower, 2), nsmall = 2)} to ",
        "{format(round(ci_upper, 2), nsmall = 2)} years."
      ),
      glue::glue(
        "Estimated from {info$n_estimable} ",
        "{ifelse(info$n_estimable == 1, 'tooth', 'teeth')}."
      )
    )

    if (length(info$terminal_teeth) > 0L) {
      lines <- c(
        lines,
        glue::glue(
          "Terminal stage Ac: {format_teeth(info$terminal_teeth)}."
        ),
        threshold_lines(info),
        compatibility_line(info)
      )
    }
  }

  # Unscored teeth are named, never explained.
  if (length(info$missing_teeth) > 0L) {
    lines <- c(lines, glue::glue(
      "Not scored: {format_teeth(info$missing_teeth)}."
    ))
  }

  if (length(info$unparameterized_teeth) > 0L) {
    lines <- c(lines, glue::glue(
      "Excluded, no published parameters: ",
      "{format_teeth(info$unparameterized_teeth)}."
    ))
  }

  return(paste(as.character(lines), collapse = "\n"))
}


#' Print a dental age estimate
#'
#' @param x a `dental_age` object from [estimate_dental_age()].
#' @param ... ignored.
#'
#' @return `x`, invisibly.
#'
#' @export
#'
#' @examples
#' print(estimate_dental_age(ExampleScores[1, ], verbose = FALSE))
#'
print.dental_age <- function(x, ...) {
  info <- attr(x, "ac_info")

  if (is.null(info)) {
    print(unclass(x))
    return(invisible(x))
  }

  cat(info$message, "\n", sep = "")

  return(invisible(x))
}


#' Terminal-stage information from a dental age estimate
#'
#' Accessor for the completion-threshold detail attached to the result of
#' [estimate_dental_age()].
#'
#' @param x a `dental_age` object.
#'
#' @return A list with the threshold, the binding tooth, the convention used
#'   (`q` and `method`), the full per-tooth table, the `low_precision` and
#'   `tied_transition` flags, the tooth names in each category, the interval
#'   level and type, the compatibility code, and the rendered report.
#'
#' @details
#' The information travels as an attribute, so it does not survive
#' arithmetic, `c()`, or row-wise `sapply()` over several individuals. Those
#' operations keep the numbers and drop the metadata. Call `ac_info()` on
#' each result before combining.
#'
#' @seealso [estimate_dental_age()], [ac_completion_threshold()]
#'
#' @export
#'
#' @examples
#' x <- data.frame(Sex = "F", Canine = "Ac", P3 = "Ac", P4 = "Ac",
#'                 M1 = "Ac", M2 = "Ac", M3 = NA)
#' est <- estimate_dental_age(x, verbose = FALSE)
#' ac_info(est)$threshold
#' ac_info(est)$binding_tooth
#'
ac_info <- function(x) {
  if (!inherits(x, "dental_age")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls dental_age} object from {.fun estimate_dental_age}."
    )
  }
  return(attr(x, "ac_info"))
}
