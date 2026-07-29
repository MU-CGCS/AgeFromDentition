#' Estimate HDI from dental age estimate
#'
#' @param age_est numeric vector output from \code{estimate_dental_age()}.
#' @param n numeric number of samples to produce. Defaults to 1e5 for speed.
#' Increase this number for investigating the tails of the distribution.
#' @param interval numeric width of the HD interval. Defaults to 0.5
#'
#' @return numeric vector with the lower and upper bounds of the interval,
#'   and the completion threshold from `age_est` if it had one
#'
#' @details
#' This is a genuine highest-density interval, obtained by sampling. It is
#' therefore **stochastic and unseeded**: two calls on the same input give
#' slightly different bounds. Call [set.seed()] first for reproducible
#' output, and raise `n` when the tails matter.
#'
#' Because of that, it is never used to decide whether an estimate is
#' compatible with a completion threshold. [estimate_dental_age()] computes
#' its own equal-tailed central interval analytically for that purpose, so
#' the compatibility code cannot vary between runs.
#'
#' `completion_threshold` is carried through unchanged so that a caller
#' plotting the interval can draw the threshold on the same axis.
#'
#' @seealso [estimate_dental_age()], [age_samples()]
#'
#' @export
#'
#' @examples
#' age_est <- estimate_dental_age(ExampleScores[1, ], verbose = FALSE)
#' estimate_age_hdi(age_est)
#'
estimate_age_hdi <- function(age_est, n = 1e5, interval = 0.5) {
  if (is.na(age_est["log_age"])) {
    hdi_lo <- NA
    hdi_hi <- NA
  } else {
    # Draw from the fitted log-normal and compute the HDI from samples.
    samp <- age_samples(age_est, n)
    hdi_int <- HDInterval::hdi(samp, credMass = interval)
    hdi_lo <- as.numeric(hdi_int[1])
    hdi_hi <- as.numeric(hdi_int[2])
  }

  # Carry the completion threshold through so a caller can plot
  # the interval and threshold on the same axis.
  info <- attr(age_est, "ac_info")
  threshold <- if (is.null(info)) NA_real_ else info$threshold

  return(c("lower_bound" = hdi_lo,
           "upper_bound" = hdi_hi,
           "completion_threshold" = threshold))
}
