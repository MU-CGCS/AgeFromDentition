#' Generate sample for age estimate
#'
#' @param age_est Result of call to estimate_dental_age()
#' @param n Number of samples
#'
#' @return n samples, or `n` `NA`s when the estimate has no finite value
#'
#' @details
#' This function is **stochastic**: it draws from the fitted log-normal and
#' is not seeded, so results vary between runs. Call [set.seed()] first if
#' you need reproducible output.
#'
#' When `age_est` has no point estimate -- every scored tooth at terminal
#' stage `"Ac"`, for instance -- there is no distribution to draw from and
#' `n` `NA`s are returned.
#'
#' @seealso [estimate_dental_age()], [estimate_age_hdi()]
#'
#' @export
#'
age_samples <- function(age_est, n) {
  log_age <- as.numeric(age_est["log_age"])
  log_total_var <- as.numeric(age_est["log_total_var"])

  if (is.na(log_age) || is.na(log_total_var)) {
    return(rep(NA_real_, n))
  }

  samp <- rlnorm(n, log_age, sqrt(log_total_var))
  return(samp)
}
