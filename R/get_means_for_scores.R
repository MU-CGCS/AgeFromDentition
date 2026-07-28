#' Lookup mean and sd for a sex / tooth / stage combination
#'
#' This function expects a row with columns for "Canine", "P3", "P4", "M1",
#' "M2", and "M3" that contain scores (or NA). Scores are recoded and
#' validated, then reference parameters are looked up.
#'
#' @param x data frame with one row
#' @param verbose boolean flag for printing diagnostic messages
#'
#' @return An object of class `dental_scores`: a list with the 6 x 2
#'   `means` data frame, the `sex`, and the tooth names in each of four
#'   categories (estimable, terminal, missing, unparameterized). Can be
#'   passed directly to [estimate_dental_age()].
#'
#' @seealso [estimate_dental_age()]
#'
#' @export
#'
#' @examples
#' get_means_for_scores(x = ExampleScores[1, ])
#'
#' # Example from Seselj et al. (2019)
#' x <- data.frame(Sex = "M", Canine = "R.25", P3 = "R.25",
#'                 P4 = "R.i", M1 = "A.5",
#'                 M2 = "R.25", M3 = NA)
#' estimate_dental_age(x)
#'
get_means_for_scores <- function(x, verbose = TRUE) {
  return(prepare_scores(x, verbose = verbose))
}
