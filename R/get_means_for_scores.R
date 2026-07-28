#' Lookup mean and sd for a sex / tooth / stage combination
#'
#' This function expects a row with columns for "Canine", "P3", "P4", "M1",
#' "M2", and "M3" that contain scores (or NA). A matrix of means and standard
#' deviations for each tooth is returned.
#'
#' @param x data frame with one row
#' @param verbose boolean flag for printing diagnostic messages
#'
#' @return 6 x 2 matrix with means and standard deviations for each tooth.
#' @export
#'
#' @examples
#' get_means_for_scores(x = ExampleScores[1, ])
#'
#' # Example from Seselj et al. (2019)
#' x <- data.frame(Sex = "M", Canine = "R.25", P3 = "R.25",
#'                 P4 = "R.i", M1 = "A.5",
#'                 M2 = "R.25", M3 = NA)
#' means <- get_means_for_scores(x)
#' estimate_dental_age(means)
#'
get_means_for_scores <- function(x, verbose = TRUE) {
  # x is 1 row of values

  x <- as.data.frame(x) # Need data.frame for proper subsetting below

  # Check Sex
  if (!(x$Sex %in% c("F", "M"))) {
    stop("Sex should be a string 'F' or 'M'.")
  }

  Teeth <- c("Canine", "P3", "P4", "M1", "M2", "M3")

  # Check that all teeth columns are present
  if (!all(Teeth %in% names(x))) stop("Teeth name mismatch in column names.")

  means <- matrix(NA, ncol = 2, nrow = 6)

  for (jj in 1:length(Teeth)) {
    tooth <- Teeth[jj]

    # Get observed stage for tooth
    stage <- x[1, tooth]

    # Check that stage is valid
    if (!validate_score(stage)) stop("Invalid stage name.")

    means[jj, ] <- AgeTables |>
      dplyr::filter(Sex == x[1, "Sex"],
                    Tooth == tooth,
                    Stage == stage) |>
      dplyr::select(log_mu, log_sd) |>
      as.numeric()
  }

  if (x$M1 == "C.i" | is.na(x$M1)) {
    if (verbose) {
      message("M1 is stage C.i or missing, dropping from age estimation.")
    }
    means[4, ] <- c(NA, NA)
  }

  colnames(means) <- c("log_mu", "log_sd")
  rownames(means) <- Teeth
  return(as.data.frame(means))
}

