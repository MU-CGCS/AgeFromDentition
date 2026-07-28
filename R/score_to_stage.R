#' Convert stage numeric score to character score
#'
#' @param x vector of numeric stage scores
#'
#' @return vector with numeric scores converted to character
#'
#' @details
#' Score 14 is apex closure and converts to `"Ac"`, a terminal stage. It is
#' deliberately **not** converted to `NA`: `NA` means the tooth was not
#' scored, whereas `"Ac"` means it was scored and is complete. The two are
#' different observations and must stay distinguishable.
#'
#' Score 0 (crypt) does convert to `NA`. It lies below the first modelled
#' stage and has no reference parameters.
#'
#' @export
#'
#' @examples
#' data.frame(numeric_stage = 0:14, stage = score_to_stage(0:14))
#'
score_to_stage <- function(x) {
  dplyr::case_when(
    x == 0 ~ NA_character_,
    x == 1 ~ "C.i",
    x == 2 ~ "C.co",
    x == 3 ~ "C.oc",
    x == 4 ~ "Cr.5",
    x == 5 ~ "Cr.75",
    x == 6 ~ "Cr.c",
    x == 7 ~ "R.i",
    x == 8 ~ "Cl.i",
    x == 9 ~ "R.25",
    x == 10 ~ "R.5",
    x == 11 ~ "R.75",
    x == 12 ~ "R.c",
    x == 13 ~ "A.5",
    x == 14 ~ "Ac",
    is.na(x) ~ NA_character_
  )
}
