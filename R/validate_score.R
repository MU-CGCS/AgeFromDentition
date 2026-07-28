#' Validate stage scores
#'
#' Check stage scores against the reference vocabulary, optionally against
#' the stages that exist for a particular tooth.
#'
#' @param x character vector of stage scores to check. `NA` is treated as
#'   valid: it means the tooth was not scored.
#' @param tooth optional character vector of tooth names, either length 1 or
#'   the same length as `x`. When supplied, each score is checked against
#'   the stages that exist for that tooth rather than against the whole
#'   vocabulary.
#'
#' @return logical vector the same length as `x`
#'
#' @details
#' The vocabulary comes from [AttainmentTables], which is the only reference
#' dataset covering all fourteen stages: [AgeTables] omits `"Ac"` because
#' apex closure is terminal and has no finite age in stage.
#'
#' Supplying `tooth` matters more than it looks. Not every stage exists for
#' every tooth: the single-rooted teeth (Canine, P3, P4) have no `"Cl.i"`
#' (root cleft initiation). Checked against the global vocabulary alone,
#' `"Cl.i"` on a canine is accepted, matches no reference row, and the tooth
#' is then silently dropped from the age estimate. The tooth-aware check
#' catches it instead.
#'
#' @seealso [score_to_stage()], [recode_score()]
#'
validate_score <- function(x, tooth = NULL) {
  x <- as.character(x)
  valid <- rep(TRUE, length(x))
  scored <- !is.na(x)

  if (is.null(tooth)) {
    valid[scored] <- x[scored] %in% unique(AttainmentTables$Stage)
    return(valid)
  }

  tooth <- as.character(tooth)
  if (length(tooth) == 1L) {
    tooth <- rep(tooth, length(x))
  } else if (length(tooth) != length(x)) {
    stop("`tooth` must be length 1 or the same length as `x`.",
         call. = FALSE)
  }

  known_teeth <- unique(AttainmentTables$Tooth)
  unknown <- setdiff(unique(tooth[!is.na(tooth)]), known_teeth)
  if (length(unknown) > 0L) {
    stop("Unknown tooth name: ", paste(unknown, collapse = ", "),
         ". Expected one of ", paste(known_teeth, collapse = ", "), ".",
         call. = FALSE)
  }

  # A stage is valid for a tooth only if the pair appears in the reference
  # data, so the anatomically impossible combinations fail here.
  allowed <- paste(AttainmentTables$Tooth, AttainmentTables$Stage)
  valid[scored] <- paste(tooth[scored], x[scored]) %in% allowed

  return(valid)
}
