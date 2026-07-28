#' @keywords internal
#'
#' @importFrom utils data
#' @importFrom stats var rlnorm
#' @importFrom dplyr filter select if_else case_when
"_PACKAGE"

# Ignore some global variables
utils::globalVariables(c("AgeTables",
                         "Sex",
                         "Tooth",
                         "Stage",
                         "log_mu",
                         "log_sd"))
