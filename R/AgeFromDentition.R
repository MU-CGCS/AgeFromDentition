#' @keywords internal
#'
#' @importFrom utils data
#' @importFrom stats var rlnorm qnorm qlnorm
#' @importFrom dplyr filter select mutate if_else case_when
"_PACKAGE"

# Ignore some global variables
utils::globalVariables(c(
    "AgeTables",
    "AttainmentTables",
    "StageTies",
    "Sex",
    "Tooth",
    "Stage",
    "Stage_hi",
    "tie_class",
    "log_mu",
    "log_sd",
    "se_log_mu",
    "sd_eff",
    "threshold",
    "n"
))
