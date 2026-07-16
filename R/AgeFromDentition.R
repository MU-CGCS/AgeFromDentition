#' AgeFromDentition
#'
#' Estimate dental age from scores of dental development.
#'
#' \tabular{ll}{ Package: \tab AgeFromDentition\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.1\cr
#'               Date: \tab 2021-06-25\cr
#'               License: \tab MIT\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name AgeFromDentition-package
#' @aliases AgeFromDentition-package AgeFromDentition
#' @docType package
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#' @keywords package
#'
#' @importFrom utils data
#' @importFrom stats var rlnorm
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select if_else case_when
NULL

# Ignore some global variables
utils::globalVariables(c("AgeTables",
                         "Sex",
                         "Tooth",
                         "Stage",
                         "log_mu",
                         "log_sd"))
