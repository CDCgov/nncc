
#' case-control data
#'
#' A toy dataset containing 7-day exposure history of 250 cases and 250 controls
#'
#' @format A data frame with 500 rows and 11 variables:
#' \describe{
#'   \item{case}{case status, 1 = case, 0 = control}
#'   \item{exp01}{whether exposed to exp01, 1 = yes, 0 = no}
#'   \item{exp09}{whether exposed to exp09, 1 = yes, 0 = no}
#'   \item{exp20}{whether exposed to exp20, 1 = yes, 0 = no}
#'   \item{exp24}{whether exposed to exp24, 1 = yes, 0 = no}
#'   \item{exp27}{whether exposed to exp27, 1 = yes, 0 = no}
#'   \item{exp43}{whether exposed to exp43, 1 = yes, 0 = no}
#'   \item{exp45}{whether exposed to exp45, 1 = yes, 0 = no}
#'   \item{exp50}{whether exposed to exp50, 1 = yes, 0 = no}
#'   \item{exp52}{whether exposed to exp52, 1 = yes, 0 = no}
#'   \item{exp57}{whether exposed to exp57, 1 = yes, 0 = no}
#' }
"anifood"



#' Variables excluded from matching
#'
#' A dataset lists variables that are excluded from matching for each exposure.
#' This dataset is supplied to the \code{rmvars} argument of the function
#' \code{\link{make_knn_strata}}. The two columns must be named with "exp_var" and "rm_vars".
#'
#' @format A data frame with two variables:
#' \describe{
#'   \item{exp_var}{exposures of interest}
#'   \item{rm_vars}{variables to be excluded from matching for a given exposure}
#' }
"excl_vars"



#' Urinary Tract Infection in American College Students
#'
#' This data set deals with urinary tract infection in sexually active college women,
#' along with covariate information on age and contraceptive use.
#' The variables are all binary and coded in 1 (condition is present) and 0 (condition is absent).
#'
#' @format sex2: a data.frame containing 239 observations
#' \describe{
#' \item{case}{urinary tract infection, the study outcome variable}
#' \item{age}{>= 24 years}
#' \item{dia}{use of diaphragm}
#' \item{oc}{use of oral contraceptive}
#' \item{vic}{use of condom}
#' \item{vicl}{use of lubricated condom}
#' \item{vis}{use of spermicide}
#' }
#'
#' @source <https://www.cytel.com/>
#'
#' @references Cytel Inc., (2010) LogXact 9 user manual, Cambridge, MA:Cytel Inc
"sex2"
#' Urinary Tract Infection in American College Students
#'
#' This data set deals with urinary tract infection in sexually active college women,
#' along with covariate information on age an contraceptive use.
#' The variables are all binary and coded in 1 (condition is present) and 0 (condition is absent):
#' case (urinary tract infection, the study outcome variable), age (>= 24 years),
#' dia (use of diaphragm), oc (use of oral contraceptive), vic (use of condom),
#' vicl (use of lubricated condom), and vis (use of spermicide).
#'
#' @format sexagg: an aggregated data.frame containing 31 observations with case weights (COUNT).
#' \describe{
#' \item{case}{urinary tract infection, the study outcome variable}
#' \item{age}{>= 24 years}
#' \item{dia}{use of diaphragm}
#' \item{oc}{use of oral contraceptive}
#' \item{vic}{use of condom}
#' \item{vicl}{use of lubricated condom}
#' \item{vis}{use of spermicide}
#' }
#'
#' @source <https://www.cytel.com/>
#'
#' @references Cytel Inc., (2010) LogXact 9 user manual, Cambridge, MA:Cytel Inc
"sexagg"

