
#' case-control data
#'
#' A dataset containing 7-day exposure history of 1241 cases and 1241 controls
#'
#' @format A data frame with 2482 rows and 58 variables:
#' \describe{
#'   \item{case}{case status, 1 = case, 0 = control}
#'   \item{exp01}{whether exposed to exp01, 1 = yes, 0 = no}
#'   \item{exp02}{whether exposed to exp02, 1 = yes, 0 = no}
#'   \item{exp03}{whether exposed to exp03, 1 = yes, 0 = no}
#'   \item{exp04}{whether exposed to exp04, 1 = yes, 0 = no}
#'   \item{exp05}{whether exposed to exp05, 1 = yes, 0 = no}
#'   \item{exp06}{whether exposed to exp06, 1 = yes, 0 = no}
#'   \item{exp07}{whether exposed to exp07, 1 = yes, 0 = no}
#'   \item{exp08}{whether exposed to exp08, 1 = yes, 0 = no}
#'   \item{exp09}{whether exposed to exp09, 1 = yes, 0 = no}
#'   \item{exp10}{whether exposed to exp10, 1 = yes, 0 = no}
#'   \item{exp11}{whether exposed to exp11, 1 = yes, 0 = no}
#'   \item{exp12}{whether exposed to exp12, 1 = yes, 0 = no}
#'   \item{exp13}{whether exposed to exp13, 1 = yes, 0 = no}
#'   \item{exp14}{whether exposed to exp14, 1 = yes, 0 = no}
#'   \item{exp15}{whether exposed to exp15, 1 = yes, 0 = no}
#'   \item{exp16}{whether exposed to exp16, 1 = yes, 0 = no}
#'   \item{exp17}{whether exposed to exp17, 1 = yes, 0 = no}
#'   \item{exp18}{whether exposed to exp18, 1 = yes, 0 = no}
#'   \item{exp19}{whether exposed to exp19, 1 = yes, 0 = no}
#'   \item{exp20}{whether exposed to exp20, 1 = yes, 0 = no}
#'   \item{exp21}{whether exposed to exp21, 1 = yes, 0 = no}
#'   \item{exp22}{whether exposed to exp22, 1 = yes, 0 = no}
#'   \item{exp23}{whether exposed to exp23, 1 = yes, 0 = no}
#'   \item{exp24}{whether exposed to exp24, 1 = yes, 0 = no}
#'   \item{exp25}{whether exposed to exp25, 1 = yes, 0 = no}
#'   \item{exp26}{whether exposed to exp26, 1 = yes, 0 = no}
#'   \item{exp27}{whether exposed to exp27, 1 = yes, 0 = no}
#'   \item{exp28}{whether exposed to exp28, 1 = yes, 0 = no}
#'   \item{exp29}{whether exposed to exp29, 1 = yes, 0 = no}
#'   \item{exp30}{whether exposed to exp30, 1 = yes, 0 = no}
#'   \item{exp31}{whether exposed to exp31, 1 = yes, 0 = no}
#'   \item{exp32}{whether exposed to exp32, 1 = yes, 0 = no}
#'   \item{exp33}{whether exposed to exp33, 1 = yes, 0 = no}
#'   \item{exp34}{whether exposed to exp34, 1 = yes, 0 = no}
#'   \item{exp35}{whether exposed to exp35, 1 = yes, 0 = no}
#'   \item{exp36}{whether exposed to exp36, 1 = yes, 0 = no}
#'   \item{exp37}{whether exposed to exp37, 1 = yes, 0 = no}
#'   \item{exp38}{whether exposed to exp38, 1 = yes, 0 = no}
#'   \item{exp39}{whether exposed to exp39, 1 = yes, 0 = no}
#'   \item{exp40}{whether exposed to exp40, 1 = yes, 0 = no}
#'   \item{exp41}{whether exposed to exp41, 1 = yes, 0 = no}
#'   \item{exp42}{whether exposed to exp42, 1 = yes, 0 = no}
#'   \item{exp43}{whether exposed to exp43, 1 = yes, 0 = no}
#'   \item{exp44}{whether exposed to exp44, 1 = yes, 0 = no}
#'   \item{exp45}{whether exposed to exp45, 1 = yes, 0 = no}
#'   \item{exp46}{whether exposed to exp46, 1 = yes, 0 = no}
#'   \item{exp47}{whether exposed to exp47, 1 = yes, 0 = no}
#'   \item{exp48}{whether exposed to exp48, 1 = yes, 0 = no}
#'   \item{exp49}{whether exposed to exp49, 1 = yes, 0 = no}
#'   \item{exp50}{whether exposed to exp50, 1 = yes, 0 = no}
#'   \item{exp51}{whether exposed to exp51, 1 = yes, 0 = no}
#'   \item{exp52}{whether exposed to exp52, 1 = yes, 0 = no}
#'   \item{exp53}{whether exposed to exp53, 1 = yes, 0 = no}
#'   \item{exp54}{whether exposed to exp54, 1 = yes, 0 = no}
#'   \item{exp55}{whether exposed to exp55, 1 = yes, 0 = no}
#'   \item{exp56}{whether exposed to exp56, 1 = yes, 0 = no}
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

