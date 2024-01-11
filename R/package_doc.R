
#' nncc: nearest-neighbors matching for case-control data
#'
#' The \code{nncc} package implements an approach to match cases with their
#' nearest controls defined by Gower distance. This approach may achieve
#' better confounding control than conventional analytic approaches such as
#' (conditional) logistic regression when you have a relatively large number of
#' exposures of interest. To learn more
#' about \code{nncc}, start with the vignettes: \code{browseVignettes("nncc")}.
#'
#' @docType package
#' @name nncc
#' @aliases case_control matching nearest_neighbors
#'
#' @section Authors(s):
#'
#' \strong{Maintainer}: Beau B. Bruce \email{lue7@@cdc.gov}
#'
#' Coauthor: Zhaohui Cui
#'
#' @section Functions:
#' \itemize{
#'   \item \code{\link{get_threshold}}
#'   \item \code{\link{distance_density_plot}}
#'   \item \code{\link{threshold_model_plot}}
#'   \item \code{\link{original_compare_plot}}
#'   \item \code{\link{make_knn_strata}}
#'   \item \code{\link{make_analysis_sets}}
#'   \item \code{\link{finalize_data}}
#'   \item \code{\link{test_mh}}
#'   \item \code{\link{get_paf}}}
NULL

## quiets concerns of R CMD check re: the .'s and other names that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "case", "closest", "cntlpat", "conf.int1",
                                                        "conf.int2", "dist", "estimate.common.odds.ratio",
                                                        "estimate.odds.ratio", "exp_var", "fn", "idx", "ncase",
                                                        "ncontrol", "newstrata", "nstrata", "nsubj", "or",
                                                        "p.value", "p_j", "p_j_R_j", "prob", "qcase", "strata",
                                                        "sub_case", "total_case", "val", "var", "xmax", "xmin",
                                                        "ymax", "ymin"))
