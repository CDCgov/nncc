
#' Calculate population attributable fraction using odds ratio
#'
#' @param df_or A data frame that stores odds ratios for all exposure of
#'   interest
#' @param which_or An unquoted name of the name of the column that stores odds
#'   ratio, or its lower or upper confidence limit in \code{df_or}.
#' @param exp_var An unquoted name of the column that stores the name of
#'   exposures in \code{df_or}
#' @param exp_level An unquoted name of the column that stores the level of the
#'   exposure variable in \code{df_or}
#' @param df_matched The list of data frames used to calculate odds ratios
#' @import rlang dplyr
#' @export
#' @return A data frame.
#' @details Use odds ratio, its upper confidence limit, and its lower confidence
#'   limit to calculate population attributable fraction, its upper confidence
#'   limit, and its lower confidence limit, respectively.
#'
#'   For more information, please refer to the vignette using
#'   \code{browseVignettes("nncc")}.


get_paf <- function(df_or, which_or, exp_var, exp_level, df_matched){
  which_or <- enquo(which_or)
  exp_var <- enquo(exp_var)
  exp_level <- enquo(exp_level)

  furrr::future_map(unique(df_or[[rlang::as_name(exp_var)]]), function(var){
  or_df <- df_or %>%
    filter(!!exp_var == !!var) %>%
    select(!!which_or, !!exp_level) %>%
    mutate(level = as.character(!!exp_level))

    df_matched[[var]]%>%
      arrange(exp, strata) %>%
      mutate(total_case = sum(case)) %>%
      group_by(exp, strata) %>%
      mutate(sub_case = sum(case)) %>%
      distinct(exp, strata, .keep_all = TRUE) %>%
      ungroup() %>%
      filter(case == 1) %>%
      mutate(level = paste0("exp", exp)) %>%
      left_join(or_df) %>%
      mutate(!!which_or := ifelse(is.na(!!which_or), 1, !!which_or)) %>%
      mutate(p_j = sub_case/total_case, p_j_R_j = p_j/!!which_or) %>%
      summarise(paf = 1-sum(p_j_R_j))

  },
  .progress = TRUE) %>%
    Reduce(rbind, .) %>%
    `colnames<-`(paste0("paf_", rlang::as_name(which_or)))%>%
 mutate(!!exp_var := df_or[[rlang::as_name(exp_var)]])}
