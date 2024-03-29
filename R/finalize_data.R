#' Final cleaning of the matched dataset(s)
#'
#' Ensures that a control retained in a data frame is used once and remove strata
#' without any case or any control. In this process, priority is first given
#' to the smallest strata then smallest distance if a control is matched to
#' multiple cases (i.e., that control exists in multiple strata).
#'
#' For more information, please refer to the vignette using
#' \code{browseVignettes("nncc")}.
#'
#' @param dfs A list of data frames generated by
#'   \code{\link{make_analysis_sets}}
#' @param filter Filter statement to apply
#' @param filterdata Extra data to left join to the \code{dfs} for filtering
#' @return A list of data frames
#' @import dplyr rlang
#' @importFrom stats setNames
#' @export

finalize_data <- function(dfs, filter = TRUE, filterdata = NULL) {
  filter <- enquo(filter)

  out <- lapply(names(dfs), function(v) {
    message(v)
        dfs[[v]] %>%
        unique_controls %>%
        group_by(strata) %>%
        left_join(summarize(., nstrata = n()), by = "strata") %>%
        ungroup %>% filter(nstrata > 1) %>% select(-nstrata) %>%
        (function(d) if(!is.null(filterdata)) { left_join(d, filterdata) } else { d }) %>%
        filter(!!filter) %>%
        fix_df %>%
        group_by(strata) %>%
        left_join(summarise(., ncase = sum(case), ncontrol = sum(case == 0))) %>%
        filter(ncase > 0 & ncontrol > 0) %>% select(-ncase, -ncontrol) %>%
        ungroup
  }) %>% setNames(names(dfs))
  out
}
