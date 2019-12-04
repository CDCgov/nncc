#' Fix the strata so they all have at least one case and control
#'
#' @param d A stratified dataset
fix_df <- function(d) {
        d %>% group_by(strata) %>%
        left_join(summarize(., nsubj = n()), by = "strata") %>%
        filter(nsubj > 1) %>% select(-nsubj) %>%
        ## finalize and filter
        ungroup %>% arrange(strata, dist)
}
