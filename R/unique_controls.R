#' Ensures controls are unique to avoid potential pseudoreplication issues
#'
#' @param stratifieddata See \code{\link{make_knn_strata}} and \code{\link{make_analysis_set}}.
#' @export
unique_controls <- function(stratifieddata) {
    stratifieddata %>%
        left_join(group_by(., strata) %>%
        summarize(nstrata = n()), by = "strata") %>% # create this so you can keep the smallest strata
        group_by(idx) %>% arrange(nstrata, dist) %>%  # preserve smallest strata then smallest distance if tied
        distinct(idx, .keep_all = TRUE) %>%
        filter(nstrata > 1) %>% 
        select(-nstrata) %>%
        arrange(strata, desc(case))
}
