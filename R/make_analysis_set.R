#' Make analysis set
#' 
#' Allow each control to only appear once,
#' keeping the closest case-control pair
#' after merging exposure data and removing strata with 
#' cases who are missing exposure information completely
#' @param stratifed_data Stratified dataset, see \code{\link{make_knn_strata}}
#' @param expvar Character of current exposure variable in d
#' @param data Original case control data
#' @param maxdist Reject any controls more than maxdist from their case
#' @param maxcontrols Maximum number of controls to keep per strata
#' @param silent Suppress exposure info useful for *apply/loop implementations?
#' @import dplyr
#' @export
make_analysis_set <- function(stratified_data, var, data,
                              maxdist = 0, maxcontrols = 20, silent = FALSE) {
    if(!silent) message(var)
    stratified_data %>%
        ## merge in the exposure and remove any who are missing it
        mutate(exp = data[[var]][idx]) %>% 
        filter(!is.na(exp)) %>%
        ## remove strata that no longer have a case
        group_by(strata) %>% 
        left_join(summarize(., qcase = sum(case)), by = "strata") %>% 
        filter(qcase == 1) %>% select(-qcase) %>%
        ## remove controls / merge strata with same control patterns
        group_by(strata) %>%
        top_n(maxcontrols + 1, -dist) %>%  # limit size of strata to maxstrata (+1 for the case)
        ungroup %>% filter(dist <= maxdist) %>% # keep only pairs closer than maxdist
        ## the merge strata operation...
        left_join(filter(., case == 0) %>% # first create the cntl pattern by pasting together the idx of each strata's controls
                  group_by(strata) %>%
                  summarize(cntlpat = paste(sort(idx), collapse = ", ")), by = "strata") %>%
        # remove strata without cntls because they have a cntlpat of NA
        filter(!is.na(cntlpat)) %>%
        left_join(group_by(., cntlpat) %>% # next use that to create a "case" pattern, i.e.,
                                           # newstrata for those with equal cntl patterns, a merge of old strata
                  summarize(newstrata = paste(sort(unique(strata)), collapse = ", ")), by = "cntlpat") %>%
        ungroup %>% mutate(newstrata = ifelse(is.na(newstrata), strata, newstrata)) %>%
        rename(oldstrata = strata, strata = newstrata) %>%
        ## ...end the merge operation and remove lone cases after removing controls
        group_by(strata) %>%
        left_join(summarize(., nsubj = n()), by = "strata") %>%
        filter(nsubj > 1) %>% select(-nsubj) %>%
        ## finalize and filter
        ungroup %>% arrange(strata, dist)
}

#' Make analysis datasets
#'
#' make_analysis_sets(cc, strata2, cc_vars, threshhold_results$threshold)
#' @param stratifed_data List of stratified datasets, see \code{\link{make_knn_strata}}
#' @param expvars Character vector of exposure variable for each set in \code{stratifed_data}
#' @param data Original case control data
#' @param threshold Maximum distance threshold for cases and controls
#' @export
make_analysis_sets <- function(stratified_data, expvars, data, threshold) { 
    mapply(make_analysis_set,
              stratified_data,
              expvars, # not other vars although other_vars was used to do the matching!
              SIMPLIFY = FALSE,
              MoreArgs = list(data = data,
                              maxdist = threshold)) %>%  
    setNames(expvars)
}
