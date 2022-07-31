
#' Calculate the pooled strata OR
#'
#' Each case and matched controls form a stratum in the data set. This function
#' is to calculate the pooled OR for the data set.
#'
#' @param dfs A named list of dataframes created by package functions
#' @param filter Filter statement to apply
#' @param filterdata Extra data to left join to the \code{dfs} for filtering
#' @details Uses the M-H method unless there is only one strata for which the
#'   fisher.test is used. For more information, please refer to the vignette
#'   using \code{browseVignettes("nncc")}.
#' @importFrom utils read.delim2
#' @importFrom stats setNames
#'
calc_strata_or <- function(dfs, filter = TRUE, filterdata = NULL) {
    filter <- enquo(filter)

    out <- lapply(names(dfs), function(v) {
        message(v)
        withCallingHandlers(tryCatch(
            dfs[[v]] %>%
            unique_controls %>%
            group_by(strata) %>%
            left_join(summarize(., nstrata = n()), by = "strata") %>%
            ungroup %>% filter(nstrata > 1) %>% select(-nstrata) %>%
            (function(d) if(!is.null(filterdata)) { left_join(d, filterdata) } else { d }) %>%
            filter(!!filter) %>%
            fix_df %>%
            with(test_mh(case, exp, strata)),
            error = function(e) NULL),
            warning = function(e) { cat(sprintf("%s\t%s\n", v, e$message),
                                        file = fn, append = TRUE)
                                        invokeRestart("muffleWarning")})
    })
    tryCatch({ issues <- read.delim2(fn, header = FALSE) %>%
                   setNames(c("var", "issue")) %>%
                   print }, error = function(e) NULL) # error means no issues
    out
}


#' Calculate odds ratios
#'
#' Calculate odds ratios using the M-H method when the matched dataset has more
#' than 1 stratum, and using the Fisher's exact test when the matched dataset
#' has only one stratum.
#'
#' For more information, please refer to the vignette using
#' \code{browseVignettes("nncc")}.
#'
#' @param case The case statuses
#' @param exp  The exposure statuses
#' @param strata The strata identifiers
#' @importFrom stats fisher.test mantelhaen.test
#' @export
test_mh <- function(case, exp, strata) {
    # need this because sometimes you are left with only
    # one strata
    if(length(unique(strata)) == 1) {
        o <- fisher.test(case, exp)
    } else {
        o <- mantelhaen.test(case, exp, strata)
    }
    # common results
    o[c("p.value", "conf.int", "estimate", "null.value", "alternative", "method", "data.name")]
 }

#' Format strata output into CSV
#'
#' @param results Output of \code{\link{test_mh}}
#' @param varnames Vector of exposure variable names
#' @param filename    String of the filename to output to
#' @return Returns the filename to allow chaining
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom stats setNames p.adjust
#' @importFrom utils write.csv
#' @export
write_strata_or_output <- function(results, varnames, filename) {
    lapply(results, function(d) d %>% unclass %>% unlist %>% data.frame(names(.), .)) %>%
    { tibble(var = varnames, ncol = sapply(., NCOL), data = .) } %>%
    filter(ncol == 2) %>% rowwise %>%
    do(data.frame(var = .$var, spread(.$data %>% setNames(c("var", "val")), var, val))) %>%
    mutate(or = as.numeric(estimate.common.odds.ratio)) %>%
    { if(is.null(.$estimate.odds.ratio)) { mutate(., estimate.odds.ratio = or) } else { . } } %>%
    mutate(or = ifelse(is.na(or), as.numeric(estimate.odds.ratio), or),
           p.value = as.numeric(p.value)) %>%
    select(var, or, conf.int1, conf.int2, p.value) %>% ungroup %>%
    mutate(or = as.numeric(or), p.value = if_else(is.nan(p.value), NA_real_, p.value)) %>%
    mutate(p.adj = p.adjust(p.value, "BY")) %>%
    arrange(desc(or)) %>% write.csv(file = filename)
    invisible(filename)
}
