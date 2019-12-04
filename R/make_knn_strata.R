#' Make case-control strata using k nearest neighbors (knn)
#'
#' @param expvar Character - name of exposure variable in df.
#' @param ncntls Numeric - number of controls to find for each case (k in knn).
#' @param df Data.frame - data set.
#' @param rmvars Charater vector - additional variables to exclude
#' @param casevar Charater - what is the variable indicating case status (1 = case, 0 = control)
#' @param matchvars Character vector - what are the variables to match on.  Note that the function automatically excludes the
#' the exposure varible when placed in the list which facilitates *apply function and loop constructs
#' @param metric Character - metric to be used. See \code{\link{cluster::daisy}}.
#' @param silent Suppress exposure info useful for *apply/loop implementations?
#' @import dplyr
#' @export
make_knn_strata <- function(expvar, matchvars, df, rmvars = character(), casevar = "case",
                            ncntls = 250, metric = "gower", silent = FALSE) {
  if(!silent) message(expvar)
  caseidx <- (1:NROW(df))[df[[casevar]] == 1]
  minusexp <- df[, setdiff(matchvars, c(expvar, rmvars))] # make sure exposure var is out
  me.daisy <- cluster::daisy(minusexp, metric = metric) 
  me.dist <- as.matrix(me.daisy)
  me.dist <- me.dist[, -caseidx]

  # find the nearest neighbors, be careful with library functions,
  # they typically assume the case itself is included
  # (and therefore those functions truncate
  # the closest assuming it has distance zero, etc.)
  lapply(seq_along(caseidx), function(i) {
      me.dist[caseidx[i], order(me.dist[caseidx[i], ])] %>%
          head(ncntls) %>%
          { data.frame(strata = caseidx[i],
                       idx = as.integer(match(names(.),
                                              rownames(df))),
                       dist = ., case = 0) } %>%
          bind_rows(data.frame(strata = caseidx[i],
                               idx = caseidx[i], dist = 0, case = 1))
  # finally merge all the datasets together
  }) %>% do.call(rbind, .) 
}
