#' Make case-control strata using k nearest neighbors (knn)
#'
#' Select a pre-defined number of controls for each case based on calculated
#' distances between cases and controls.
#'
#' For more information, please refer to the vignette using
#' \code{browseVignettes("nncc")}.
#'
#' @param expvar A character - the name of the exposure variable in \code{df}.
#' @param ncntls An integer to specify number of controls to find for each case
#'   (k in knn).
#' @param df A dataframe that contains the case-control data.
#' @param rmvars A data frame that lists variables to be excluded from matching
#'   for each exposure. For details, please see the vignette of this package.
#' @param casevar A character - what is the name of the variable indicating case
#'   status (1 = case, 0 = control)
#' @param matchvars Character vector - what are the variables to match on. Note
#'   that the function automatically excludes the the exposure variable.
#' @param metric A character to specify a metric for measuring distance between
#'   a case and a control. See \code{\link[cluster]{daisy}}.
#' @param silent Suppress exposure info useful for *apply/loop implementations?
#' @import dplyr
#' @importFrom utils head
#' @export
#' @return A list of data frames with a \code{length} of number of exposures of
#'   interest.
make_knn_strata <- function(expvar, matchvars, df,
                            rmvars = data.frame(exp_var = character(), rm_vars = character(), stringsAsFactors = FALSE),
                            casevar = "case",
                            ncntls = 250, metric = "gower", silent = FALSE) {
  if(!silent) message(expvar)
  caseidx <- (1:NROW(df))[df[[casevar]] == 1]
  if (!is.data.frame(rmvars)) {
    stop("Please supply a data frame to the rmvars argument")
  } else if (is.data.frame(rmvars)) {
    rmvars <- rmvars %>% filter(exp_var == expvar) %>% .[["rm_vars"]] %>% unlist
  }
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
