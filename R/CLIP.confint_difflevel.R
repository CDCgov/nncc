#' Combine confidence interval for \code{logistf::logistf()} after multiple imputation
#'
#' The function was modified from \code{\link[logistf]{CLIP.confint}} to combine
#' results from m imputed data sets that have different structures (e.g., a
#' covariate in a model have different levels across different imputed data
#' sets) on April 15, 2022.
#'
#' The formula in \code{\link[logistf]{logistf}} must be written as variable of
#' interest followed by covariates that have different levels across different
#' imputed data sets.
#'
#' For more information, please refer to the vignette using
#' \code{browseVignettes("nncc")} and the original function
#' \code{\link[logistf]{CLIP.confint}}.
#'
#' Please cite the original function \code{\link[logistf]{CLIP.confint}} for
#' publication.

#' @param variable Must be used to include variables of interest; each of
#'   variable of interest must have the same levels across different imputed
#'   data sets.
#' @inheritParams logistf::CLIP.confint
#' @importFrom stats as.formula model.matrix model.response model.frame uniroot
#' @importFrom graphics grid
#' @export

CLIP.confint.difflevel <- function (obj = NULL, variable = NULL, data, firth = TRUE, weightvar = NULL,
          control = logistf:::logistf.control(), ci.level = c(0.025, 0.975),
          pvalue = TRUE, offset = NULL, bound.lo = NULL, bound.up = NULL,
          legacy = FALSE)
{


  if (!is.null(obj)) {
      fits <- obj
      if (missing(data)){
        if (is.null(fits[[1]]$data)){
            stop("Please provide data either as list of imputed data sets or by calling logistf on the imputed data sets with dataout=TRUE.\\n")}
        else data <- lapply(1:length(fits), function(X) fits[[X]]$data)}
      else {data <- data}

      formula <- as.formula(fits[[1]]$call$formula)
      nimp <- length(data)
  }
  else {
    nimp <- length(data)
    stop(paste("Please provide a list of fit objects or a mira object with fits on the ",
               nimp, " imputed data sets.\\n"))
  }


  nvar <- length(variable)


  inner.CLIP <- function(myvar) {
    variable <- myvar
    cov.name <- colnames(model.matrix(formula, data = data[[1]])) # to ensure pos is correct, variable of interest should be written before covariates that have different levels across different imputed datasets
    pos <- match(variable, cov.name)
    old <- legacy
    imputations <- nimp
    if (is.null(variable))
      stop("You must specify a variable of interest.")
    res <- matrix(0, length(grid), 3)
    k <- lapply(1:imputations, function(i) {variable.names <- colnames(model.matrix(formula, data = data[[i]]))
                                            length(variable.names)
                                            })
    #imputation.indicator <- unlist(sapply(1:imputations,
    #                                      function(x) rep(x, nperimp[x]))[TRUE])
    #mat.data <- matrix(0, sum(nperimp), ncol(data[[1]]))
    #for (i in 1:imputations) mat.data[imputation.indicator ==
    #                                    i, 1:ncol(data[[i]])] <- as.matrix(data[[i]])
    #big.data <- data.frame(mat.data)
    #colnames(big.data) <- colnames(data[[1]])
    xyw <- lapply(1:imputations, function(i){

      xyw <- matrix(0, nrow(data[[i]]), k[[i]] + 2)
      xyw[, 1:k[[i]]] <- model.matrix(formula, data = data[[i]])
      xyw[, k[[i]] + 1] <- model.response(model.frame(as.formula(formula), data = data[[i]]))
      if (is.null(weightvar)) {
                              xyw[, k[[i]] + 2] <- rep(1, nrow(data[[i]]))
                              }  else xyw[, k[[i]] + 2] <- data[[i]][[weightvar]]
      xyw
    })

    # by default, bound.lo = NULL
    if (is.null(bound.lo)) {
      lower.collect <- (unlist(lapply(1:imputations, function(x) fits[[x]]$ci.lower[pos])))
      lowerbound.lo <- min(lower.collect)
      upperbound.lo <- max(lower.collect)
    }
    else {
      lowerbound.lo <- min(bound.lo)
      upperbound.lo <- max(bound.lo)
    }
    if (is.null(bound.up)) {
      upper.collect <- (unlist(lapply(1:imputations, function(x) fits[[x]]$ci.upper[pos])))
      lowerbound.up <- min(upper.collect)
      upperbound.up <- max(upper.collect)
    }
    else {
      lowerbound.up <- min(bound.up)
      upperbound.up <- max(bound.up)
    }
    if (lowerbound.lo == upperbound.lo) {
      lowerbound.lo <- lowerbound.lo - 1/2
      upperbound.lo <- upperbound.lo + 1/2
    }
    if (lowerbound.up == upperbound.up) {
      lowerbound.up <- lowerbound.up - 1/2
      upperbound.up <- upperbound.up + 1/2
    }
    estimate <- mean(unlist(lapply(1:imputations, function(x) fits[[x]]$coefficients[pos])))
    iter <- numeric(0)
    loglik <- unlist(lapply(1:imputations, function(x) fits[[x]]$loglik[2]))
    beta <- lapply(1:imputations, function(x) fits[[x]]$coefficients)

    lpdf <- function(zz, z) logistf.pdf(x = xyw[[zz]][,1:k[[zz]]], y = xyw[[zz]][,k[[zz]] + 1], weight = xyw[[zz]][,k[[zz]]+2],
                                                                     beta = beta[[zz]], loglik = loglik[zz], pos = pos,
                                        firth = firth, offset = offset, control = control,
                                        b = z, old = old)$pdf
    f = function(z) mean(unlist(lapply(1:imputations, function(zz) lpdf(zz,
                                                                        z))))
    f.lower <- f(lowerbound.lo) - ci.level[1]
    f.upper <- f(upperbound.lo) - ci.level[1]
    iter[1] <- 2
    itwhile <- 0
    while (f.lower > 0 & (upperbound.lo - lowerbound.lo) >
           0 & itwhile < 5) {
      itwhile <- itwhile + 1
      lowerbound.lo <- lowerbound.lo - (upperbound.lo -
                                          lowerbound.lo)/2
      f.lower <- f(lowerbound.lo) - ci.level[1]
      iter[1] <- iter[1] + 1
    }
    if (itwhile >= 5 & f.upper < 0)
      stop("pool.pl can not find a lower boundary for the lower confidence limit.\\n Try to increase number of imputations or supply boundaries by bound.lo=c(x,xx).\\n")
    itwhile <- 0
    while (f.upper < 0 & (upperbound.lo - lowerbound.lo) >
           0 & itwhile < 5) {
      itwhile <- itwhile + 1
      upperbound.lo <- upperbound.lo + (upperbound.lo -
                                          lowerbound.lo)/2
      f.upper <- f(upperbound.lo) - ci.level[1]
      iter[1] <- iter[1] + 1
    }
    if (itwhile >= 5 & f.upper < 0)
      stop("pool.pl can not find an upper boundary for the lower confidence limit.\\n Try to increase number of imputations or supply boundaries by bound.lo=c(x,xx).\\n")
    ci <- numeric(0)
    res.ci <- uniroot(f = function(z) {
      f(z) - ci.level[1]
    }, lower = lowerbound.lo, upper = upperbound.lo, f.lower = f.lower,
    f.upper = f.upper)
    ci[1] <- res.ci$root
    iter[1] <- res.ci$iter + iter[1]
    f.lower <- f(lowerbound.up) - ci.level[2]
    f.upper <- f(upperbound.up) - ci.level[2]
    iter[2] <- 2
    itwhile <- 0
    while (f.lower > 0 & (upperbound.up - lowerbound.up) >
           0 & itwhile < 5) {
      itwhile <- itwhile + 1
      lowerbound.up <- lowerbound.up - (upperbound.up -
                                          lowerbound.up)/2
      f.lower <- f(lowerbound.up) - ci.level[2]
      iter[2] <- iter[2] + 1
    }
    if (itwhile >= 5 & f.lower > 0)
      stop("pool.pl can not find a lower boundary for the upper confidence limit.\\n Try to increase number of imputations or supply boundaries by bound.up=c(x,xx).\\n")
    itwhile <- 0
    while (f.upper < 0 & (upperbound.up - lowerbound.up) >
           0 & itwhile < 5) {
      itwhile <- itwhile + 1
      upperbound.up <- upperbound.up + (upperbound.up -
                                          lowerbound.up)/2
      f.upper <- f(upperbound.up) - ci.level[2]
      iter[2] <- iter[2] + 1
    }
    if (itwhile >= 5 & f.upper < 0)
      stop("pool.pl can not find an upper boundary for the upper confidence limit.\\n Try to increase number of imputations or supply boundaries by bound.up=c(x,xx).\\n")
    res.ci <- uniroot(f = function(z) {
      f(z) - ci.level[2]
    }, lower = lowerbound.up, upper = upperbound.up, f.lower = f.lower,
    f.upper = f.upper)
    ci[2] <- res.ci$root
    iter[2] <- res.ci$iter + iter[2]
    if (pvalue == "TRUE") {
      pvalue1 <- f(0)
      pvalue <- 2 * min(pvalue1, (1 - pvalue1))
    }
    else pvalue <- NA
    res <- list(estimate = estimate, ci = ci, pvalue = pvalue,
                imputations = imputations, ci.level = ci.level, myvar = myvar,
                call = match.call(), bound.lo = c(lowerbound.lo,
                                                  upperbound.lo), bound.up = c(lowerbound.up, upperbound.up),
                iter = iter)
    return(res)
  }
  estimate <- numeric(nvar)
  ci <- matrix(0, nvar, 2)
  pvalue.out <- numeric(nvar)
  bound.LOW <- matrix(0, nvar, 2)  # edited bound.lo to bound.LOW. otherwise, this code erases the boundary you supplied
  bound.UPP <- matrix(0, nvar, 2)
  iter <- matrix(0, nvar, 2)
  for (i in 1:nvar) {
    res.tmp <- inner.CLIP(myvar = variable[i])
    estimate[i] <- res.tmp$estimate
    ci[i, ] <- res.tmp$ci
    pvalue.out[i] <- res.tmp$pvalue
    bound.LOW[i, ] <- res.tmp$bound.lo
    bound.UPP[i, ] <- res.tmp$bound.up
    iter[i, ] <- res.tmp$iter
  }
  res <- list(variable = variable, estimate = estimate, ci = ci,
              pvalue = pvalue.out, imputations = res.tmp$imputations,
              ci.level = ci.level, bound.lo = bound.LOW, bound.up = bound.UPP,
              iter = iter, call = match.call())
  attr(res, "class") <- "CLIP.confint"
  return(res)
}


