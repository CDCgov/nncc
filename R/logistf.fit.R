#' Fit a Firth logistic regression model
#'
#' @param x A model matrix returned by \code{\link[stats]{model.matrix}}
#' @param y The response of a model frame returned by \code{\link[stats]{model.response}}
#' @param weight An optional weighting variable for each observation
#' @param offset An optional offset variable
#' @param firth Use of Firth's penalized maximum likelihood (firth=TRUE, default)
#' or the standard maximum likelihood method (firth=FALSE)
#' @param col.fit Numerical vector containing the positions of the variables to
#' fit, if not specified: all variables are taken
#' @param init Specifies the initial values of the coefficients for the fitting algorithm
#' @param control Controls Newton-Raphson iteration. Default is
#' \code{control= logistf.control(maxstep, maxit, maxhs, lconv, gconv, xconv)}
#' @import mgcv
#' @useDynLib nncc logistffit

logistf.fit <- function (x, y, weight = NULL, offset = NULL, firth = TRUE, col.fit = NULL,
          init = NULL, control) {
  n <- nrow(x)
  k <- ncol(x)
  collapse <- control$collapse
  coll <- FALSE
  if (collapse && isTRUE(all.equal(weight, rep(1, length(weight)))) &&
      isspecnum(col.fit, 1)) {
    xy <- cbind(x, y)
    temp <- unique(unlist(sapply(1:ncol(xy), function(X) unique(xy[,
                                                                   X]))))
    if (length(temp) <= 10) {
      xc <- uniquecombs(cbind(x, y, offset))
      xorig <- x
      yorig <- y
      weight <- table(attr(xc, "index"))
      x <- xc[, 1:k]
      y <- xc[, k + 1]
      if (!is.null(offset))
        offset <- xc[, k + 2]
      n <- nrow(xc)
      coll <- TRUE
    }
  }
  if (is.null(init))
    init = rep(0, k)
  if (is.null(col.fit))
    col.fit = 1:k
  if (is.null(offset))
    offset = rep(0, n)
  if (is.null(weight))
    weight = rep(1, n)
  if (missing(control))
    control <- logistf.control()
  if (col.fit[1] == 0)
    maxit <- 0
  else maxit <- control$maxit
  maxstep <- control$maxstep
  maxhs <- control$maxhs
  lconv <- control$lconv
  gconv <- control$gconv
  xconv <- control$xconv
  beta <- init
  firth <- if (firth)
    1
  else 0
  ncolfit <- length(col.fit)
  covar <- matrix(0, k, k)
  Ustar <- double(k)
  pi <- double(n)
  Hdiag <- double(n)
  loglik <- evals <- iter <- 0
  conv <- double(3)
  mode(x) <- mode(weight) <- mode(beta) <- mode(offset) <- "double"
  mode(y) <- mode(firth) <- mode(n) <- mode(k) <- "integer"
  mode(maxstep) <- mode(lconv) <- mode(gconv) <- mode(xconv) <- "double"
  mode(loglik) <- "double"
  mode(col.fit) <- mode(ncolfit) <- mode(maxit) <- mode(maxhs) <- "integer"
  mode(evals) <- mode(iter) <- "integer"
  res <- .C("logistffit", x, y, n, k, weight, offset,
            beta = beta, col.fit, ncolfit, firth, maxit, maxstep,
            maxhs, lconv, gconv, xconv, var = covar, Ustar = Ustar,
            pi = pi, Hdiag = Hdiag, loglik = loglik, evals = evals,
            iter = iter, conv = conv, PACKAGE = "nncc")
  if (coll) {
    res$pi <- res$pi[attr(xc, "index")]
    res$Hdiag <- as.numeric((res$Hdiag/weight)[attr(xc, "index")])
  }
  res <- res[c("beta", "var", "Ustar", "pi",
               "Hdiag", "loglik", "evals", "iter",
               "conv")]
  res
}


#' Is specnum
#' @param x A Numerical vector
#' @param a A constant
isspecnum <- function (x, a) {
  if (length(x) == 1) {
    if (x == a)
      return(TRUE)
    else return(FALSE)
  }
  else return(FALSE)
}

