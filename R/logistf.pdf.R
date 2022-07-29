#' Likehood-ratio test of two models
#'
#' Please cite the original function \code{\link[logistf]{CLIP.confint}} for
#' publication.
#' @param x A model matrix returned by \code{\link[stats]{model.matrix}}
#' @param y The response of a model frame returned by \code{\link[stats]{model.response}}
#' @param pos Position of the variable (for which profile likelihood confidence
#' intervals should be computed) in the \code{colnames} of model matrix \code{x}
#' @param firth Use of Firth's penalized maximum likelihood (firth=TRUE, default)
#' or the standard maximum likelihood method (firth=FALSE)
#' @param weight An optional weighting variable for each observation
#' @param control Controls Newton-Raphson iteration. Default is
#' \code{control= logistf.control(maxstep, maxit, maxhs, lconv, gconv, xconv)}
#' @param plcontrol Controls Newton-Raphson iteration for the estimation of the
#' profile likelihood confidence intervals. Default is \code{plcontrol=
#' logistpl.control(maxstep, maxit, maxhs, lconv, xconv, ortho, pr)}
#' @param offset An optional offset variable
#' @param b A bound for the lower or upper limit
#' @param beta Regression coefficients for \code{colnames} of model matrix
#' \code{x} returned by \code{logistf::logistf()}
#' @param loglik The (penalized) log-likelihood of the full models
#' @param fit Output of \code{logistf.fit}
#' @param old Corresponding to the \code{legacy} argument in
#' \code{\link[logistf]{CLIP.confint}}
#' @importFrom  stats pchisq

logistf.pdf <- function (x, y, pos, firth = TRUE, weight, control, plcontrol,
          offset = NULL, b, beta = NULL, loglik = NULL, fit = NULL,
          old = FALSE) {
  if (missing(control))
    control <- logistf.control()
  if (missing(plcontrol))
    plcontrol <- logistpl.control()
  if (is.null(offset))
    offset <- rep(0, nrow(x))
  res <- matrix(0, 1, 3)
  k <- ncol(x)
  n <- nrow(x)
  if (is.null(fit) & is.null(beta) & is.null(loglik)) {
    if (old)
      fit <- logistf.fit.old(x, y, weight = weight, offset = offset,
                             firth = firth, control = control)
    else fit <- logistf.fit(x, y, weight = weight, offset = offset,
                            firth = firth, control = control)
    beta <- fit$coefficients
    loglik <- fit$loglik[2]
  }
  init <- beta
  init[pos] <- b
  if (old)
    xx <- logistf.fit.old(x, y, weight = weight, offset = offset,
                          firth = firth, col.fit <- (1:k)[-pos], init = init,
                          control = control)
  else xx <- logistf.fit(x, y, weight = weight, offset = offset,
                         firth = firth, col.fit <- (1:k)[-pos], init = init, control = control)
  res[1, 1] <- b
  res[1, 2] <- 2 * (loglik - xx$loglik)
  res[1, 3] <- 1 - (1 - pchisq(res[, 2], 1))/2
  res[1, 3][res[, 1] < beta[pos]] <- 1 - res[, 3][res[, 1] <
                                                    beta[pos]]
  results <- list(beta = res[1, 1], chisq = res[1, 2], pdf = res[1,
                                                                 3])
  results
}


#' Default control
#'
#' @param maxit Max iterations
#' @param maxhs Maxhs
#' @param maxstep Max steps
#' @param lconv lconv
#' @param gconv gconv
#' @param xconv xconv
#' @param collapse collapse
#'
logistf.control <- function (maxit = 25, maxhs = 5, maxstep = 5, lconv = 1e-05,
          gconv = 1e-05, xconv = 1e-05, collapse = TRUE) {
  list(maxit = maxit, maxhs = maxhs, maxstep = maxstep, lconv = lconv,
       gconv = gconv, xconv = xconv, collapse = collapse)
}



#' Default logistpl control
#'
#' @param maxit Max iterations
#' @param maxhs Maxhs
#' @param maxstep Max steps
#' @param lconv lconv
#' @param xconv xconv
#' @param ortho ortho
#' @param pr    pr
#'
logistpl.control <- function (maxit = 100, maxhs = 5, maxstep = 5, lconv = 1e-05,
          xconv = 1e-05, ortho = FALSE, pr = FALSE) {
  list(maxit = maxit, maxhs = maxhs, maxstep = maxstep, lconv = lconv,
       xconv = xconv, ortho = ortho, pr = pr)
}
