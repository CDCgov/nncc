inverseLinpack <-
function(x=diag(3)) {
if(nrow(x) != ncol(x))
stop("wrong dimensions!")
obj <- .C("linpack_inv_det", res=as.double(x),
as.integer(nrow(x)), det=as.double(0), PACKAGE="nncc")
list(inverse=matrix(obj$res, nrow(x)), det=obj$det)
}

