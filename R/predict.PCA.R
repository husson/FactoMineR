predict.PCA <- function(object, newdata,...){
    if (!is.null(colnames(newdata))) {
	  if (any(!(rownames(object$var$coord)%in%colnames(newdata)))) warning("The names of the variables is not the same as the ones in the active variables of the PCA result")
	}
    ecart.type <- object$call$ecart.type
    centre <- object$call$centre
    col.w <- object$call$col.w 
	ncp <- ncol(object$var$coord)
    if (is.null(ecart.type)) ecart.type <- rep(1, length(centre))
	newdata <- newdata[,rownames(object$var$coord)]
    newdata <- t(t(as.matrix(newdata))-centre)
    newdata <- t(t(newdata)/ecart.type)
    coord <- t(t(newdata)*col.w)
    coord <- crossprod(t(coord),object$svd$V)
    dist2 <- rowSums(t(t(newdata^2)*col.w))
    cos2 <- coord^2/dist2
    coord <- coord[, 1:ncp, drop = F]
    cos2 <- cos2[, 1:ncp, drop = F]
    colnames(coord) <- colnames(cos2) <- paste("Dim",  c(1:ncp), sep = ".")
    rownames(coord) <- rownames(cos2) <- names(dist2) <- rownames(newdata)
    result <- list(coord = coord, cos2 = cos2, dist = sqrt(dist2))
	return(result)
}