predict.CA <- function(object, newdata, ...){
    if (!inherits(object, "CA")) print("object should be a CA object")
    if (!is.null(colnames(newdata))) {
	  if (any(!(rownames(object$col$coord)%in%colnames(newdata)))) warning("The names of the columns is not the same as the ones in the active columns of the CA result")
	}
	newdata <- newdata[,rownames(object$col$coord)]
    marge.col <- object$call$marge.col
    col.w <- object$call$col.w 
	ncp <- ncol(object$row$coord)
	
    somme.row <- rowSums(newdata)
    newdata <- newdata/somme.row
    coord <- crossprod(t(as.matrix(newdata)),object$svd$V)
    dist2.row <- rowSums(t((t(newdata)-marge.col)^2/marge.col))
    cos2 <- coord^2/dist2.row
    coord <- coord[, 1:ncp,drop=FALSE]
    cos2 <- cos2[, 1:ncp,drop=FALSE]
    colnames(coord) <- colnames(cos2) <- paste("Dim", 1:ncp)
    rownames(coord) <- rownames(cos2) <- rownames(newdata)
    result <- list(coord = coord, cos2 = cos2)
	return(result)
}



