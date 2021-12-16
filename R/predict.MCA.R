predict.MCA <- function(object, newdata, ...){
    if (!inherits(object, "MCA")) print("object should be a MCA object")
    if (!is.null(colnames(newdata))) {
	  if (any(!(rownames(object$var$eta)%in%colnames(newdata)))) warning("The names of the variables is not the same as the ones in the active variables of the MCA result")
	}
	olddata <- object$call$X[,rownames(object$var$eta),drop=FALSE]
	newdata <- newdata[,colnames(olddata),drop=FALSE]
	pb <- NULL
	for (i in 1:ncol(newdata)) {
	  if (sum(!levels(newdata[,i])%in%levels(olddata[,i]))>0) pb <- c(pb, levels(newdata[,i])[which(!levels(newdata[,i])%in%levels(olddata[,i]))])
	}
	if (!is.null(pb)) stop("The following categories are not in the active dataset: ",pb)
	newdata <- rbind(olddata,newdata)[-(1:nrow(olddata)),,drop=FALSE]
	tab.newdata <- tab.disjonctif(newdata)
    marge.col <- object$call$marge.col
	ncp <- ncol(object$ind$coord)
	
    somme.row <- rowSums(tab.newdata)
    tab.newdata <- tab.newdata/somme.row
    coord <- crossprod(t(as.matrix(tab.newdata)),object$svd$V)
    dist2.row <- rowSums(t((t(tab.newdata)-marge.col)^2/marge.col))
    cos2 <- coord^2/dist2.row
    coord <- coord[, 1:ncp,drop=FALSE]
    cos2 <- cos2[, 1:ncp,drop=FALSE]
    colnames(coord) <- colnames(cos2) <- paste("Dim", 1:ncp)
    rownames(coord) <- rownames(cos2) <- rownames(newdata)
    result <- list(coord = coord, cos2 = cos2)
	return(result)
}
