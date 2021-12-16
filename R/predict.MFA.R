predict.MFA <- function(object, newdata, ...){
## newdata : les donnees pour les individus supplementaires
## object : la sortie de l'AFM sur les donnees actives
 ec <- function(V, poids) {
        res <- sqrt(sum(V^2 * poids,na.rm=TRUE)/sum(poids[!is.na(V)]))
    }

if (!is.null(object$quanti.var$coord)) ncp <- ncol(object$quanti.var$coord)
else ncp <- ncol(object$quali.var$coord)

tab.supp <- matrix(NA,nrow(newdata),0)
for (g in 1:length(object$call$group)){
  if (object$call$nature.group[g]=="quanti"){
#    tab.aux <- sweep(newdata[,(c(1,1+cumsum(object$call$group))[g]):cumsum(object$call$group)[g]],2,object$separate.analyses[[g]][["call"]]$centre,FUN="-")
#    tab.aux <- sweep(tab.aux,2,object$separate.analyses[[g]][["call"]]$ecart.type,FUN="/")
    tab.aux <- t(t(newdata[,(c(1,1+cumsum(object$call$group))[g]):cumsum(object$call$group)[g]]) - object$separate.analyses[[g]][["call"]]$centre)
    tab.aux <- t(t(tab.aux) / object$separate.analyses[[g]][["call"]]$ecart.type)
    tab.supp <- cbind(tab.supp,as.matrix(tab.aux))
	} else {
    tab.disj <- tab.disjonctif(object$separate.analyses[[g]][["call"]]$X)
    tab.disj.supp <- tab.disjonctif(rbind.data.frame(object$separate.analyses[[g]][["call"]]$X[1:2,],newdata[,(c(1,1+cumsum(object$call$group))[g]):cumsum(object$call$group)[g]])[-(1:2),,drop=FALSE])  ### pour que les donnees supplementaires aient les memes modalites
   if (!is.null(object$call$row.w.init)) SomRow <- sum(object$call$row.w.init)
   else SomRow <- length(object$call$row.w)
    M <- object$separate.analyses[[g]]$call$marge.col/SomRow
    # Z <- sweep(tab.disj/SomRow, 2, M*2, FUN = "-")
    # Zsup <- sweep(tab.disj.supp/SomRow, 2, M*2, FUN = "-")
    # Zsup <- sweep(Zsup, 2,apply(Z,2,ec,object$global.pca$call$row.w.init),FUN="/")
    Z <- t(t(tab.disj/SomRow)-M*2)
    Zsup <- t(t(tab.disj.supp/SomRow) - M*2)
    Zsup <- t(t(Zsup) / apply(Z,2,ec,object$global.pca$call$row.w.init))
    tab.supp <- cbind(as.matrix(tab.supp),Zsup)
  }
}
    tab.supp <- sweep(tab.supp,2,sqrt(object$call$col.w),FUN="*")
    coord <- crossprod(t(tab.supp),object$global.pca$svd$V*sqrt(object$call$col.w))
    dist2 <- rowSums(tab.supp^2)
    cos2 <- (coord)^2/dist2
    coord <- coord[, 1:ncp, drop = FALSE]
    cos2 <- cos2[, 1:ncp, drop = FALSE]
    colnames(coord) <- colnames(cos2) <- paste("Dim",  c(1:ncp), sep = ".")
    rownames(coord) <- rownames(cos2) <- names(dist2) <- rownames(newdata)
    result <- list(coord = coord, cos2 = cos2, dist = sqrt(dist2))
}

