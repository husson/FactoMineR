reconst <- function (res, ncp=NULL){

if (inherits(res, "CA")) {
  if (is.null(ncp)) ncp <- ncol(res$row$coord)
  ncp <- min(ncp,ncol(res$row$coord))
  X=res$call$X
  P <- as.matrix(X/sum(X))
  Rc <- apply(P, 2, sum)
  Rr <- apply(P, 1, sum)
  if (ncp>0) {
    U <- sweep(sweep(res$row$coord[,1:ncp,drop=FALSE],1,sqrt(Rr),FUN="*"), 2, sqrt(res$eig[1:ncp,1]), FUN = "/")
    V <- sweep(sweep(res$col$coord[,1:ncp,drop=FALSE],1,sqrt(Rc),FUN="*"), 2, sqrt(res$eig[1:ncp,1]), FUN = "/")
    if (ncp>1) { S=sweep(U,2,sqrt(res$eig[1:ncp,1]),FUN="*")%*%t(V)
    } else S=(U*sqrt(res$eig[1:ncp,1]))%*%t(V)
    hatX <- sum(X)*(sweep(sweep(S,1,sqrt(Rr),FUN="*"),2,sqrt(Rc),FUN="*") + Rr%*%t(Rc))
  } else hatX <- Rr%*%t(Rc)
  return(hatX)
} else{
  if (is.null(ncp)) ncp <- ncol(res$ind$coord)
  ncp <- min(ncp,ncol(res$ind$coord))
  if (inherits(res, "MFA")) coord.var = sweep(as.matrix(res$quanti.var$coord)[,1:ncp,drop=F],1,res$call$col.w,FUN="*")
  if (inherits(res, "PCA")) coord.var = as.matrix(res$var$coord)[,1:ncp,drop=F]
  coord.ind = as.matrix(res$ind$coord)[,1:ncp,drop=F]
  hatX = coord.ind%*%t(sweep(coord.var,2,sqrt(res$eig[1:ncp,1]),FUN="/"))
if (inherits(res, "PCA")) {
  hatX = sweep(hatX,2,res$call$ecart.type,FUN="*")
  hatX = sweep(hatX,2,res$call$centre,FUN="+")
}
if (inherits(res, "MFA")) {
  ecarttype=res$separate.analyses[[1]][["call"]][["ecart.type"]]
  for (g in 2:length(res$call$group)) ecarttype=c(ecarttype,res$separate.analyses[[g]][["call"]][["ecart.type"]])
  moy=res$separate.analyses[[1]][["call"]][["centre"]]
  for (g in 2:length(res$call$group)) moy=c(moy,res$separate.analyses[[g]][["call"]][["centre"]])
  hatX = sweep(hatX,2,ecarttype,FUN="*")
  hatX = sweep(hatX,2,res$call$col.w,FUN="/")
  hatX = sweep(hatX,2,moy,FUN="+")
  }
  return(hatX)
  }
}

