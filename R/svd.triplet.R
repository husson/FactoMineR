svd.triplet <- function (X, row.w = NULL, col.w = NULL,ncp=Inf) {


tryCatch.W.E <- function(expr){  ## function proposed by Maechler
    W <- NULL
    w.handler <- function(w){ # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                     warning = w.handler),
         warning = W)
}


   if (is.null(row.w)) row.w <- rep(1/nrow(X), nrow(X))
   if (is.null(col.w)) col.w <- rep(1, ncol(X))
   ncp <- min(ncp,nrow(X)-1,ncol(X))
   row.w <- row.w / sum(row.w)
    X <- t(t(X)*sqrt(col.w))*sqrt(row.w)
if (ncol(X)<nrow(X)){
##    svd.usuelle <- svd(X,nu=ncp,nv=ncp)
## lignes suivantes pour eviter qq pb de convergence de l'algo LINPACK de svd
	svd.usuelle <- tryCatch.W.E(svd(X,nu=ncp,nv=ncp))$val
    if (names(svd.usuelle)[[1]]=="message"){
	  svd.usuelle <- tryCatch.W.E(svd(t(X),nu=ncp,nv=ncp))$val
	  if (names(svd.usuelle)[[1]]=="d"){
	    aux <- svd.usuelle$u
		svd.usuelle$u <- svd.usuelle$v
		svd.usuelle$v <- aux
      } else{
          bb <- eigen(crossprod(X,X),symmetric=TRUE)
 		  svd.usuelle <- vector(mode = "list", length = 3)
          svd.usuelle$d[svd.usuelle$d<0]<-0
	      svd.usuelle$d <- sqrt(svd.usuelle$d)
          svd.usuelle$v <- bb$vec[,1:ncp]
#          svd.usuelle$u <- sweep(X%*%svd.usuelle$v,2,svd.usuelle$d[1:ncp],FUN="/")
          svd.usuelle$u <- t(t(crossprod(t(X),svd.usuelle$v))/svd.usuelle$d[1:ncp])
		  }
    }
    U <- svd.usuelle$u
    V <- svd.usuelle$v
    if (ncp >1){
	  mult <- sign(as.vector(crossprod(rep(1,nrow(V)),as.matrix(V))))
	  mult[mult==0] <- 1
      U <- t(t(U)*mult)
      V <- t(t(V)*mult)
    }
    U <- U/sqrt(row.w)
    V <- V/sqrt(col.w)
}
else{
	svd.usuelle <- tryCatch.W.E(svd(t(X),nu=ncp,nv=ncp))$val
    if (names(svd.usuelle)[[1]]=="message"){
	  svd.usuelle <- tryCatch.W.E(svd(X,nu=ncp,nv=ncp))$val
	  if (names(svd.usuelle)[[1]]=="d"){
	    aux <- svd.usuelle$u
		svd.usuelle$u <- svd.usuelle$v
		svd.usuelle$v <- aux
      } else{
          bb <- eigen(crossprod(t(X),t(X)),symmetric=TRUE)
		  svd.usuelle <- vector(mode = "list", length = 3)
          svd.usuelle$d[svd.usuelle$d<0]<-0
	      svd.usuelle$d <- sqrt(svd.usuelle$d)
          svd.usuelle$v <- bb$vec[,1:ncp]
          svd.usuelle$u <- t(t(crossprod(X,svd.usuelle$v))/svd.usuelle$d[1:ncp])
	  }
    }
    U <-  svd.usuelle$v
    V <- svd.usuelle$u
    mult <- sign(as.vector(crossprod(rep(1,nrow(V)),as.matrix(V))))
	mult[mult==0] <- 1
    V <- t(t(V)*mult)/sqrt(col.w)
    U <- t(t(U)*mult)/sqrt(row.w)
}
    vs <- svd.usuelle$d[1:min(ncol(X),nrow(X)-1)]
	num <- which(vs[1:ncp]<1e-15)
    if (length(num)==1){
	  U[,num] <- U[,num,drop=FALSE]*vs[num]
      V[,num] <- V[,num,drop=FALSE]*vs[num]
	} 
    if (length(num)>1){
	  U[,num] <- t(t(U[,num])*vs[num])
      V[,num] <- t(t(V[,num])*vs[num])
	}
    res <- list(vs = vs, U = U, V = V)
    return(res)
}
