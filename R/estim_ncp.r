estim_ncp <- function(X, ncp.min=0,ncp.max=NULL, scale=TRUE,method="GCV"){

## Pas de NA dans X
method <- tolower(method)

### For categorical variables
pquali <- 0
if (!is.numeric(X[,1])){
    pquali <- ncol(X)
    tab.disj <- tab.disjonctif(X)
    X <- scale(tab.disj)*sqrt(nrow(X)/(nrow(X)-1))/sqrt(ncol(X))
    ponder <- 1-apply(tab.disj/nrow(X), 2, sum)
    X <- sweep(X,2,sqrt(ponder),FUN="*")
    scale = FALSE
	if (method=="smooth") warning("You should use the GCV criterion in the argument method\n")
}

p=ncol(X)
n=nrow(X)
if (is.null(ncp.max)) ncp.max <- ncol(X)-pquali-1
ncp.max <- min(nrow(X)-2,ncol(X)-1,ncp.max)
crit <- NULL


X=scale(X,scale=FALSE)

if (scale){
 et = apply(X,2,sd)
 X = sweep(X,2,et,FUN="/")
}
##if (ncp.min==0)  crit = mean(X^2, na.rm = TRUE)*n/(n-1)
##if (ncp.min==0)  crit = mean(X^2, na.rm = TRUE)*(n*(p-pquali))/(p*(n-1))
if (ncp.min==0)  crit = mean(X^2, na.rm = TRUE)*(n*p)/((p-pquali)*(n-1))

rr = svd(X,nu=ncp.max,nv=ncp.max)

for (q in max(ncp.min,1):ncp.max){
    if (q==max(ncp.min,1)) {
	  if (q==1) rec = tcrossprod(as.matrix(rr$u)[,1,drop=FALSE]*rr$d[1],as.matrix(rr$v)[,1,drop=FALSE])
	  else rec = tcrossprod(sweep(as.matrix(rr$u)[,1:q,drop=F],2,rr$d[1:q],FUN="*"),as.matrix(rr$v)[,1:q,drop=F])
	}
    else rec = rec + tcrossprod(sweep(as.matrix(rr$u)[,q,drop=F],2,rr$d[q],FUN="*"),as.matrix(rr$v)[,q,drop=F])

##if (scale) rec = sweep(rec,2,et,FUN="*")

    if (method=="smooth"){
      if (q>1){
        a <- apply(rr$u[,1:q]^2,1,sum)
        b <- apply(rr$v[,1:q]^2,1,sum)
      } else {
        a=rr$u[,1]^2
        b=rr$v[,1]^2
      }
##      zz=sweep(rec-X,1,1-a,FUN="/")
      zz=sweep(rec-X,1,1-1/n-a,FUN="/")
##      sol = sweep(zz,2,1-b,FUN="/")
      sol = sweep(zz[,(1-b)>1e-10,drop=FALSE],2,(1-b)[(1-b)>1e-10],FUN="/")
      crit=c(crit,mean(sol^2))
    }    
##    if (method=="gcv") crit=c(crit,mean(( (n*(p-pquali))*(X-rec)/ (n*(p-pquali)- q*(n+p-pquali-q)))^2,na.rm=T))
##    if (method=="gcv") crit=c(crit,mean(( (n*(p-pquali))*(X-rec)/ ((n-1)*(p-pquali)- q*(n+p-pquali-q-1)))^2,na.rm=T))
##avec Julie    if (method=="gcv") crit=c(crit,mean(( n*(p-pquali)*(X-rec)/ ((n-1)*p- q*(n+p-pquali-q-1)))^2,na.rm=T))
    if (method=="gcv") crit=c(crit,mean(( n*p*(X-rec)/ ((n-1)*(p-pquali)- q*(n+p-pquali-q-1)))^2,na.rm=T))
  }
  if (any(diff(crit)>0)) { ncp = which(diff(crit)>0)[1]
  } else ncp <- which.min(crit)
#  return(list(ncp = which.min(crit)+ncp.min-1,criterion=crit))
  return(list(ncp = ncp+ncp.min-1,criterion=crit))
}
