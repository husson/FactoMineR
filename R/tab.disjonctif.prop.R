tab.disjonctif.prop<-function (tab,seed=NULL,row.w=NULL) 
{
    if (!is.null(seed)){set.seed(seed)}
    moy.p <- function(V, poids) {
        res <- sum(V * poids,na.rm=TRUE)/sum(poids[!is.na(V)])
    }

	if (is.null(row.w)) row.w=rep(1/nrow(tab),nrow(tab))
    tab <- as.data.frame(tab)
    modalite.disjonctif <- function(i) {
        moda <- tab[, i]
        nom <- names(tab)[i]
        n <- length(moda)
        moda <- as.factor(moda)
        x <- matrix(0L, n, length(levels(moda)))
          ind<-(1:n) + n * (unclass(moda) - 1L)
          indNA<-which(is.na(ind))
                
        x[(1:n) + n * (unclass(moda) - 1)] <- 1L
        if (length(indNA)!=0){
          if (is.null(seed)) {
           x[indNA,]<- NA
           x[indNA,]<- matrix(rep(apply(x,2,moy.p,row.w),each=length(indNA)),nrow=length(indNA))
##           x[indNA,]<- matrix(rep(apply(x,2,sum)/sum(x),each=length(indNA)),nrow=length(indNA))
          } else {
           for (k in 1:length(indNA)) {
            aux <- runif(length(levels(moda)))
            x[indNA[k],]=aux/sum(aux)
           }
          }
         }
          if ((ncol(tab) != 1) & (levels(moda)[1] %in% c(1:nlevels(moda),"n", "N", "y", "Y"))) 
            dimnames(x) <- list(row.names(tab), paste(nom, levels(moda),sep = "."))
        else dimnames(x) <- list(row.names(tab), levels(moda))
        return(x)
    }
    if (ncol(tab) == 1) 
        res <- modalite.disjonctif(1)
    else {
        res <- lapply(1:ncol(tab), modalite.disjonctif)
        res <- as.matrix(data.frame(res, check.names = FALSE))
    }
    return(res)
}
