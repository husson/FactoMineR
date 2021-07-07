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
          x[indNA,]<- matrix(rep(apply(x,2,sum)/sum(x),each=length(indNA)),nrow=length(indNA))
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

	# isQuali <- which(!sapply(tab,is.numeric))
	# if (length(isQuali)==0) return(tab)
    # if (ncol(tab)==1) {
      # if (is.numeric(tab[,1])){
  	    # return(tab)
	  # } else { 
	    # tabdisj <- .Call("disjoVarProp", as.integer(data.matrix(tab)),
                     # as.integer(c(nrow(tab),nlevels(tab[,1]))))

        ## Pb sometimes NaN with disjoVarProp ou disjoMatProp
	    # if (any(is.nan(tabdisj))){
	      # tabdisj <- .Call("disjoVarProp", as.integer(data.matrix(tab)),
                     # as.integer(c(nrow(tab),nlevels(tab[,1]))))
	    # }
        ## End : Pb sometimes NaN with disjoVarProp ou disjoMatProp


	    # rownames(tabdisj) <- rownames(tab)
	    # colnames(tabdisj) <- levels(tab[,1])
	  # }
	 # } else {
	  # isQuanti <- which(sapply(tab,is.numeric))
	  # if (length(isQuanti)==0){
        # tabdisj <- .Call("disjoMatProp", as.integer(data.matrix(tab)),
                     # as.integer(dim(tab)),
                     # as.integer(unlist(lapply(tab, nlevels))))
        ## Pb sometimes NaN with disjoVarProp ou disjoMatProp
	    # if (any(is.nan(tabdisj))){
          # tabdisj <- .Call("disjoMatProp", as.integer(data.matrix(tab)),
                     # as.integer(dim(tab)),
                     # as.integer(unlist(lapply(tab, nlevels))))
	    # }
        ## End : Pb sometimes NaN with disjoVarProp ou disjoMatProp
	  # } else {
        # nomMod <- lapply(tab, levels)
	    # nomMod[isQuanti]=names(nomMod[isQuanti]) 
        # tabdisj <- .Call("disjoMatProp", as.integer(data.matrix(tab[,isQuali,drop=FALSE])),
                     # as.integer(dim(tab[,isQuali,drop=FALSE])),
                     # as.integer(unlist(lapply(tab[,isQuali,drop=FALSE], nlevels))))
        ## Pb sometimes NaN with disjoVarProp ou disjoMatProp
	    # if (any(is.nan(tabdisj))){
          # tabdisj <- .Call("disjoMatProp", as.integer(data.matrix(tab[,isQuali,drop=FALSE])),
                     # as.integer(dim(tab[,isQuali,drop=FALSE])),
                     # as.integer(unlist(lapply(tab[,isQuali,drop=FALSE], nlevels))))
	    # }
        ## End : Pb sometimes NaN with disjoVarProp ou disjoMatProp
	  # }
 	  # listModa <- unlist(lapply(tab,levels))
 	  # wlistModa <- which((listModa)%in%c("y","n","Y","N"))
      # if (!is.null(wlistModa)){
 	    # variable <- rep(attributes(tab)$names,lapply(tab,nlevels))
 	    # listModa[wlistModa] <- paste(variable[wlistModa],listModa[wlistModa],sep = ".")
	  # }
      ## tabdisj <- as.matrix(data.frame(tabdisj, check.names = FALSE))
	  # dimnames(tabdisj) <- list(attributes(tab)$row.names,listModa)
	  # if (length(isQuanti)!=0){
	    # tabdisj <- cbind(tabdisj,tab[,isQuanti,drop=FALSE])
        # Vec <- rep(1,ncol(tab))
        # NLevels <- unlist(pmax(lapply(tab,nlevels),1))  # pmax useful for quanti variables
        # Vec[isQuanti] <- 0 # for quanti vari
        # TabDisjMod <- rep(Vec,NLevels)
	    # ordre <- c(which(TabDisjMod!=0),which(TabDisjMod==0))
	    # tabdisj <- tabdisj[,order(ordre)]	
	  # }
	# }
    # return(tabdisj)
}
