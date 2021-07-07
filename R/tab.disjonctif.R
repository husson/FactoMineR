tab.disjonctif <- function (tab){
#     tab<-as.data.frame(tab)
#     #fonction interne permettant la realisation d'un TDC pour un unique facteur
#     modalite.disjonctif <- function(i){
#         moda <- as.factor(tab[, i])
#         n <- length(moda)
#         x <- matrix(0L, n, nlevels(moda))
#         x[(1:n) + n * (unclass(moda) - 1L)] <- 1L
#         return(x)
#     }
#     # fin fonction interne
# 
#     if (ncol(tab)==1) {
# 	  res <- modalite.disjonctif(1)
# 	  dimnames(res) <- list(attributes(tab)$row.names, levels(tab[,1]))
# 	}
#     else {
# 	  variable <- rep(attributes(tab)$names,lapply(tab,nlevels))
# 	  listModa <- unlist(lapply(tab,levels))
# 	  wlistModa <- which((listModa)%in%c("y","n","Y","N"))
#       if (!is.null(wlistModa)) listModa[wlistModa] <- paste(variable[wlistModa],listModa[wlistModa],sep = ".")
#       numlistModa <- which(unlist(lapply(listModa,is.numeric)))
#       if (!is.null(numlistModa)) listModa[numlistModa] <- paste(variable[numlistModa],listModa[numlistModa],sep = ".")
#       res <- lapply(1:ncol(tab), modalite.disjonctif)
#       res <- as.matrix(data.frame(res, check.names = FALSE))
# 	  dimnames(res) <- list(attributes(tab)$row.names,listModa)
# 	}
#  return(res)
    tab <- as.data.frame(tab)
	isQuali <- which(!sapply(tab,is.numeric))
	if (length(isQuali)==0) return(tab)
    if (ncol(tab)==1) {
      if (is.numeric(tab[,1])){
  	    return(tab)
	  } else { 
	    tabdisj <- .Call("disjoVar", as.integer(data.matrix(tab)),
                     as.integer(c(nrow(tab),nlevels(tab[,1]))))
	    rownames(tabdisj) <- rownames(tab)
	    colnames(tabdisj) <- levels(tab[,1])
	  }
	 } else {
	  isQuanti <- which(sapply(tab,is.numeric))
	  if (length(isQuanti)==0){
        tabdisj <- .Call("disjoMat", as.integer(data.matrix(tab)),
                     as.integer(dim(tab)),
                     as.integer(unlist(lapply(tab, nlevels))))
	  } else {
        nomMod <- lapply(tab, levels)
	    nomMod[isQuanti]=names(nomMod[isQuanti]) 
        tabdisj <- .Call("disjoMat", as.integer(data.matrix(tab[,isQuali,drop=FALSE])),
                     as.integer(dim(tab[,isQuali,drop=FALSE])),
                     as.integer(unlist(lapply(tab[,isQuali,drop=FALSE], nlevels))))
	  }
 	  listModa <- unlist(lapply(tab,levels))
 	  wlistModa <- which((listModa)%in%c("y","n","Y","N"))
      if (!is.null(wlistModa)){
 	    variable <- rep(attributes(tab)$names,lapply(tab,nlevels))
 	    listModa[wlistModa] <- paste(variable[wlistModa],listModa[wlistModa],sep = ".")
	  }
      # tabdisj <- as.matrix(data.frame(tabdisj, check.names = FALSE))
	  dimnames(tabdisj) <- list(attributes(tab)$row.names,listModa)
	  if (length(isQuanti)!=0){
	    tabdisj <- cbind(tabdisj,tab[,isQuanti,drop=FALSE])
        Vec <- rep(1,ncol(tab))
        NLevels <- unlist(pmax(lapply(tab,nlevels),1))  # pmax useful for quanti variables
        Vec[isQuanti] <- 0 # for quanti vari
        TabDisjMod <- rep(Vec,NLevels)
	    ordre <- c(which(TabDisjMod!=0),which(TabDisjMod==0))
	    tabdisj <- tabdisj[,order(ordre)]	
	  }
	}
    return(tabdisj)
}
