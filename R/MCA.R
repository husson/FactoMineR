MCA <- function (X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL, excl=NULL,
    graph = TRUE, level.ventil = 0, axes = c(1, 2), row.w = NULL, 
    method="Indicator",na.method="NA",tab.disj=NULL){
    
############
ventil.tab <- function (tab, level.ventil=0.05,row.w=NULL,ind.sup=NULL,quali.sup=NULL,quanti.sup=NULL) {
 if (is.null(row.w)) row.w <- rep(1,nrow(tab)-length(ind.sup))
 col.var <- 1:ncol(tab)
 if (!is.null(c(quali.sup,quanti.sup))) col.var <- col.var[-c(quali.sup,quanti.sup)]
 for (i in col.var) {
   if (is.factor(tab[,i])){
      tab[,i] <- ventilation(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
   }
   if (is.ordered(tab[,i])){
      tab[,i] <- ventilation.ordonnee(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
   }
 }
 return(tab)
}

ventilation <- function(Xqual,level.ventil=0.05,row.w=NULL,ind.sup=NULL) {
 if (!is.factor(Xqual)) stop("Xqual should be a factor \n")
 modalites <- levels(Xqual)
 if (length(modalites)<=1) stop("not enough levels \n")
 if (is.null(ind.sup)) {
  ind.act <- (1:length(Xqual))
 } else {ind.act <- (1:length(Xqual))[-ind.sup]}
 tabl <- table(Xqual[ind.act])
 if (!is.null(row.w)){
   for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual[ind.act]==levels(Xqual)[j])*row.w,na.rm=TRUE)
 }
 selecti <- (tabl/sum(tabl,na.rm=TRUE))< level.ventil
 if (sum(selecti)==length(modalites)) return(Xqual)

 if (!any(selecti)) return(Xqual) else {
  lesquels <- modalites[!selecti]
#  if (length(lesquels)==1) return(NULL) else {
  if (length(lesquels)==1) return(Xqual) else {
   prov <- factor(Xqual[(Xqual%in%lesquels)],levels=lesquels)
   prov <- table(prov)
   proba <- prov/sum(prov)

   for (j in modalites[selecti]) {
    Xqual[which(Xqual==j)] <- sample(lesquels,sum(Xqual==j,na.rm=TRUE), replace=TRUE,prob=proba)
   }
   Xqualvent <- factor(as.character(Xqual))
  }
 }
 return(Xqualvent)
}

ventilation.ordonnee <- function(Xqual,level.ventil=0.05,ind.sup=NULL,row.w=NULL) {
 if (!is.ordered(Xqual)) stop("Xqual must be ordered \n")
 mod <- levels(Xqual)
 if (length(mod)<=1) stop("not enough levels \n")
 if (is.null(ind.sup)) {
  ind.act <- (1:length(Xqual))
 } else {ind.act <- (1:length(Xqual))[-ind.sup]}
 tabl <- table(Xqual[ind.act])
 if (!is.null(row.w)){
   for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual[ind.act]==levels(Xqual)[j])*row.w,na.rm=TRUE)
 }
 selecti <- (tabl/sum(tabl))<level.ventil
 if (!any(selecti)) return(Xqual) else {
  numero <- which(selecti)
  while(any((tabl/sum(tabl))<level.ventil)) {
   j <- which(((tabl/sum(tabl))<level.ventil))[1]
   K <- length(mod)
   if (j<K) {
    if ((j>1)&(j<K-1)) levels(Xqual) <- c(mod[1:(j-1)],paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."),mod[j+2:K])
    if (j==1) levels(Xqual) <- c(paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."),mod[j+2:K])
    if (j==(K-1)) levels(Xqual) <- c(mod[1:(j-1)],paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."))
   } else {
      levels(Xqual) <- c(mod[1:(j-2)],paste(mod[j-1],mod[j],sep="."),paste(mod[j-1],mod[j],sep="."))
   }
  }
 }
# if (nlevels(Xqual)>1) return(Xqual)
# else return(NULL)
 return(Xqual)
}

  # fct.eta2 <- function(vec,x,weights) {
     # res <- summary(lm(x~vec,weights=weights))$r.squared
  # }
  

fct.eta2 <- function(vec,x,weights) {   
  VB <- function(xx) {
	return(sum((colSums((tt*xx)*weights)^2)/ni))
  }
  tt <- tab.disjonctif(vec)
  ni <- colSums(tt*weights)
  unlist(lapply(as.data.frame(x),VB))/colSums(x*x*weights)
}

  modif.rate <- function(resmca) {
    tab <- cbind.data.frame(resmca$eig,rep(0, nrow(resmca$eig)),rep(100, nrow(resmca$eig)))
	nbvar <- nrow(resmca$var$eta)
    nbeig <- resmca$eig[resmca$eig[,1]>=1/nbvar,1]
    pseudo <- (nbvar/(nbvar-1)*(nbeig-1/nbvar))^2
    tab[1:length(nbeig),4] <- round(pseudo/sum(pseudo)*100,2)
    tab[,5] <- cumsum(tab[,4])
    colnames(tab)[4:5] <- c("modified rates","cumulative modified rates")
    return(tab)
  }

#############
## Main program    
#############

  if (is.null(rownames(X))) rownames(X) <- 1:nrow(X)
  if (is.null(colnames(X))) colnames(X) <- colnames(X, do.NULL = FALSE,prefix="V")
  X <- as.data.frame(X)
  is.quali <- which(!unlist(lapply(X,is.numeric)))
  X[,is.quali] <- lapply(X[,is.quali,drop=FALSE],as.factor)

  X <- droplevels(X)
  ind.act <- setdiff(1:nrow(X),ind.sup)

  if (!is.null(which(lapply(X,class)=="logical"))){
    for (k in which(lapply(X,class)=="logical")) X[,k] <- as.factor(X[,k])
  }

  if (!is.null(quali.sup) & !is.numeric(quali.sup)) quali.sup<- which(colnames(X)%in%quali.sup)
  if (!is.null(quanti.sup) & !is.numeric(quanti.sup)) quanti.sup<- which(colnames(X)%in%quanti.sup)
  if (level.ventil > 0) X <- ventil.tab(X,level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup,quali.sup=quali.sup,quanti.sup=quanti.sup)
  niveau <- unlist(lapply(X,levels))
  if (sum(duplicated(niveau))>0){
    for (j in 1:ncol(X)) {
      if (sum(niveau %in% levels(X[, j])) != nlevels(X[, j])) levels(X[, j]) <- paste(attributes(X)$names[j], levels(X[, j]), sep = "_")
    }
  }

nonact <- c(quanti.sup,quali.sup)
if (!is.null(nonact)) act <- (1:ncol(X))[-nonact]
else act <- (1:ncol(X))
Z <- tab.disjonctif(X[, act,drop=FALSE])
## add for ecxl
aux <- colnames(Z)%in%excl
if (any(aux)) excl <- which(aux)

if (any(is.na(X[,act]))){
 if (is.null(tab.disj)){
  if (na.method=="Average"){
    newRowW <- rep(1e-08,nrow(X))
	newRowW[-ind.sup] <- row.w
    Z <- tab.disjonctif.prop(X[, act],row.w=newRowW)
  }
  if (na.method=="NA"){
    warnings('Missing values for one variable are considered as a new category; you can use method="Average" or use the imputeMCA function of the missMDA package')
    for (j in act) X[,j] <- as.factor(replace(as.character(X[,j]),is.na(X[,j]),paste(attributes(X)$names[j],".NA",sep="")))
    Z <- tab.disjonctif(X[, act])
  }
 } else{
   Vec <- rep("var",ncol(X))
   NLevels <- sapply(X,nlevels)
   if (!is.null(quali.sup)) Vec[quali.sup] <- "quali.sup"
   if (!is.null(quanti.sup)){
     Vec[quanti.sup] <- "quanti.sup"
	 NLevels[NLevels==0] <- 1
   }   
   TabDisjMod <- rep(Vec,NLevels)
   Z <- tab.disj[,which(TabDisjMod=="var")]
 }
}
Ztot <- Z

col.sup <- NULL
if (!is.null(quali.sup)){
     if (any(is.na(X[,quali.sup,drop=FALSE]))){
       for (j in quali.sup) X[,j] <- as.factor(replace(as.character(X[,j]),is.na(X[,j]),paste(attributes(X)$names[j],".NA",sep="")))
     }
     X[,quali.sup] <- ventil.tab(X[,quali.sup,drop=FALSE],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
     if (is.null(tab.disj)) Zqs <- tab.disjonctif(X[, quali.sup])
     else Zqs <- tab.disj[,which(TabDisjMod=="quali.sup")]
     Ztot <- cbind.data.frame(Z, Zqs)
     col.sup <- (ncol(Z) + 1):ncol(Ztot)
}
Xact <- X[,act]

if (!is.null(quanti.sup)){
     if (any(is.na(X[,quanti.sup,drop=FALSE]))){
       for (j in quanti.sup) X[,j] <- replace(X[,j],is.na(X[,j]), mean(X[,j], na.rm=TRUE))
     }
     if (is.null(tab.disj)) X.quanti.sup <- as.matrix(X[, quanti.sup])
	 else  X.quanti.sup <- tab.disj[,which(TabDisjMod=="quanti.sup"),drop=FALSE]
     if (!is.null(ind.sup)) X.quanti.sup <- X.quanti.sup[ind.act, ,drop=FALSE]
     colnames(X.quanti.sup) <- attributes(X)$names[quanti.sup]
}

    if (is.null(row.w)) row.w <- rep(1, nrow(X) - length(ind.sup))
    if (length(row.w) != nrow(X) - length(ind.sup)) stop("length of vector row.w should be the number of active rows")
    if (tolower(method)=="burt") {  ## boucle utile pour calculer la distance au cdg et pour calculer les cos2
      res.mca <- CA(Ztot, ncp = ncol(Z)-length(act), row.sup = ind.sup, col.sup = col.sup, graph = FALSE, row.w = row.w, excl=excl) 
      res.mca$col$coord <- t(t(res.mca$col$coord)*sqrt(res.mca$eig[1:ncol(res.mca$col$coord),1]))
      auxil <- rowSums(res.mca$col$coord^2)
      if (!is.null(col.sup)){ 
	    res.mca$col.sup$coord <- t(t(res.mca$col.sup$coord)*sqrt(res.mca$eig[1:ncol(res.mca$col.sup$coord),1]))
        auxil2 <- rowSums(res.mca$col.sup$coord^2)
	  }
    }
    res.mca <- CA(Ztot, ncp = min(ncp,ncol(Z)-length(act)), row.sup = ind.sup, excl=excl, col.sup = col.sup, graph = FALSE, row.w = row.w)
    if (is.null(ncol(res.mca$row$coord))) res.mca$row$coord <- matrix(res.mca$row$coord,ncol=1) 
    ncp <- ncol(res.mca$row$coord)
    res.mca$call$X <- X
    res.mca$call$ind.sup <- ind.sup
    res.mca$call$quali <- (1:ncol(X))
    if (!is.null(quali.sup) | !is.null(quanti.sup)) res.mca$call$quali <- res.mca$call$quali[-c(quali.sup, quanti.sup)]
    res.mca$call$quali.sup <- quali.sup
    res.mca$call$quanti.sup <- quanti.sup
    res.mca$call$row.w <- row.w
    if (!is.null(excl)) res.mca$call$excl <- excl
	res.mca$call$call <- match.call()
    if (length(act)>1) res.mca$eig <- res.mca$eig[1:min(length(ind.act)-1,sum(sapply(Xact,nlevels))-length(act)),,drop=FALSE]
    else res.mca$eig <- res.mca$eig[1:(nlevels(Xact)-1),,drop=FALSE]
    names(res.mca)[3] <- "ind"
    res.mca$ind <- res.mca$ind[1:3]
    names(res.mca$ind) <- c("coord", "contrib", "cos2")
    names(res.mca)[4] <- "var"
    if (tolower(method)=="burt"){
      res.mca$var$coord <- t(t(res.mca$var$coord)*sqrt(res.mca$eig[1:ncol(res.mca$var$coord),1]))
      res.mca$var$cos2 <- res.mca$var$coord^2/auxil
    }
    res.mca$var <- res.mca$var[1:3]
    names(res.mca$var) <- c("coord", "contrib", "cos2")
    indice <- 6
    if (!is.null(ind.sup)) {
        names(res.mca)[indice] <- "ind.sup"
        names(res.mca$ind.sup) <- c("coord", "cos2")
        indice <- indice + 1
        Xact <- X[ind.act,act ,drop=FALSE]
    }
    if (!is.null(quali.sup)) {
        names(res.mca)[indice] <- "quali.sup"
        names(res.mca$quali.sup) <- c("coord", "cos2")
        if (tolower(method)=="burt"){
          res.mca$quali.sup$coord <- t(t(res.mca$quali.sup$coord)*sqrt(res.mca$eig[1:ncol(res.mca$quali.sup$coord),1]))
          res.mca$quali.sup$cos2 <- res.mca$quali.sup$coord^2/auxil2
        }
    }

    if (!is.null(ind.sup)) Z <- Z[ind.act, ]
    Nj <- colSums(Z * row.w)
    N <- sum(Nj)/(ncol(X) - length(quali.sup) - length(quanti.sup))
    if (N>1) coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
	else coef <- sqrt(Nj)
    res.mca$var$v.test <- as.matrix(res.mca$var$coord*coef)

	# if (ncp>1) eta2 <- t(sapply(Xact,fct.eta2,res.mca$ind$coord,weights=row.w))
	# else {
	  # eta2 <- as.matrix(sapply(Xact,fct.eta2,res.mca$ind$coord,weights=row.w),ncol=ncp)
      # colnames(eta2) = paste("Dim", 1:ncp)
      # rownames(eta2) = colnames(Xact)
	# }

    variable <- rep(attributes(Xact)$names,unlist(lapply(Xact,nlevels)))
    if (length(act)>1){
      CTR <- aggregate(res.mca$var$contrib/100,by=list(factor(variable)),FUN=sum)
      rownames(CTR) <- CTR[,1]
      CTR <- t(t(CTR[,-1,drop=FALSE])*res.mca$eig[1:ncp,1])*ncol(Xact)
      eta2 <- CTR[attributes(Xact)$names,,drop=FALSE]
      res.mca$var$eta2 <- eta2
    } else {
	  eta2 <- matrix(1,1,ncp)
	  colnames(eta2) <- paste("Dim",1:ncp)
	  rownames(eta2) <- colnames(X[,act,drop=FALSE])
      res.mca$var$eta2 <- eta2
	}
	
    if (!is.null(quali.sup)) {
        if (!is.null(ind.sup)) Zqs <- Zqs[ind.act, ]
        Nj <- colSums(Zqs * row.w)
        if (N>1) coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
		else coef <- sqrt(Nj)
        res.mca$quali.sup$v.test <- as.matrix(res.mca$quali.sup$coord*coef)

        eta2 <- matrix(NA, length(quali.sup), ncp)
        colnames(eta2) <- paste("Dim", 1:ncp)
        rownames(eta2) <- attributes(X)$names[quali.sup]
		 if (ncp>1) eta2 <- t(sapply(as.data.frame(X[rownames(Xact), quali.sup,drop=FALSE]),fct.eta2,res.mca$ind$coord,weights=row.w))
		 else {
		   eta2 <- as.matrix(sapply(as.data.frame(X[rownames(Xact), quali.sup,drop=FALSE]),fct.eta2,res.mca$ind$coord,weights=row.w),ncol=ncp)
		 }
		
        res.mca$quali.sup$eta2 <- eta2
    }
    if (!is.null(quanti.sup)) {
        U <- res.mca$svd$U
        coord.quanti.sup <- matrix(NA, ncol(X.quanti.sup), ncp)
        coord.quanti.sup <- cov.wt(cbind.data.frame(U,X.quanti.sup),cor=TRUE,wt=row.w,method="ML")$cor[-(1:ncol(U)),1:ncol(U),drop=FALSE]
#		coord.quanti.sup <- cor(X.quanti.sup,U,method="pearson")
        dimnames(coord.quanti.sup) <- list(colnames(X.quanti.sup), paste("Dim", 1:ncp))
        res.mca$quanti.sup$coord <- coord.quanti.sup
    }

    if (tolower(method)=="burt"){
      res.mca$eig[,1] <- res.mca$eig[,1]^2
      res.mca$eig[,2] <- res.mca$eig[,1]/sum(res.mca$eig[,1]) * 100
      res.mca$eig[,3] <- cumsum(res.mca$eig[,2])      
    }

	if (!is.null(excl)){
	  res.mca$excl$coord <- res.mca$var$coord[excl,]
      res.mca$excl$cos2 <- res.mca$var$cos2[excl,]
      res.mca$excl$v.test <- res.mca$var$v.test[excl,]
	  res.mca$var$coord <- res.mca$var$coord[-excl,]
      res.mca$var$contrib <- res.mca$var$contrib[-excl,]
      res.mca$var$cos2 <- res.mca$var$cos2[-excl,]
      res.mca$var$v.test <- res.mca$var$v.test[-excl,]
      res.mca$eig <- modif.rate(res.mca)
    }
	
    class(res.mca) <- c("MCA", "list")
    if (graph & (ncp>1)) {
        print(plot.MCA(res.mca, choix = "ind", invisible="ind", axes = axes,new.plot=TRUE))
        if (method=="Indicator") print(plot.MCA(res.mca, choix = "ind", invisible=c("var","quali.sup","quanti.sup"), axes = axes,new.plot=TRUE,cex=0.8))
		print(plot.MCA(res.mca, choix = "var", axes = axes,new.plot=TRUE))
        if (!is.null(quanti.sup)) print(plot.MCA(res.mca, choix = "quanti.sup", axes = axes,new.plot=TRUE))
    }
    return(res.mca)
}
