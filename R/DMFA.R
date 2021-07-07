DMFA = function(don, num.fact = ncol(don), scale.unit=TRUE, ncp=5,quanti.sup=NULL,quali.sup=NULL, graph=TRUE, axes=c(1,2)){

  if (is.null(rownames(don))) rownames(don) = 1:nrow(don)
  if (is.null(colnames(don))) colnames(don) = paste("V",1:ncol(don),sep="")
  for (j in 1:ncol(don)) if (colnames(don)[j]=="") colnames(don)[j] = paste("V",j,sep="")
  if (!is.null(quali.sup) & !is.numeric(quali.sup)) quali.sup<- which(colnames(X)%in%quali.sup)
  if (!is.null(quanti.sup) & !is.numeric(quanti.sup)) quanti.sup<- which(colnames(X)%in%quanti.sup)
  for (j in 1:nrow(don)) if (is.null(rownames(don)[j])) rownames(don)[j] = paste("row",j,sep="")
  don <- as.data.frame(don)
  is.quali <- which(!unlist(lapply(don,is.numeric)))
  don[,is.quali] <- lapply(don[,is.quali,drop=FALSE],as.factor)
  don <- droplevels(don)
  don <- don[,c(num.fact,quali.sup,(1:ncol(don))[-c(num.fact,quali.sup,quanti.sup)],quanti.sup)]
  num.fact <- 1
  if (!is.null(quali.sup)) quali.sup = (2:(1+length(quali.sup)))
  don[,num.fact] = as.factor(don[,num.fact])
  lev <- levels(don[,num.fact])
  if (all(lev%in%(1:100000))) lev=paste("Gr",lev,sep="")
  levels(don[,num.fact]) = lev
  ng <- length(lev)
  if (!is.null(quanti.sup)){
    quanti.sup = ((ncol(don)-length(quanti.sup)+1):ncol(don))
    quanti.sup = quanti.sup + length(quali.sup) 
  }
  vars <- colnames(don[,-num.fact])
  n <- nrow(don)
  p <- ncol(don)-1
  group.means <- matrix(0, nrow = ng, ncol = p)
  Cov <- Xc <- FS <- ni <- structure(vector(mode = "list", length = ng), names = lev)

  for(i in 1:ng) {
    Xc[[i]] <- scale(don[don[,num.fact]==lev[i] , -c(num.fact,quali.sup)], scale=scale.unit)
    if (!scale.unit) Cov[[i]] <- cov(Xc[[i]])
    if (scale.unit) Cov[[i]] <- cor(Xc[[i]])
    ni[[i]] <- nrow(Xc[[i]])
    if (i ==1) X = Xc[[i]]
    else X = rbind.data.frame(X, Xc[[i]])
  }

  X <- cbind.data.frame(don[,num.fact,drop=FALSE],X)
  if (is.null(quali.sup)) res.pca <- PCA(X,quali.sup=1,graph=FALSE,ncp=ncp,quanti.sup=quanti.sup)
  else {
    X.quali <- don[,quali.sup]
    for (i in 1:length(quali.sup)) X.quali <- cbind.data.frame(X.quali,as.factor(paste(don[,quali.sup[i]],don[,num.fact],sep="")))
    X <- cbind.data.frame(don[,num.fact],X.quali,X[,-1])
    res.pca = PCA(X,quali.sup=1:(1+2*length(quali.sup)),graph=FALSE,ncp=ncp,quanti.sup=quanti.sup)
  }
### deb ajout
res.pca$ind$coord <- res.pca$ind$coord[rownames(don),]
res.pca$ind$contrib <- res.pca$ind$contrib[rownames(don),]
res.pca$ind$cos2 <- res.pca$ind$cos2[rownames(don),]
res.pca$ind$dist <- res.pca$ind$dist[rownames(don)]
### fin ajout

  ncp=ncol(res.pca$var$coord)
  V = res.pca$var$coord
  for (j in 1:ng) FS[[j]] = res.pca$ind$coord[don[,num.fact]==lev[j],]
## Ajout des variables partielles
  cor.partiel <- correl <- structure(vector(mode = "list", length = ng), names = lev)
  for (j in 1:ng){
    cor.partiel[[j]] = cor(Xc[[j]],FS[[j]])
    correl[[j]] = cor(FS[[j]])
  }
#### Graphe des groupes avec methode de Seb
  coord.gr = coord.gr2 = cos2.gr = matrix(0,ng,ncp)
  for (s in 1:ncp) {
    if (is.null(quanti.sup)) for (j in 1:ng) coord.gr[j,s] = sum(diag(V[,s]%*%t(V[,s])%*%Cov[[j]]))/res.pca$eig[s,1]
    else for (j in 1:ng) coord.gr[j,s] = sum(diag(V[,s]%*%t(V[,s])%*%Cov[[j]][1:(nrow(Cov[[j]])-length(quanti.sup)),1:(nrow(Cov[[j]])-length(quanti.sup))]))/res.pca$eig[s,1]
  }
  for (j in 1:ng){
    if (is.null(quanti.sup)){
      eigaux = eigen(Cov[[j]])
      coord.gr2[j,] = coord.gr[j,] / eigaux$values[1]
      cos2.gr[j,] = coord.gr[j,]^2 / sum(eigaux$values^2) *100
    }
    else {
      coord.gr2[j,] = coord.gr[j,] / eigen(Cov[[j]][1:(nrow(Cov[[j]])-length(quanti.sup)),1:(nrow(Cov[[j]])-length(quanti.sup))])$values[1]
      cos2.gr[j,] = coord.gr[j,]^2 / sum(eigen(Cov[[j]][1:(nrow(Cov[[j]])-length(quanti.sup)),1:(nrow(Cov[[j]])-length(quanti.sup))])$values^2) *100
    }
  }
  colnames(coord.gr) = colnames(coord.gr2) = colnames(cos2.gr) = colnames(res.pca$var$coord)
  rownames(coord.gr) = rownames(coord.gr2) = rownames(cos2.gr) = lev
#### Fin graphe des groupes avec methode de Seb
 
  res=list()
  if (nrow(res.pca$quali.sup$coord)>ng){ 
    res=res.pca
    res$quali.sup$coord=res$quali.sup$coord[-(1:ng),]
    res$quali.sup$cos2=res$quali.sup$cos2[-(1:ng),]
    res$quali.sup$v.test=res$quali.sup$v.test[-(1:ng),]
  }
  else   res=res.pca[names(res.pca)!=c("quali.sup")]
  res$var.partiel = cor.partiel
  res$cor.dim.gr = correl
  res$Xc = Xc
  res$group$coord = coord.gr
  res$group$coord.n = coord.gr2
  res$group$cos2 = cos2.gr
  res$Cov = Cov
  class(res) <- c("DMFA", "list")
  if (graph) {
    plot.DMFA(res, choix="ind",invisible="quali", label="none", axes=axes,new.plot=TRUE)
    plot.DMFA(res, choix="var", axes=axes,new.plot=TRUE)
    plot.DMFA(res, choix="group", axes=axes,new.plot=TRUE)
    if (!is.null(quali.sup)) plot.DMFA(res, choix="quali", axes=axes,new.plot=TRUE)
  }
  return(res)
}
