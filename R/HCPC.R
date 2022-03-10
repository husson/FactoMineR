HCPC <- function (res, nb.clust = 0, consol = TRUE, iter.max = 10, min = 3, 
    max = NULL, metric = "euclidean", method = "ward", order = TRUE, 
    graph.scale = "inertia", nb.par = 5, graph = TRUE, proba = 0.05,cluster.CA="rows",
    kk=Inf,description=TRUE,...) 
{
    auto.cut.tree <- function(res, min, max, metric, method, weight=NULL,cla=NULL,...) {
        if (order) {
		    if (is.null(res$call$row.w)) res$call$row.w <- rep(1/nrow(res$ind$coord),nrow(res$ind$coord))
            if (is.null(res$call$row.w.init)) res$call$row.w.init <- res$call$row.w
            sss <- cbind.data.frame(res$ind$coord, res$call$X, res$call$row.w, res$call$row.w.init)
			if (!is.null(weight)) weight <- weight[order(sss[, 1], decreasing = FALSE)]
            sss <- sss[order(sss[, 1], decreasing = FALSE), ]
            res$ind$coord <- sss[, 1:ncol(res$ind$coord),drop=FALSE]
            res$call$X <- sss[, (ncol(res$ind$coord) + 1):(ncol(sss)-2)]
            res$call$row.w <- sss[,ncol(sss)-1]
            res$call$row.w.init <- sss[,ncol(sss)]
        }
        X <- as.data.frame(res$ind$coord)	

#		if("flashClust"%in%rownames(installed.packages())) require(flashClust,quiet=TRUE)
        do <- dist(X,method=metric)^2
        if (is.null(weight)) weight <- rep(1,nrow(X))
        eff <- outer(weight,weight,FUN=function(x,y,n) {x*y/n/(x+y)},n=sum(weight))
        dissi <- do*eff[lower.tri(eff)]
        hc <- flashClust::hclust(dissi, method = method, members = weight)
		inert.gain <- rev(hc$height)
		if (!is.null(cla)) inert.gain <- c(inert.gain,cla$tot.withinss/sum(cla$size))
		intra <- rev(cumsum(rev(inert.gain)))
        quot <- intra[min:(max)]/intra[(min - 1):(max - 1)] 
		nb.clust <- which.min(quot) + min -1
# changement dans calcul annule. Mis dans la version 1.34  2016/04/12 (2 lignes changees)
#        quot = inert.gain[(min-1):(max-1)]/inert.gain[min:max] 
#		nb.clust = which.max(quot) + min - 1
        return(list(res = res, tree = hc, nb.clust = nb.clust, 
            within = intra, inert.gain = inert.gain, quot = quot))
    }
    consolidation <- function(X, clust, iter.max = 10, ...) {
        centers <- NULL
        centers <- by(X, clust, colMeans)
        centers <- matrix(unlist(centers), ncol = ncol(X), byrow = TRUE)
        km <- kmeans(X, centers = centers, iter.max = iter.max, ...)
        return(km)
    }
    select <- function(Y, default.size, method, coord.centers) {
        clust <- Y[1, ncol(Y)]
        Y <- Y[, -ncol(Y),drop=FALSE]
        Z <- rbind(Y, coord.centers)
        if (nrow(Y) == 1) {
            distance <- data.frame(0, row.names = "")
            colnames(distance) <- rownames(Z[1, ])
        }
        else {
            distance <- as.matrix(dist(Z, method = method))
            distance <- distance[(nrow(Y) + 1):nrow(distance), 
                -((nrow(Y) + 1):ncol(distance))]
            distance <- sort(distance[clust, ], decreasing = FALSE)
        }
        if (length(distance) > default.size) 
            distance <- distance[1:default.size]
        else distance <- distance
    }
    distinctivness <- function(Y, default.size, method, coord.centers) {
        clust <- as.numeric(Y[1, ncol(Y)])
        Y <- Y[, -ncol(Y),drop=FALSE]
        Z <- rbind(Y, coord.centers)
        if (nrow(Y) == 1) {
            distance <- as.matrix(dist(Z, method = method))
            ind.car <- vector(length = 1, mode = "numeric")
            ind.car <- min(distance[-c(1, (clust + 1)), 1])
            names(ind.car) <- rownames(Z[1, ])
        }
        else {
            distance <- as.matrix(dist(Z, method = method))
            distance <- distance[(nrow(Y) + 1):nrow(distance),-((nrow(Y) + 1):ncol(distance))]
            if (nrow(distance) == 2) center.min <- distance[-clust, ]
            else center.min <- apply(distance[-clust, ], 2, min)
            ind.car <- sort(center.min, decreasing = TRUE)
        }
        if (length(ind.car) > default.size) ind.car <- ind.car[1:default.size]
        else ind.car <- ind.car
    }
#### Main program
#   if((method=="ward")&(!("flashClust"%in%rownames(installed.packages())))) method="ward.D" ### use of ward.D because I transform the distance to have the results obtained by ward.D2 
   res.sauv <- res	
   if ((kk!=Inf)&(consol==TRUE)){
     warning("No consolidation has been done after the hierarchical clustering since kk is different from Inf (see help for more details)")
     consol <- FALSE
   }
   if (is.vector(res)) {
        res <- cbind.data.frame(res, res)
        res <- PCA(res, scale.unit = FALSE, ncp = Inf, graph = FALSE)
        vec <- TRUE
    } else vec <- FALSE
#    if(inherits(res,"CA")){
#	  if(cluster.CA=="rows") res=as.data.frame(res$row$coord)
#	  if(cluster.CA=="columns") res=as.data.frame(res$col$coord)
#    }
    if (is.matrix(res)) res <- as.data.frame(res)
    cla <- NULL

    if (inherits(res, "PCA") | inherits(res, "MCA") | inherits(res,"MFA") | inherits(res, "HMFA") | inherits(res, "FAMD")) {
	  if (kk<nrow(res$ind$coord)){
	    res <- as.data.frame(res$ind$coord)
		kk <- min(kk, nrow(unique(res$ind$coord)))
      }
    }
    if (inherits(res, "CA")) {
	  if (cluster.CA=="rows"){
	    if (kk<nrow(res$row$coord)) {
		  res <- as.data.frame(sweep(res$row$coord,2,sqrt(res$eig[1:ncol(res$row$coord),1]),FUN="*"))
		  kk <- min(kk, nrow(unique(res$row$coord)))
        }
	  } else {
	    if (kk<nrow(res$col$coord)){
		  res <- as.data.frame(sweep(res$col$coord,2,sqrt(res$eig[1:ncol(res$col$coord),1]),FUN="*"))
  		  kk <- min(kk, nrow(unique(res$col$coord)))
        }
	  }
    }
    if (is.data.frame(res)){
	  res <-  res[,unlist(lapply(res,is.numeric)),drop=FALSE]
### AJOUT K-means
      if (kk<nrow(res)) kk <- min(kk,nrow(unique(res)))
	  if (kk <= nrow(res)){
	    cla <- kmeans(res, centers=kk, iter.max = 100, nstart = 4)
        res <- PCA(cla$centers, row.w=cla$size, scale.unit = FALSE, ncp = Inf, graph = FALSE)
      } else {
	    res <- PCA(res, scale.unit = FALSE, ncp = Inf, graph = FALSE)
	  }
### Fin AJOUT K-means
##      res <- PCA(res, scale.unit = FALSE, ncp = Inf, graph = FALSE)
    }

    if(inherits(res,"CA")){
	  aux <- res$eig
	  if(cluster.CA=="rows") res <- PCA(res$row$coord, scale.unit = FALSE, ncp = Inf, graph = FALSE,row.w=res$call$marge.row*sum(res$call$X))
	  if(cluster.CA=="columns") res <- PCA(res$col$coord, scale.unit = FALSE, ncp = Inf, graph = FALSE,row.w=res$call$marge.col*sum(res$call$X))
	  res$eig <- aux
    }
    if (is.null(max)) max <- min(10, round(nrow(res$ind$coord)/2))
    max <- min(max, nrow(res$ind$coord) - 1)
    if (inherits(res, "PCA") | inherits(res, "MCA") | inherits(res,"MFA") | inherits(res, "HMFA") | inherits(res, "FAMD")) {
    	if (!is.null(res$call$ind.sup)) res$call$X <- res$call$X[-res$call$ind.sup, ]
        if (is.null(res$call$row.w.init)) res$call$row.w.init <- res$call$row.w
        t <- auto.cut.tree(res, min = min, max = max, metric = metric, method = method, weight = res$call$row.w.init,cla=cla,order=order,...)
    }
    else stop("res should be from data.frame, PCA, CA, MCA, FAMD, MFA, or HMFA class")
    if (inherits(t$tree, "agnes")) t$tree <- as.hclust(t$tree)
    if (inherits(t$tree, "hclust")) {
        if (graph.scale == "inertia") {
            nb.ind <- nrow(t$res$ind$coord)
            inertia.height <- rep(0, nb.ind - 1)
            for (i in 1:(nb.ind - 1)) inertia.height[i] <- t$inert.gain[(nb.ind - i)]
            inertia.height <- sort(inertia.height, decreasing = FALSE) 
			t$tree$height <- inertia.height
        }
        auto.haut <- ((t$tree$height[length(t$tree$height) - t$nb.clust + 
            2]) + (t$tree$height[length(t$tree$height) - t$nb.clust + 1]))/2
        if (graph) {
            if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
            old.mar <- par()$mar
            par(mar = c(0.5, 2, 0.75, 0))
            lay <- matrix(ncol = 5, nrow = 5, c(2, 4, 4, 4, 4, 
                2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 
                1, 3, 3, 3, 3))
            layout(lay, respect = TRUE)
            barplot(t$inert.gain[1:max(15, max)], col = c(rep("black", 
                t$nb.clust - 1), rep("grey", max(max, 15) - t$nb.clust + 
                1)), rep(0.1, max(max, 15)), space = 0.9)
            plot(x = 1, xlab = "", ylab = "", main = "", col = "white", axes = FALSE)
            text(1, 1, "Hierarchical Clustering", cex = 2)
            plot(x = 1, xlab = "", ylab = "", main = "", col = "white", axes = FALSE)
            legend("top", "inertia gain  ", box.lty = NULL, cex = 1)
        }
        else {
            if (nb.clust == 0 | nb.clust == 1) nb.clust <- -1
        }
        if ((nb.clust == 0) | (nb.clust == 1)) {
			  print("Click on the graph to cut the tree")
			  flush.console()
#            if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))){
              plot(t$tree, hang = -1, main = "Click to cut the tree",  xlab = "", sub = "")
              abline(h = auto.haut, col = "black", lwd = 3)
			  coupe <- locator(n = 1)
              while (coupe$y < min(t$tree$height)) {
                cat("No class \n")
                coupe <- locator(n = 1)
              }
              y <- coupe$y
#			} else {
#              plot(t$tree, hang = -1, main = "Tree and suggested number of clusters",  xlab = "", sub = "")
#              abline(h = auto.haut, col = "black", lwd = 3)
#			  y <- auto.haut
#			}
        } else {
            if (graph)
                plot(t$tree, hang = -1, main = "Hierarchical Classification", xlab = "", sub = "")
            if (nb.clust < 0) y <- auto.haut
            else y <- (t$tree$height[length(t$tree$height) - nb.clust + 2] + t$tree$height[length(t$tree$height) - nb.clust + 1])/2
        }
    }
    else stop("The tree should be from 'hclust' or 'agnes' class.")
    clust <- cutree(as.hclust(t$tree), h = y)
    nb.clust <- max(clust)
	X <- as.data.frame(t$res$ind$coord)
	ordColo <- unique(clust[t$tree$order])

    if (graph) {
#    if ((graph)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
#        rect <- rect.hclust(t$tree, h = y, border = seq(1, nb.clust, 1))
        rect <- rect.hclust(t$tree, h = y, border = ordColo)
        clust <- NULL
        for (j in 1:nb.clust) clust <- c(clust, rep(j, length(rect[[j]])))
        clust <- as.factor(clust)
        belong <- cbind.data.frame(t$tree$order, clust)
        belong <- belong[do.call("order", belong), ]
        clust <- as.factor(belong$clust)
        if (nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) layout(matrix(nrow=1,ncol=1,1),respect=TRUE)
    }
    if (consol) {
        res.consol <- consolidation(X, clust = clust, iter.max = iter.max, ...)
        clust <- res.consol$cluster
## ajout pour trier les classes
aux <- names(clust)
ord <- order(res.consol$centers[,1,drop=FALSE])
res.consol$centers <- res.consol$centers[ord,,drop=FALSE]
clust <- (order(ord))[clust]

## Add 2014-07-08
# if (kk<Inf){
  # rr <- as.factor(cla$cluster)
  # levels(rr)[as.integer(names(clust))]=clust
 ##levels(rr) <- 1:nlevels(rr)
  # names(rr) <- aux
  # cla$cluster <- rr
# }
names(clust) <- aux
if (kk<Inf){
clust <- clust[order(as.integer(names(clust)))]
rr <- clust[cla$cluster]
names(rr) <- names(cla$cluster)
cla$cluster <- as.factor(rr)
}
## fin ajout			
centers <- res.consol$centers
    }
    if (!consol) {
        list.centers <- by(X, clust, colMeans)
        centers <- matrix(unlist(list.centers), ncol = ncol(X),byrow = TRUE)
        colnames(centers) <- colnames(X)

		## ajout pour trier les classes
		aux <- names(clust) <- rownames(X)
		ord <- order(centers[,1,drop=FALSE])
		centers <- centers[ord,,drop=FALSE]
		clust <- (order(ord))[clust]
		names(clust) <- aux
		## Add 2014-07-08
		if (kk<Inf){
		  clust <- clust[order(as.integer(names(clust)))]
		  rr <- clust[cla$cluster]
		  names(rr) <- names(cla$cluster)
		  cla$cluster <- as.factor(rr)
		}
## fin ajout		
    }
    clust <- as.factor(clust)
## Add 2014-07-08
  X <- cbind.data.frame(X,clust)

if (kk<Inf){
  if (inherits(res.sauv, "PCA") | inherits(res.sauv, "MCA") | inherits(res.sauv,"MFA") | inherits(res.sauv, "HMFA") | inherits(res.sauv, "FAMD")){
    if (is.null(res.sauv$call$ind.sup)) data.clust <- cbind.data.frame(res.sauv$call$X, clust=cla$cluster)
    else data.clust <- cbind.data.frame(res.sauv$call$X[-res.sauv$call$ind.sup, ], clust=cla$cluster)
  } else {
    if (inherits(res.sauv, "CA")){
      if (cluster.CA=="columns") {
	    if (!is.null(res.sauv$call$col.sup)) data.clust <- cbind.data.frame(t(res.sauv$call$Xtot[,-res.sauv$call$col.sup]), clust=cla$cluster)
	    else data.clust <- cbind.data.frame(t(res.sauv$call$Xtot), clust=cla$cluster)
	  } 
	  if (cluster.CA=="rows") {
	    if (!is.null(res.sauv$call$row.sup)) data.clust <- cbind.data.frame(res.sauv$call$Xtot[-res.sauv$call$row.sup,], clust=cla$cluster)
 	    else data.clust <- cbind.data.frame(res.sauv$call$Xtot, clust=cla$cluster)
	  }
    } else {
      data.clust <- cbind.data.frame(res.sauv, clust=cla$cluster)
	}
  }
} else {
  if (inherits(res.sauv, "PCA") | inherits(res.sauv, "MCA") | inherits(res.sauv,"MFA") | inherits(res.sauv, "HMFA") | inherits(res.sauv, "FAMD")) data.clust <- cbind.data.frame(res.sauv$call$X[rownames(t$res$call$X),], clust)
  if (inherits(res.sauv, "data.frame")) data.clust <- cbind.data.frame(res.sauv[rownames(X),], clust)
#  if (inherits(res.sauv, "data.frame")) data.clust <- X cbind.data.frame(res.sauv$call$X[rownames(t$res$call$X),], clust)
  if (inherits(res.sauv, "numeric")) data.clust <- X
  if (inherits(res.sauv, "CA")) {
    if (cluster.CA=="rows") data.clust <- cbind.data.frame(res.sauv$call$Xtot[rownames(t$res$call$X),],clust)
	if (cluster.CA=="columns") data.clust <- cbind.data.frame(t(res.sauv$call$Xtot[,rownames(t$res$call$X)]),clust)
  }
}
  if (inherits(res.sauv, "PCA") | inherits(res.sauv, "MCA") | inherits(res.sauv,"MFA") | inherits(res.sauv, "HMFA") | inherits(res.sauv, "FAMD")) data.clust <- data.clust[rownames(res.sauv$ind$coord),]
  if (inherits(res.sauv, "CA")&(cluster.CA=="row")) data.clust <- data.clust[rownames(res.sauv$row$coord),]
  if (inherits(res.sauv, "CA")&(cluster.CA=="columns")) data.clust <- data.clust[rownames(res.sauv$col$coord),]
  if (inherits(res.sauv, "data.frame")) data.clust <- data.clust[rownames(res.sauv),]
  if (vec) data.clust <- as.data.frame(data.clust[, -2])
if (description){
     if (!inherits(res.sauv, "CA")&!(vec)){
	   if (!is.null(res.sauv$call$row.w.init)) desc.var <- catdes(data.clust, ncol(data.clust), proba = proba, row.w = res.sauv$call$row.w.init)
	   else desc.var <- catdes(data.clust, ncol(data.clust), proba = proba, row.w = res.sauv$call$row.w)
	 }
    else {
      if ((vec) | (is.null(res.sauv$call$quanti.sup)& is.null(res.sauv$call$quali.sup))) desc.var <- descfreq(data.clust[,-which(sapply(data.clust,is.factor))], data.clust[,ncol(data.clust)], proba = proba)
	  else { 
	   desc.var <- catdes(data.clust[,c(res.sauv$call$quanti.sup,res.sauv$call$quali.sup,ncol(data.clust))], length(c(res.sauv$call$quanti.sup,res.sauv$call$quali.sup,ncol(data.clust))), proba = proba,row.w=apply(data.clust[,-c(res.sauv$call$quanti.sup,res.sauv$call$quali.sup,ncol(data.clust))],1,sum))
       desc.var$frequency <- descfreq(data.clust[,-c(res.sauv$call$quanti.sup,res.sauv$call$quali.sup,ncol(data.clust))], data.clust[,ncol(data.clust)], proba = proba)
	   desc.var <- desc.var[c(length(desc.var),1:(length(desc.var)-1))]   # frequency will appear first
	  }
    }
    if (kk==Inf) desc.axe <- catdes(X, ncol(X), proba = proba, row.w = res$call$row.w.init)
}
  if (inherits(res.sauv, "data.frame")) tabInd <- cbind.data.frame(res.sauv,data.clust[,ncol(data.clust)])
  if (inherits(res.sauv, "PCA") | inherits(res.sauv, "MCA") | inherits(res.sauv,"MFA") | inherits(res.sauv, "HMFA") | inherits(res.sauv, "FAMD")) tabInd <- cbind.data.frame(res.sauv$ind$coord,data.clust[rownames(res.sauv$ind$coord),ncol(data.clust)])
#  if (inherits(res.sauv, "CA")&(cluster.CA=="rows")) tabInd <- cbind.data.frame(res.sauv$row$coord,data.clust[,ncol(data.clust)])
#  if (inherits(res.sauv, "CA")&(cluster.CA=="columns")) tabInd <- cbind.data.frame(res.sauv$col$coord,data.clust[,ncol(data.clust)])
  if (inherits(res.sauv, "CA")&(cluster.CA=="rows")) tabInd <- cbind.data.frame(res.sauv$row$coord,data.clust[rownames(res.sauv$row$coord),ncol(data.clust)])
  if (inherits(res.sauv, "CA")&(cluster.CA=="columns")) tabInd <- cbind.data.frame(res.sauv$col$coord,data.clust[rownames(res.sauv$col$coord),ncol(data.clust)])
colnames(tabInd)[ncol(tabInd)] <- "Cluster"

if (description){
  list.centers <- by(tabInd[,-ncol(tabInd),drop=FALSE], tabInd[,ncol(tabInd)], colMeans)
  centers <- matrix(unlist(list.centers), ncol = ncol(tabInd)-1,byrow = TRUE)
  colnames(centers) <- colnames(tabInd)[-ncol(tabInd)]
  cluster <- tabInd[,ncol(tabInd),drop=FALSE]
  para <- by(tabInd, cluster, simplify = FALSE, select, default.size = nb.par, method = metric, coord.centers = centers)
  dist <- by(tabInd, cluster, simplify = FALSE, distinctivness, default.size = nb.par, method = metric, coord.centers = centers)
  desc.ind <- list(para = para, dist = dist)
}
    if (consol) call <- list(t = t, min = min, max = max, X = X, bw.before.consol=sum(rev(t$tree$height)[1:(nb.clust-1)]),bw.after.consol=res.consol$betweenss/nrow(data.clust),vec = vec,call=match.call())
	else call <- list(t = t, min = min, max = max, X = X, bw.before.consol=sum(rev(t$tree$height)[1:(nb.clust-1)]),vec = vec,call=match.call())
	if (description){
	  if (kk!=Inf) res.HCPC <- list(data.clust = data.clust, desc.var = desc.var, desc.ind = desc.ind, call = call)
      else res.HCPC <- list(data.clust = data.clust, desc.var = desc.var, desc.axes = desc.axe, desc.ind = desc.ind, call = call)
	} else {
	  res.HCPC <- list(data.clust = data.clust, call = call)
	}
    if ((kk==Inf)&(graph)) {
#       plot.HCPC(res.HCPC,choice="tree",new.plot=FALSE)
    	if (vec || (ncol(tabInd)==2)) 
            plot.HCPC(res.HCPC, choice = "3D.map", t.level = "all", angle = 0, ind.names = FALSE,new.plot=TRUE)
        else {
          plot.HCPC(res.HCPC, choice = "3D.map", t.level = "all", ind.names = TRUE,new.plot=TRUE)
          plot.HCPC(res.HCPC, choice = "map", draw.tree = FALSE, label = "ind",new.plot=TRUE)
        }
    }
    if ((kk!=Inf)&(graph)) {
			if (inherits(res.sauv, "PCA")) plot(res.sauv, col.ind=as.numeric(data.clust$clust),new.plot=TRUE,cex=0.8)
            if (inherits(res.sauv, "MCA")) plot(res.sauv, col.ind=as.numeric(data.clust$clust),invisible=c("var","quali.sup"),new.plot=TRUE,cex=0.8)
            if (inherits(res.sauv, "MFA")) plot(res.sauv, col.ind=as.numeric(data.clust$clust),invisible=c("quali","quali.sup"),new.plot=TRUE,cex=0.8)
            if (inherits(res.sauv, "CA")&(cluster.CA=="rows")) plot(res.sauv, col.row=as.numeric(data.clust$clust),invisible=c("col","col.sup"),new.plot=TRUE,cex=0.8)
            if (inherits(res.sauv, "CA")&(cluster.CA=="columns")) plot(res.sauv, col.col=as.numeric(data.clust$clust),invisible=c("row","row.sup"),new.plot=TRUE,cex=0.8)
			legend("topleft",legend = paste("Cluster",1:nlevels(data.clust$clust)),text.col=1:nlevels(data.clust$clust),cex=0.8)
	}
    if (graph) par(mar = old.mar)
    class(res.HCPC) <- "HCPC"
    return(res.HCPC)
}
