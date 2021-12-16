HMFA<-function (X, H, type = rep("s", length(H[[1]])), ncp = 5, graph = TRUE, axes=c(1,2), name.group = NULL) {
    hdil <- function(H) {
        nbnivh <- length(H)
        dil <- H
        a <- NULL
        dil[[nbnivh]] <- rep(length(H[[nbnivh]]), length(H[[nbnivh]]))
        if (nbnivh > 1) {
            for (i in 1:(nbnivh - 1)) {
                h <- nbnivh - i
                k <- nbnivh - i + 1
                for (j in 1:length(H[[k]])) a <- c(a, rep(H[[k]][j] * dil[[k]][j], H[[k]][j]))
                dil[[h]] <- a
                a <- NULL
            }
        }
        return(dil)
    }
    htabdes <- function(H) {
        nbnivh <- length(H)
        nbvarh <- H
        if (nbnivh > 1) {
            for (i in 2:nbnivh) {
                for (j in 1:length(H[[i]])) {
                  nbvarh[[i]][j] <- 0
                  if (j == 1) {
                    for (k in 1:H[[i]][1]) nbvarh[[i]][j] <- nbvarh[[i]][j] + nbvarh[[i - 1]][k]
                  }
                  else {
                    a <- 0
                    b <- 0
                    for (n in 1:(j - 1)) a <- a + H[[i]][n]
                    a <- a + 1
                    for (n in 1:j) b <- b + H[[i]][n]
                    for (k in a:b) nbvarh[[i]][j] <- nbvarh[[i]][j] +  nbvarh[[i - 1]][k]
                  }
                }
            }
        }
        return(nbvarh)
    }
    hweight <- function(X, H, type = rep("s", length(H[[1]]))) {
        Hq <- H
        niv1 <- MFA(X, group = H[[1]], type = type, graph = FALSE)
        cw <- niv1$call$col.w
        Hq[[1]] <- niv1$call$group.mod
        Hinter <- htabdes(Hq)
        nbnivh <- length(Hq)
        cw.partiel <- H
        cw.partiel[[1]] <- cw
        for (n in 2:nbnivh) {
            niv2 <- MFA(niv1$call$XTDC, group = Hinter[[n]], type = c(rep("c", length(Hinter[[n]]))), weight.col.mfa = cw, graph = FALSE)
            cw <- niv2$call$col.w * cw
            cw.partiel[[n]] <- cw
        }
        return(cw.partiel)
    }

    if (is.null(rownames(X))) rownames(X) <- 1:nrow(X)
    if (is.null(colnames(X))) colnames(X) <- paste("V",1:ncol(X),sep="")
	X <- as.data.frame(X)
	is.quali <- which(!unlist(lapply(X,is.numeric)))
    X[,is.quali] <- lapply(X[,is.quali,drop=FALSE],as.factor)
    for (j in 1:ncol(X)) if (colnames(X)[j]=="") colnames(X)[j] <- paste("V",j,sep="")
    for (j in 1:nrow(X)) if (is.null(rownames(X)[j])) rownames(X)[j] <- paste("row",j,sep="")
	## avoid problem when a category has 0 individuals
    for (j in 1:ncol(X)) {
      if (!is.numeric(X[,j])) levels(X[,j])[which(table(X[,j])==0)] <- levels(X[,j])[which(table(X[,j])!=0)[1]]
    }

    poids <- hweight(X, H, type = type)
    nbind <- dim(X)[1]
    nbnivo <- length(H)
    res1 <- list()
    for (j in 1:ncol(X)) {
      if (is.numeric(X[,j])) X[,j] <- scale(X[,j],scale=FALSE)
    }
    niv1 <- MFA(X, group = H[[1]], type = type, ncp = ncp, graph = FALSE)
    X <- niv1$call$XTDC
    Hq <- H
    Hq[[1]] <- niv1$call$group.mod
    Xdes <- htabdes(Hq)
    ind.var <- 0
    ind.quali <- NULL
    nbgroup <- length(H[[1]])
    for (g in 1:nbgroup) {
        if (type[g] == "n") ind.quali <- c(ind.quali, c((ind.var + 1):(ind.var + niv1$call$group.mod[g])))
        ind.var <- ind.var + niv1$call$group.mod[g]
    }
    for (h in 1:nbnivo) {
        nbgroup <- length(H[[h]])
        ind.col <- 0
        data.partiel <- vector(mode = "list", length = nbgroup)
        group.mod <- Xdes[[h]]
        for (g in 1:nbgroup) {
            data.partiel[[g]] <- as.data.frame(matrix(0, nrow(X), ncol(X), byrow = TRUE, dimnames = dimnames(X)))
            data.partiel[[g]][, (ind.col + 1):(ind.col + Xdes[[h]][g])] <- X[,(ind.col + 1):(ind.col + Xdes[[h]][g])]
            ind.col <- ind.col + group.mod[g]
        }
        res1[[h]] <- data.partiel
    }
    res.afmh <- PCA(X, col.w = poids[[nbnivo]], graph = FALSE, ncp = ncp, scale.unit = FALSE)
    dilat <- hdil(H)
    nb.v.p <- ncol(res.afmh$ind$coord)
    coord.group <- list()
    for (h in 1:nbnivo) {
        res <- sweep(as.matrix(res.afmh$var$coord^2),1,poids[[h]],FUN="*")
        nbgroup <- length(H[[h]])
        ind.col <- 1
        group.mod <- Xdes[[h]]
        aux.mat <- matrix(0, nbgroup, nb.v.p)
        for (g in 1:nbgroup) {
            aux.mat[g, ] <- apply(res[ind.col:(ind.col + group.mod[g] - 1),, drop=FALSE], 2, sum)
            ind.col <- ind.col + group.mod[g]
        }
        colnames(aux.mat) <- colnames(res.afmh$var$cor)
        if (is.null(name.group)){
          name.aux <- paste("L", h, ".", sep = "")
          rownames(aux.mat) <- paste(name.aux, "G", 1:nbgroup, sep = "")
        }
        else{
          rownames(aux.mat) <- name.group[[h]]
        }
        coord.group[[h]] <- aux.mat
    }
    part1 <- list()
    if (!is.null(ind.quali)) part1.quali <- list()

    for (h in 1:nbnivo) {
        nbgroup <- length(H[[h]])
        part2 <- array(0, dim = c(nrow(res.afmh$ind$coord), nb.v.p, nbgroup))
        if (!is.null(ind.quali)) part2.quali <- array(0, dim = c(length(ind.quali), nb.v.p, nbgroup))

        for (g in 1:nbgroup) {
            formule <- matrix(0, dim(X)[1], nb.v.p)
            formule <- sweep(as.matrix(res1[[h]][[g]]),2, poids[[nbnivo]],FUN="*")
	    formule <- formule %*%t(X)
	    formule <- sweep(formule,2,rep(nbind, nbind),FUN="/")
            formule <- formule %*% as.matrix(res.afmh$ind$coord)
	    formule <- sweep(formule,2,res.afmh$eig[1:nb.v.p, 1]/ dilat[[h]][g],FUN="/")
            namecol <- paste("Dim", 1:nb.v.p, sep = "")
            formule <- matrix(formule, dim(formule)[1], dim(formule)[2])
            colnames(part2) <- namecol
            rownames(part2) <- rownames(X)
            if (!is.null(ind.quali)) {
                rownames(part2.quali) <- colnames(X)[ind.quali]
                colnames(part2.quali) <- namecol
            }
            part2[, , g] <- formule
        }
        part1[[h]] <- part2
        if (!is.null(ind.quali)) {
            for (k in 1:length(ind.quali)) 
            if (sum(as.integer(X[,ind.quali[k]] > 0))<2) part2.quali[k, , ] <- part2[X[,ind.quali[k]] > 0, , ]
            else part2.quali[k, , ] <- apply(part2[X[,ind.quali[k]] > 0, , ], c(2, 3), mean)
            part1.quali[[h]] <- part2.quali
        }
    }

## ajout
    canonical <- matrix(0, 0 ,ncol(res.afmh$ind$coord))
	colnames(canonical) <- colnames(res.afmh$ind$coord)
    for (h in 1:nbnivo) {
      nbgroup <- length(H[[h]])
	  for (g in 1:nbgroup) {
	  canonical <- rbind(canonical, diag(cor(res.afmh$ind$coord,part1[[h]][,,g])))
	  }
      if (is.null(name.group)){
        name.aux <- paste("L", h, ".", sep = "")
        rownames(canonical)[(nrow(canonical)-nbgroup+1):nrow(canonical)] <- paste(name.aux, "G", 1:nbgroup, sep = "")
      }
      else rownames(canonical)[(nrow(canonical)-nbgroup+1):nrow(canonical)] <- name.group[[h]]
	}
	## fin ajout
	res.afmh$group <- list(coord=coord.group,canonical=canonical)
    results <- list(eig = res.afmh$eig, group = res.afmh$group, ind = res.afmh$ind, partial = part1)
    if (!is.null(ind.quali) & length(ind.quali) < nrow(res.afmh$var$coord)) {
        results$quanti.var$coord <- res.afmh$var$coord[-ind.quali, ]
        results$quanti.var$cor <- res.afmh$var$cor[-ind.quali,  ]
        results$quanti.var$cos2 <- res.afmh$var$cos2[-ind.quali,  ]
        results$quanti.var$contrib <- res.afmh$var$contrib[-ind.quali, ]
    }
    if (is.null(ind.quali)) {
        results$quanti.var$coord <- res.afmh$var$coord
        results$quanti.var$cor <- res.afmh$var$cor
        results$quanti.var$cos2 <- res.afmh$var$cos2
        results$quanti.var$contrib <- res.afmh$var$contrib
    }
    if (!is.null(ind.quali)) {
        aux <- matrix(0, nrow = length(ind.quali), ncol = ncol(res.afmh$ind$coord))
        for (k in 1:length(ind.quali)) 
        if (sum(as.integer(X[,ind.quali[k]] > 0))<2) aux[k, ] <- res.afmh$ind$coord[X[,ind.quali[k]] > 0, ]
        else aux[k, ] <- apply(res.afmh$ind$coord[X[,ind.quali[k]] > 0, ], 2, mean)
        dimnames(aux) <- dimnames(res.afmh$var$contrib[ind.quali, ])
        results$quali.var$coord <- aux
        results$quali.var$contrib <- res.afmh$var$contrib[ind.quali, ]
        results$quali.var$partial <- part1.quali
    }
    results$call$H <- H
    results$call$X <- X
	results$call$call <- sys.calls()[[1]]
    if (!is.null(ind.quali)) results$call$Hq <- Xdes
    class(results) <- c("HMFA", "list")
    if (graph) {
        plot.HMFA(results, choix = "ind",axes=axes,new.plot=TRUE)
        plot.HMFA(results, choix = "var",axes=axes,new.plot=TRUE)
        plot.HMFA(results, choix = "group",axes=axes,new.plot=TRUE)
    }
    return(results)
}
