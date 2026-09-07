PCA <- function(X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
    quali.sup = NULL, row.w = NULL, col.w = NULL, graph = TRUE, axes = c(1, 2), ...){
	
    moy.ptab <- function(V, poids) {
#      as.vector(crossprod(poids/sum(poids),as.matrix(V)))
      poids <- poids / sum(poids)
	  drop(crossprod(poids, V))
    }
	
    ec.tab <- function(V, poids) {
#        ecart.type <- sqrt(as.vector(crossprod(poids/sum(poids),as.matrix(V)^2)))
		poids <- poids / sum(poids)
        ecart.type <- drop(sqrt(crossprod(poids, V^2)))
		ecart.type[ecart.type <= 1e-16] <- 1
        return(ecart.type)
    }

# fct.eta2 <- function(group, Y, weights = NULL) {
  # n <- nrow(Y)
  # if (is.null(weights)) weights <- rep(1, n)
  # group <- as.matrix(group)
  
  # r2_one_group <- function(grp) {
    # idx_ok <- which(!is.na(grp))
    # w      <- if (length(idx_ok) < n) weights[idx_ok] else weights
    # Yk     <- if (length(idx_ok) < n) Y[idx_ok, , drop=FALSE] else Y
    # grp    <- grp[idx_ok]
    # w_norm   <- w / sum(w)
    # moy_glob <- drop(crossprod(w_norm, Yk))               # vecteur k
    # Yc  <- sweep(Yk, 2, moy_glob)                         # n x k centre
    # sct <- colSums(Yc * (w * Yc))                         # vecteur k
    # ## Matrice indicatrice
    # grp_fac  <- factor(grp)
    # indic    <- tab.disjonctif(grp_fac)
    # w_grp    <- drop(crossprod(indic, w))                  # K
    # moy_g    <- sweep(crossprod(indic * w, Yk), 1, w_grp, "/")  # K x k

    # dg  <- moy_g[as.integer(grp_fac), , drop=FALSE] - rep(moy_glob, each=length(grp))
    # sce <- colSums(dg * (w * dg))                         # vecteur k

    # sce / sct
  # }

  # ## Appliquer pour chaque colonne de group matrice p x k
  # resu <- t(apply(group, 2, r2_one_group))
  # colnames(resu) <- colnames(Y)
  # resu
# }

fct.eta2 <- function(group, Y, weights = NULL) {  # no NA on Y hat are dimensions
  n <- nrow(Y)
  if (is.null(weights)) weights <- rep(1, n)
  Y   <- as.matrix(Y)

  idx_ok  <- which(!is.na(group))
  w       <- weights[idx_ok]
  Yk      <- Y[idx_ok, , drop=FALSE]
  grp_fac <- factor(group[idx_ok])
  indic   <- tab.disjonctif(grp_fac)
  K       <- nlevels(grp_fac)
  grp_idx <- as.integer(grp_fac)

  w_norm   <- w / sum(w)
  n_eff    <- sum(w)^2 / sum(w^2)
  w_grp    <- drop(crossprod(indic, w))

  moy_glob <- drop(crossprod(w_norm, Yk))
  Yc       <- sweep(Yk, 2, moy_glob)
  sct      <- colSums(Yc * (w * Yc))

  moy_g    <- sweep(crossprod(indic * w, Yk), 1, w_grp, "/")   # K x k
  dg       <- moy_g[grp_idx, , drop=FALSE] - rep(moy_glob, each=length(grp_idx))
  sce      <- colSums(dg * (w * dg))
  scr      <- sct - sce

  sce / sct
}

# fct.eta2 <- function(group, Y, weights = NULL) {  # no NA on Y that corresponds to indiv coordinates
  # prep_anova_weights <- function(group, weights = NULL) {
    # n <- length(group)
    # if (is.null(weights)) weights <- rep(1, n)
    # idx_ok <- which(!is.na(group))   # longueur n_ok <= n
    # if (length(idx_ok) < n) {
      # weights  <- weights[idx_ok]
      # group <- group[idx_ok]
    # }
    # weights_norm     <- weights / sum(weights)
    # somme_p_group <- tapply(weights, group, sum)
    # list(idx_ok= idx_ok,n_total= n,weights= weights,weights_norm= weights_norm,group=group,somme_p_group = somme_p_group)
  # }
  
  # r2_from_prep <- function(y, prep) {
    # if (length(prep$idx_ok) < prep$n_total) y <- y[prep$idx_ok]
    # moy_glob <- sum(prep$weights_norm * y)
    # d   <- y - moy_glob
    # sct <- sum(prep$weights * d * d)
    # moy_g <- tapply(prep$weights * y, prep$group, sum) / prep$somme_p_group
    # dg  <- moy_g[prep$group] - moy_glob
    # sce <- sum(prep$weights * dg * dg)
    # sce / sct
  # }
  # prep <- prep_anova_weights(group, weights)
  # Y <- as.data.frame(Y)
  # unlist(lapply(Y, r2_from_prep, prep))
# }

### main program
    X <- as.data.frame(X)
    rowX <- rownames(X)
	colX <- colnames(X)

    is.quanti <- which(vapply(X, is.numeric, logical(1)))
    is.quali <- setdiff(1:ncol(X),is.quanti)

    if (length(is.quali) > 0) {
      X[, is.quali] <- lapply(X[, is.quali, drop=FALSE], as.factor)
      niveau <- unlist(lapply(X[, is.quali, drop=FALSE], levels))
      if (anyDuplicated(niveau) || any(niveau %in% as.character(1:nrow(X)))) {
         X[, is.quali] <- Map(function(col, nom) {
           if (sum(niveau %in% levels(col)) != nlevels(col) || 
             any(levels(col) %in% as.character(1:nrow(X))))
           levels(col) <- paste(nom, levels(col), sep="_")
        col
         }, X[, is.quali, drop=FALSE], names(X)[is.quali])
      }
    }


    X <- droplevels(X)
    if (!is.null(quali.sup)  & !is.numeric(quali.sup))  quali.sup  <- which(colX %in% quali.sup)
    if (!is.null(quanti.sup) & !is.numeric(quanti.sup)) quanti.sup <- which(colX %in% quanti.sup)

    # Impute NA by means
    if (any(is.na(X[,is.quanti]))){
        warning("Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package")
#        for (j in is.quanti) X[[j]][is.na(X[[j]])] <- mean(X[[j]], na.rm = TRUE)
        cols_na <- is.quanti[vapply(X[, is.quanti, drop=FALSE], anyNA, logical(1))]
        X[, cols_na] <- lapply(X[, cols_na, drop=FALSE], function(col) {
          col[is.na(col)] <- mean(col, na.rm=TRUE)
        col
        })
    }

    res.call <- list(row.w=row.w, col.w = col.w, scale.unit = scale.unit, ncp = ncp,
        centre = 0, ecart.type = 0, X = X, row.w.init = row.w, call = match.call())   ## row.w, centre, ecart.type modified after
		
    if (!is.null(quali.sup)) {
        X.quali.sup <- X[setdiff(1:nrow(X),ind.sup), quali.sup,drop=FALSE]
#        X.quali.sup <- X[, quali.sup,drop=FALSE]
#        if (!is.null(ind.sup)) X.quali.sup <- X.quali.sup[-ind.sup,,drop=FALSE]
        colX.quali.sup <- colX[quali.sup]
	}

    if (!is.null(quanti.sup)) {
        X.quanti.sup <- as.matrix(X[setdiff(1:nrow(X),ind.sup), quanti.sup,drop=FALSE])
        colX.quanti.sup <- colX[quanti.sup]        
	}

#    if (is.null(quali.sup)) auxi <- names(X)[!sapply(X, is.numeric)]
#	else auxi <- names(X[,-quali.sup,drop=FALSE])[!sapply(X[,-quali.sup,drop=FALSE], is.numeric)]
	auxi <- names(X[,setdiff(1:ncol(X),quali.sup),drop=FALSE])[!sapply(X[,setdiff(1:ncol(X),quali.sup),drop=FALSE], is.numeric)]
    if (length(auxi) > 0) stop(paste("\nThe following variables are not quantitative:", auxi))

    todelete <- c(quali.sup, quanti.sup)
	X <- as.matrix(X[, setdiff(1:ncol(X),todelete),drop=FALSE])
	colX <- colX[setdiff(1:length(colX),todelete)]
		
    if (!is.null(ind.sup)) {
        X.ind.sup <- X[ind.sup,,drop=FALSE]
		rowX.ind.sup <- rowX[ind.sup]
        X <- X[setdiff(1:nrow(X),ind.sup),,drop=FALSE]
		rowX <- rowX[setdiff(1:length(rowX),ind.sup)]
#		X <- X[-ind.sup,,drop=FALSE]
#		rowX <- rowX[-ind.sup]
    }
    ncp <- min(ncp, nrow(X) - 1, ncol(X))
    if (is.null(row.w)) row.w <- rep(1, nrow(X))
    row.w.init <- row.w
    row.w <- row.w / sum(row.w)
    if (is.null(col.w)) col.w <- rep(1, ncol(X))

#    X <- as.matrix(X)
    centre <- moy.ptab(X, row.w)
    X   <- t(t(X) - centre)
    if (scale.unit) {
        ecart.type <- ec.tab(X, row.w)
        X <- t(t(X) / ecart.type)
    } else {
        ecart.type <- rep(1, length(centre))
    }

#    dist2.ind <- rowSums(t(t(X^2) * col.w))
#    dist2.var <- as.vector(crossprod(rep(1, nrow(X)), as.matrix(X^2 * row.w)))
    dist2.ind <- drop(X^2 %*% col.w)
    dist2.var <- colSums(X*(row.w* X))
    res.call$row.w <- row.w/sum(row.w)
    res.call$row.w.init <- row.w.init
    res.call$col.w <- col.w
	res.call$centre <- centre
    res.call$ecart.type <- ecart.type
    res.call$ncp <- ncp
#    res.call <- list(row.w = (row.w/sum(row.w)), col.w = col.w,
#        scale.unit = scale.unit, ncp = ncp, centre = centre,
#        ecart.type = ecart.type, X = Xtot, row.w.init = row.w.init, call = match.call())
    tmp <- svd.triplet(X, row.w = row.w, col.w = col.w, ncp = ncp)
    eig <- tmp$vs^2
    vp  <- matrix(NA, length(eig), 3)
    rownames(vp) <- paste("comp", 1:length(eig))
    colnames(vp) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
    vp[, "eigenvalue"] <- eig
#    vp[, "percentage of variance"] <- (eig/sum(t(t(X^2)*col.w)*row.w)) * 100
    vp[, "percentage of variance"] <- (eig/drop(row.w %*% (X^2 %*% col.w))) * 100
    vp[, "cumulative percentage of variance"] <- cumsum(vp[, "percentage of variance"])

    V <- tmp$V
    U <- tmp$U
    eig <- eig[1:ncp]
    coord.ind  <- t(t(as.matrix(U)) * sqrt(eig))
    coord.var  <- t(t(as.matrix(V)) * sqrt(eig))
    contrib.var <- t(t(coord.var^2) / eig) * col.w
    contrib.var <- sweep(coord.var^2 * col.w, 2, eig, "/")
    cor.var    <- coord.var / sqrt(dist2.var)
    cos2.var   <- cor.var^2
    rownames(coord.var) <- rownames(cos2.var) <- rownames(cor.var) <- rownames(contrib.var) <- colX
    colnames(coord.var) <- colnames(cos2.var) <- colnames(cor.var) <- colnames(contrib.var) <- paste("Dim", 
        c(1:ncol(V)), sep = ".")
    res.var <- list(coord = coord.var[, 1:ncp,drop=FALSE], cor = cor.var[, 
        1:ncp,drop=FALSE], cos2 = cos2.var[, 1:ncp,drop=FALSE], contrib = contrib.var[, 
        1:ncp,drop=FALSE] * 100)
    dist2 <- dist2.ind
    cos2.ind <- coord.ind^2/dist2
    contrib.ind <- t(t(coord.ind^2*row.w/sum(row.w))/eig)
    rownames(coord.ind) <- rownames(cos2.ind) <- rownames(contrib.ind) <- names(dist2) <- rowX
    colnames(coord.ind) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("Dim", 
        c(1:ncol(U)), sep = ".")
    res.ind <- list(coord = coord.ind[, 1:ncp,drop=FALSE], cos2 = cos2.ind[, 
        1:ncp,drop=FALSE], contrib = contrib.ind[, 1:ncp,drop=FALSE] * 100, dist = sqrt(dist2))
    res <- list(eig = vp, var = res.var, ind = res.ind, svd = tmp)

    if (!is.null(ind.sup)) {
        if (is.null(ecart.type)) ecart.type <- rep(1, length(centre))
        X.ind.sup <- t(t(as.matrix(X.ind.sup))-centre)
        X.ind.sup <- t(t(X.ind.sup)/ecart.type)
        coord.ind.sup <- t(t(X.ind.sup)*col.w)
        coord.ind.sup <- crossprod(t(coord.ind.sup),tmp$V)
        dist2 <- rowSums(t(t(X.ind.sup^2)*col.w))
        cos2.ind.sup <- coord.ind.sup^2/dist2
        coord.ind.sup <- coord.ind.sup[, 1:ncp, drop = F]
        cos2.ind.sup <- cos2.ind.sup[, 1:ncp, drop = F]
        colnames(coord.ind.sup) <- colnames(cos2.ind.sup) <- paste("Dim",  c(1:ncp), sep = ".")
        rownames(coord.ind.sup) <- rownames(cos2.ind.sup) <- names(dist2) <- rowX.ind.sup
        res.ind.sup <- list(coord = coord.ind.sup, cos2 = cos2.ind.sup, dist = sqrt(dist2))
        res$ind.sup <- res.ind.sup
        res.call$ind.sup <- ind.sup
    }
    if (!is.null(quanti.sup)) {
        centre.sup <- moy.ptab(X.quanti.sup,row.w)
        res.call$quanti.sup <- X.quanti.sup
        X.quanti.sup <- t(t(as.matrix(X.quanti.sup))-centre.sup)
        if (scale.unit) {
            ecart.type.sup <- ec.tab(X.quanti.sup, row.w)
            X.quanti.sup <- t(t(X.quanti.sup)/ecart.type.sup)
        }
        coord.vcs <- t(X.quanti.sup*row.w)
        coord.vcs <- crossprod(t(coord.vcs),tmp$U)
        col.w.vcs <- rep(1, ncol(coord.vcs))
        cor.vcs <- matrix(NA, ncol(X.quanti.sup), ncol(tmp$U))
		dist2 <- as.vector(crossprod(rep(1,nrow(X.quanti.sup)),as.matrix(X.quanti.sup^2*row.w)))
        cor.vcs <- coord.vcs/sqrt(dist2)
        cos2.vcs <- cor.vcs^2
        colnames(coord.vcs) <- colnames(cor.vcs) <- colnames(cos2.vcs) <- paste("Dim", c(1:ncol(cor.vcs)), sep = ".")
        rownames(coord.vcs) <- rownames(cor.vcs) <- rownames(cos2.vcs) <- colX.quanti.sup
        res.quanti.sup <- list(coord = coord.vcs[, 1:ncp, drop=FALSE], cor = cor.vcs[, 1:ncp, drop=FALSE], cos2 = cos2.vcs[, 1:ncp, drop=FALSE])
        res$quanti.sup <- res.quanti.sup
    }
    if (!is.null(quali.sup)) {
        nombre <- modalite <- NULL
        eta2 <- t(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w))
#		if (ncp>1) eta2 <- t(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w))
#		else {
#		  eta2 <- as.matrix(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w),ncol=ncp)
#		}
#        colnames(eta2) <- paste0("Dim.", 1:ncp)
#        rownames(eta2) <- colX.quali.sup
    X.act <- as.matrix(res.call$X[setdiff(1:nrow(res.call$X), ind.sup),
                               setdiff(1:ncol(res.call$X), todelete), drop=FALSE])
	## Masque des valeurs non-NA (n x p) 
	X.act.hasna <- anyNA(X.act)

	if (!X.act.hasna) {
	  fctbar <- lapply(1:ncol(X.quali.sup), function(i) {
		var   <- as.factor(X.quali.sup[, i])
		levs  <- levels(var)
		idx_ok <- which(!is.na(var))
		var_ok <- var[idx_ok]
		indic  <- tab.disjonctif(var_ok)                     # n_ok x K
		w_mat  <- indic * row.w[idx_ok]                      # n_ok x K
		w_grp  <- drop(crossprod(w_mat, rep(1, length(idx_ok))))  # K
		bary   <- sweep(crossprod(w_mat, X.act[idx_ok, ]), 1, w_grp, "/")  # K x p
		rownames(bary) <- if (levs[1] %in% as.character(1:nrow(X)) ||
							  levs[1] %in% c("y","Y","n","N"))
							paste(colX.quali.sup[i], levs, sep=".")
						  else levs
		colnames(bary) <- colX
		list(bary=bary, n.mod=nlevels(var), nombre=drop(crossprod(indic, row.w.init[idx_ok])))
	  })
	} else {
	  X.act.ok  <- !is.na(X.act)
	  X.act.num <- X.act
	  X.act.num[!X.act.ok] <- 0
	  p <- ncol(X.act)
	  fctbar <- lapply(1:ncol(X.quali.sup), function(i) {
		var    <- as.factor(X.quali.sup[, i])
		levs   <- levels(var)
		idx_ok <- which(!is.na(var))
		var_ok <- var[idx_ok]
		indic  <- tab.disjonctif(var_ok)                     # n_ok x K
		w_mat  <- indic * row.w[idx_ok]                      # n_ok x K
		w_grp_and_num <- crossprod(w_mat, cbind(X.act.ok[idx_ok, ], X.act.num[idx_ok, ]))  # K x 2p
		w_grp  <- w_grp_and_num[, 1:p, drop=FALSE]
		num    <- w_grp_and_num[, (p+1):(2*p), drop=FALSE]
		bary   <- num / ifelse(w_grp > 0, w_grp, NA)
		rownames(bary) <- if (levs[1] %in% as.character(1:nrow(X)) ||
							  levs[1] %in% c("y","Y","n","N"))
							paste(colX.quali.sup[i], levs, sep=".")
						  else levs
		colnames(bary) <- colX
		list(bary=bary, n.mod=nlevels(var), nombre=drop(crossprod(indic, row.w.init[idx_ok])))
	  })
	}

	barycentre <- do.call(rbind, lapply(fctbar, `[[`, "bary"))
	modalite   <- c(modalite, sapply(fctbar, `[[`, "n.mod"))
	nombre     <- c(nombre,   unlist(lapply(fctbar, `[[`, "nombre")))

#        for (i in 1:ncol(X.quali.sup)) {
#            var <- as.factor(X.quali.sup[,i])
#            n.mod <- nlevels(var)
#            modalite <- c(modalite, n.mod)
#            bary <- matrix(NA, n.mod, ncol(X))
#            for (j in 1:n.mod) {
#                ind <- levels(var)[j]
#                bary[j, ] <- moy.ptab((as.matrix(res.call$X[setdiff(1:nrow(res.call$X),ind.sup),setdiff(1:ncol(res.call$X),todelete),drop=FALSE]))[which(var == ind),,drop=FALSE], row.w[which(var == ind)])
#                nombre <- c(nombre, sum(row.w.init[which(var == ind)]))
#            }
#            colnames(bary) <- colX
#            if ((levels(var)[1] %in% (1:nrow(X))) | (levels(var)[1] %in% c("y", "Y", "n", "N"))) row.names(bary) <- paste(colX.quali.sup[i], as.character(levels(var)),sep=".")
#            else row.names(bary) <- as.character(levels(var))
#            if (i == 1)  barycentre <- bary
#            else barycentre <- rbind(barycentre, bary)
#        }
        bary <- t(t(barycentre)-centre)
        if (!is.null(ecart.type)) bary <- t(t(bary)/ecart.type)
        dist2 <- rowSums(t(t(bary^2)*col.w))
        coord.barycentre <- t(t(bary)*col.w)
        coord.barycentre <- crossprod(t(coord.barycentre),tmp$V)
        colnames(coord.barycentre) <- paste("Dim", 1:ncol(coord.barycentre), sep = ".")
        cos2.bary.sup <- coord.barycentre^2/dist2
        vtest <- t(t(coord.barycentre)/sqrt(eig))
        if (sum(row.w.init)>1) vtest <- vtest*sqrt(nombre/((sum(row.w.init) - nombre)/(sum(row.w.init) - 1)))
		else vtest <- vtest*sqrt(nombre)
        cos2.bary.sup <- cos2.bary.sup[, 1:ncp, drop=FALSE]
        coord.barycentre <- coord.barycentre[, 1:ncp, drop=FALSE]
        vtest <- vtest[, 1:ncp, drop=FALSE]
        dimnames(cos2.bary.sup) <- dimnames(vtest) <- dimnames(coord.barycentre)
        names(dist2) <- rownames(coord.barycentre)
        res.quali.sup <- list(coord = coord.barycentre, cos2 = cos2.bary.sup, v.test = vtest, dist = sqrt(dist2), eta2=eta2)
        call.quali.sup <- list(quali.sup = X.quali.sup, modalite = modalite, nombre = nombre, barycentre = as.data.frame(barycentre), numero = quali.sup)
        res$quali.sup <- res.quali.sup
        res.call$quali.sup <- call.quali.sup
    }
    res$call <- res.call
    class(res) <- c("PCA", "list")
    if (graph & (ncp>1)) {
        print(plot.PCA(res, choix = "ind", axes = axes, ...))
        print(plot.PCA(res, choix = "var", axes = axes,shadowtext=TRUE,new.plot=TRUE, ...))
    }
    return(res)
}
