catdes <- function(donnee,num.var,proba = 0.05,row.w=NULL, na.method="NA", html.table=TRUE){

    # moy.p <- function(V, fac=NULL, poids, na.rm=TRUE) {
		# poids[is.na(V)] <- 0
        # if (is.null(fac)) {
		  # res <- sum(V * poids,na.rm=na.rm)/sum(poids)
		# } else {
		  # res <- NULL
		  # for (i in 1:nlevels(fac)) res <- c(res, sum(V[fac==levels(fac)[i]] * poids[fac==levels(fac)[i]],na.rm=na.rm)/sum(poids[fac==levels(fac)[i]]))
		# }
		# return(res)
    # }
    # ec <- function(V, fac=NULL, poids, na.rm=TRUE) {
		# poids[is.na(V)] <- 0
        # if (is.null(fac)){
  		  # V <- V-moy.p(V,fac=NULL,poids,na.rm)
		  # res <- sum(V^2 * poids,na.rm=na.rm)/sum(poids)
        # } else {
		  # moy.par.mod <- moy.p(V,fac=fac,poids,na.rm)
		  # res <- NULL
		  # for (i in 1:nlevels(fac)) {
		   # res <- c(res, sum((V[fac==levels(fac)[i]]-moy.par.mod[i])^2 * poids[fac==levels(fac)[i]],na.rm=na.rm)/sum(poids[fac==levels(fac)[i]]))
		# }}
		# return(sqrt(res))
    # }
	moy.p <- function(V, fac=NULL, poids, na.rm=TRUE) { # fac is a dummy matrix of the factor
	  idx.na          <- is.na(V)
	  poids[idx.na]   <- 0
 	  V[idx.na]       <- 0
	  if (is.null(fac)) {
		sum(V * poids, na.rm=na.rm) / sum(poids)
	  } else {
		num     <- drop(crossprod(fac, V * poids))           # K
		denom   <- drop(crossprod(fac, poids))               # K
		num / denom
	  }
	}

	ec <- function(V, fac=NULL, poids, na.rm=TRUE) {
      idx.na          <- is.na(V)
      poids[idx.na]   <- 0
      V[idx.na]       <- 0
	  if (is.null(fac)) {
		m   <- sum(V * poids, na.rm=na.rm) / sum(poids)
		Vc  <- V - m
		sqrt(sum(Vc^2 * poids, na.rm=na.rm) / sum(poids))
	  } else {
		denom   <- drop(crossprod(fac, poids))               # K
		sum.par.moy <- drop(crossprod(fac, V * poids))
		moy.par.mod <- ifelse(denom==0,0,sum.par.moy / denom)  # K
		Vc      <- V - fac%*%moy.par.mod           # residus
		num     <- drop(crossprod(fac, Vc^2 * poids))        # K
		sqrt(num / denom)
	  }
	}

	fct.eta2 <- function(group, Y, weights = NULL) {
	  n <- nrow(Y)
	  if (is.null(weights)) weights <- rep(1, n)
	  Y <- as.matrix(Y)
	  if (tolower(na.method)!="na"){
	    id_grp <- which(!is.na(group))
		Y <- Y[id_grp,,drop=FALSE]
		group <- group[id_grp]
		weights <- weights[id_grp]
	  }
	  tab.disj.group <- tab.disjonctif(group)
	  K       <- nlevels(group)
	  grp_idx <- max.col(tab.disj.group)
	  n_eff   <- sum(weights)^2 / sum(weights^2)
	  r2_one_col <- function(y) {  	  ## Y var by var due to NA
		idx_ok   <- which(!is.na(y))
		w        <- weights[idx_ok]
		y        <- y[idx_ok]
		grp      <- grp_idx[idx_ok]
		K        <- length(unique(grp))
		tdg      <- tab.disj.group[idx_ok, , drop=FALSE]

		w_norm   <- w / sum(w)
#		n_eff_k  <- sum(w)^2 / sum(w^2)
	    n_eff_k    <- sum(w!=0)
		moy_glob <- sum(w_norm * y)
		yc       <- y - moy_glob
		sct      <- sum(w * yc * yc)

		w_grp    <- drop(crossprod(tdg, w))
		moy_g    <- drop(crossprod(tdg * w, y)) / w_grp
		dg       <- moy_g[grp] - moy_glob
		sce      <- sum(w * dg * dg)
		scr      <- sct - sce
		ddl.inter <- K - 1
		ddl.intra <- n_eff_k - K
		eta2    <- sce / sct
		F.stat  <- (sce / ddl.inter) / (scr / ddl.intra)
		p.value <- pf(F.stat, ddl.inter, ddl.intra, lower.tail=FALSE)
	c(eta2=eta2, p.value=p.value)
	  }
	if (anyNA(Y)){
  	  res <- apply(Y, 2, r2_one_col) 
	  t(res)
	} else { 
	  w_norm   <- weights / sum(weights)
#	  n_eff    <- sum(weights)^2 / sum(weights^2)
	  n_eff    <- sum(weights!=0)
	  w_grp    <- drop(crossprod(tab.disj.group, weights))

	  moy_glob <- drop(crossprod(w_norm, Y))
	  Yc       <- sweep(Y, 2, moy_glob)
	  sct      <- colSums(Yc * (weights * Yc))
	  moy_g    <- sweep(crossprod(tab.disj.group * weights, Y), 1, w_grp, "/")   # K x k
	  dg       <- moy_g[grp_idx, , drop=FALSE] - rep(moy_glob, each=length(grp_idx))
	  sce      <- colSums(dg * (weights * dg))
	  scr      <- sct - sce

	  ddl.inter <- K - 1
	  ddl.intra <- n_eff - K
	  eta2    <- sce / sct
	  F.stat  <- (sce / ddl.inter) / (scr / ddl.intra)
	  p.value <- pf(F.stat, ddl.inter, ddl.intra, lower.tail=FALSE)
	  matrix(c(eta2=eta2, p.value=p.value),ncol=2)
	}
}
	
  donnee <- as.data.frame(donnee)
  if (is.numeric(donnee[,num.var])) stop(paste("The variable",num.var,"must be qualitative"))
  if (is.null(row.w)) row.w <- rep(1,nrow(donnee))
  is.quali <- which(!vapply(donnee, is.numeric, logical(1)))
  donnee[,is.quali] <- lapply(donnee[,is.quali,drop=FALSE],as.factor)
  if (tolower(na.method)!="na" & anyNA(donnee[,num.var])){
    if (length(row.w)==nrow(donnee)) row.w <- row.w[!is.na(donnee[,num.var])]
    donnee <- donnee[!is.na(donnee[,num.var]),,drop=FALSE]
  }
  donnee <- droplevels(donnee)
  lab.sauv <- lab <- colnames(donnee)
  donnee[, is.quali] <- lapply(donnee[, is.quali, drop=FALSE], function(col) {
    if (anyNA(col)) {
      levels(col) <- c(levels(col), "NA")
      col[is.na(col)] <- "NA"
    }
    if (levels(col)[1] == "") levels(col)[1] <- "NA"
    col })
  # for (i in 1:length(lab)){
    # lab[i] <- gsub(" ",".",lab[i])
    # if (is.factor(donnee[,i])) {
         # if(any(is.na(donnee[,i]))){
             # levels(donnee[,i]) <- c(levels(donnee[,i]), "NA")
             # donnee[,i][is.na(donnee[,i])] <- "NA"
         # }
      # if (levels(donnee[,i])[1]=="") levels(donnee[,i])[1] <- "NA"
    # }
  # }
  quali  <- setdiff(is.quali, num.var)
  quanti <- setdiff(1:ncol(donnee),c(quali,num.var))
  if (length(quanti)==0) quanti <- NULL
  colnames(donnee) <- lab
  res <- list()

  nb.modalite <- nlevels(donnee[,num.var])
  nb.quali <- length(quali)
  old.warn <- options("warn")
  tab.disj.num.var <- tab.disjonctif(donnee[,num.var])  ## no NA
  if (nb.quali>0){
    options(warn = -1)
# --- Initialisation ---
Test.chi <- matrix(NA, nrow = nb.quali, ncol = 2,
                   dimnames = list(colnames(donnee)[quali], c("p.value", "df")))

nom <- tri <- structure(
  vector(mode = "list", length = nb.modalite),
  names = levels(donnee[, num.var])
)

indicateur.quali <- 0

# --- Boucle principale sur les variables qualitatives ---
for (i in seq_len(nb.quali)) {

  Table <- t(sweep(tab.disj.num.var, 1, row.w, FUN = "*")) %*%
             tab.disjonctif(donnee[, quali[i]])

  # Suppression colonne NA si necessaire
  if (tolower(na.method) != "na" && colnames(Table)[ncol(Table)] == "NA")
    Table <- Table[, -ncol(Table), drop = FALSE]

  # Suppression des modalites a effectif nul
  marge.li  <- rowSums(Table)
  keep      <- marge.li != 0
  Table     <- Table[keep, , drop = FALSE]
  marge.li  <- marge.li[keep]

  if (nrow(Table) == 0 || ncol(Table) == 0) next

  marge.col <- colSums(Table)
  N         <- sum(marge.li)

  # Test du chi2
  test_res       <- chisq.test(Table, correct = FALSE)
  Test.chi[i, 1] <- test_res$p.value
  Test.chi[i, 2] <- test_res$parameter

  # --- Vectorisation de la double boucle j/k ---
  nr <- nrow(Table)
  nc <- ncol(Table)

  # Vecteurs aplatis (ordre : colonne par colonne, comme as.vector)
  n_jk  <- round(as.vector(Table),              0)
  mli_r <- round(rep(marge.li,      times = nc), 0)
  mco_r <- round(rep(marge.col, each  = nr),     0)
  N_r   <- round(N, 0)

  # p-values hypergeometriques bilaterales (vectorisees)
  p_left  <- phyper(n_jk - 1, mli_r, N_r - mli_r, mco_r) * 2 +
             dhyper(n_jk,      mli_r, N_r - mli_r, mco_r)
  p_right <- phyper(n_jk, mli_r, N_r - mli_r, mco_r, lower.tail = FALSE) * 2 +
             dhyper(n_jk, mli_r, N_r - mli_r, mco_r)
  aux4 <- pmin(p_left, p_right)

  mask <- aux4 <= proba
  if (any(mask)) {

    # Indices j (modalite de num.var) et k (modalite de quali[i])
    idx   <- which(mask)
    j_idx <- ((idx - 1) %% nr) + 1
    k_idx <- ((idx - 1) %/% nr) + 1

    mod.var <- rownames(Table)
    mod.i   <- levels(donnee[, quali[i]])

    # Proportions vectorisees
    aux2_vec <- Table[cbind(j_idx, k_idx)] / marge.li[j_idx]   # Mod/Cla
    aux3_vec <- marge.col[k_idx] / N                            # Global
    aux1_vec <- Table[cbind(j_idx, k_idx)] / marge.col[k_idx]  # Cla/Mod
    aux5_vec <- (1 - 2 * as.integer(aux2_vec > aux3_vec)) *
                qnorm(aux4[mask] / 2)                           # v.test

    # Remplissage de tri et nom par modalite de num.var
    for (j in seq_len(nr)) {
      sel <- j_idx == j
      if (!any(sel)) next
      mod_j <- mod.var[j]
      tri[[mod_j]] <- rbind(tri[[mod_j]],
                            cbind(aux1_vec[sel] * 100,
                                  aux2_vec[sel] * 100,
                                  aux3_vec[sel] * 100,
                                  aux4[mask][sel],
                                  aux5_vec[sel]))
      nom[[mod_j]] <- rbind(nom[[mod_j]],
                            cbind(mod.i[k_idx[sel]],
                                  colnames(donnee)[quali[i]]))
    }
  }
}

# --- Filtrage et tri du tableau chi2 ---
Test.chi <- Test.chi[!is.na(Test.chi[, 1]) & Test.chi[, 1] <= proba, , drop = FALSE]

if (nrow(Test.chi) > 0) {
  Test.chi      <- Test.chi[order(Test.chi[, 1]), , drop = FALSE]
  res$test.chi2 <- Test.chi
}

# --- Construction du tableau des categories ---
for (j in seq_len(nb.modalite)) {
  mat <- tri[[j]]
  if (is.null(mat)) next

  indicateur.quali <- 1
  oo    <- order(mat[, 5], decreasing = TRUE)
  mat   <- matrix(mat[oo, ], ncol = 5)
  n_mat <- nom[[j]]
  n_mat <- if (is.matrix(n_mat)) n_mat[oo, ,drop=FALSE] else matrix(n_mat[oo], nrow = 1)
  rownames(mat) <- paste(n_mat[, 2], n_mat[, 1], sep = "=")
  colnames(mat) <- c("Cla/Mod", "Mod/Cla", "Global", "p.value", "v.test")

  tri[[j]] <- mat
  nom[[j]] <- n_mat
}
    # Test.chi <- matrix(NA,nrow=nb.quali,ncol=2)
# #	marge.li <- apply(sweep(tab.disjonctif(donnee[,num.var]),1,row.w,FUN="*"),2,sum) # row margin can be different if NA
    # nom <- tri <- structure(vector(mode = "list", length = nb.modalite), names = levels(donnee[,num.var]))
    # indicateur.quali <- 0
    # for (i in 1:nb.quali){
	  # Table <- t(sweep(tab.disj.num.var,1,row.w,FUN="*"))%*%tab.disjonctif(donnee[,quali[i]])
# ### Ajout 04/07/2023
	  # if (tolower(na.method)!="na" & colnames(Table)[ncol(Table)]=="NA") Table <- Table[,-ncol(Table)]
# ### Fin ajout 04/07/2023
	# marge.li <- apply(Table,1,sum) # row margin can be different if NA in the variable
	# if (any(marge.li==0)){     ## supprime modalites dont effectif egal a 0 
	  # Table <- Table[marge.li!=0,,drop=FALSE]
	  # marge.li <- marge.li[marge.li!=0]
    # }
	# marge.col <- apply(Table,2,sum)
	# Test <- chisq.test(Table,correct=FALSE)
    # Test.chi[i,1] <- Test$p.value
    # Test.chi[i,2] <- Test$parameter
    # for (j in 1:nrow(Table)) {
     # for (k in 1:ncol(Table)) {
       # aux2 <- Table[j,k]/marge.li[j]
       # aux3 <- marge.col[k]/sum(marge.col)
       # aux4 <- min(phyper(round(Table[j,k],0)-1,round(marge.li[j],0),round(sum(marge.li),0)-round(marge.li[j],0),round(marge.col[k],0))*2+dhyper(round(Table[j,k],0),round(marge.li[j],0),round(sum(marge.li),0)-round(marge.li[j],0),round(marge.col[k],0)),phyper(round(Table[j,k],0),round(marge.li[j],0),round(sum(marge.li),0)-round(marge.li[j],0),round(marge.col[k],0),lower.tail=FALSE)*2+dhyper(round(Table[j,k],0),round(marge.li[j],0),round(sum(marge.li),0)-round(marge.li[j],0),round(marge.col[k],0)))
       # if (aux4 <= proba) {
         # aux5 <- (1-2*as.integer(aux2>aux3))*qnorm(aux4/2)
         # aux1 <- Table[j,k]/marge.col[k]
# #         tri[[j]] <- rbind(tri[[j]],c(aux1*100,aux2*100,aux3*100,aux4,aux5, Table[j, k]))
# #         nom[[j]] <- rbind(nom[[j]],c(levels(donnee[,quali[i]])[k],colnames(donnee)[quali[i]]))
         # tri[[rownames(Table)[j]]] <- rbind(tri[[rownames(Table)[j]]],c(aux1*100,aux2*100,aux3*100,aux4,aux5))
         # nom[[rownames(Table)[j]]] <- rbind(nom[[rownames(Table)[j]]],c(levels(donnee[,quali[i]])[k],colnames(donnee)[quali[i]]))
       # }
     # }
    # }
    # rownames(Test.chi) <- colnames(donnee)[quali]
   # }
   # if (nrow(matrix(Test.chi,ncol=2))>1){
     # if (sum(Test.chi[,1] <= proba)==1){
       # nomaux <- rownames(Test.chi[order(Test.chi[,1]),])[1]
       # Test.chi <- matrix(Test.chi[Test.chi[,1] <= proba,],ncol=2)
       # rownames(Test.chi) <- nomaux
     # }
     # else Test.chi <- Test.chi[Test.chi[,1] <= proba,]
   # }
   # else if (Test.chi[,1] > proba) Test.chi <- NULL
   # if (!is.null(Test.chi)){
     # if (nrow(matrix(Test.chi,ncol=2))>1){
       # oo <- order(Test.chi[,1])
       # Test.chi <- Test.chi[oo,]
     # }  
     # colnames(Test.chi) <- c("p.value","df")
     # res$test.chi2 <- Test.chi
   # }
   # for (j in 1:nb.modalite){
     # if (!is.null(tri[[j]])){
       # indicateur.quali <- 1
       # oo <- rev(order(tri[[j]][,5]))
       # tri[[j]] <- tri[[j]][oo,]
       # nom[[j]] <- nom[[j]][oo,]
       # if (nrow(matrix(tri[[j]],ncol=5))>1) rownames(tri[[j]]) <- paste(nom[[j]][,2],nom[[j]][,1],sep="=")
       # else {
         # tri[[j]] <- matrix(tri[[j]],ncol=5)
         # rownames(tri[[j]]) <- paste(nom[[j]][2],nom[[j]][1],sep="=")
       # }
       # colnames(tri[[j]]) <-  c("Cla/Mod","Mod/Cla","Global","p.value","v.test")
# #       colnames(tri[[j]]) <-  c("Cla/Mod","Mod/Cla","Global","p.value","v.test","n")
     # }
   # }
    if (indicateur.quali>0) res$category <- tri
   }

  if (!is.null(quanti)){
    nom <- result <- structure(vector(mode = "list", length = nb.modalite), names = levels(donnee[,num.var]))
#	tabF <- matrix(0, length(quanti), 2)
	tabF <- fct.eta2(donnee[,num.var], donnee[,quanti,drop=FALSE], weights=row.w)
    for (i in 1:length(quanti)){
#	  tabF[i,] <- fct.eta2(tab.disj.num.var, donnee[,quanti[i]], weights=row.w)
#      res.aov <- summary(aov(donnee[,quanti[i]]~donnee[,num.var], na.action = na.exclude,weights=row.w))[[1]]
#      tabF[i, 1] <- res.aov[1,2]/sum(res.aov[,2])
#      tabF[i, 2] <- res.aov[1,5]
      moy.mod <- moy.p(donnee[,quanti[i]],fac=tab.disj.num.var,poids=row.w)
	  n.mod <- apply(sweep(tab.disj.num.var,1,row.w*as.numeric(!is.na(donnee[,quanti[i]])),FUN="*"),2,sum)
      sd.mod <- ec(donnee[,quanti[i]],fac=tab.disj.num.var,poids=row.w)
      moy <- moy.p(donnee[,quanti[i]],poids=row.w)
      et <- ec(donnee[,quanti[i]],poids=row.w)
      n   = table(is.na(donnee[, quanti[i],drop=FALSE]),donnee[,num.var])[1,]
      for (j in 1:nb.modalite){
        v.test <- (moy.mod[j]-moy)/et*sqrt(n.mod[j])/sqrt((sum(n.mod)-n.mod[j])/(sum(n.mod)-1))
        p.value <- pnorm(abs(v.test),lower.tail = FALSE)*2
        if(!is.na(v.test)){
        if (p.value <= proba) {
          result[[j]] <- rbind(result[[j]],c(v.test,moy.mod[j],moy,sd.mod[j],et,p.value,n[j]))
          nom[[j]] <- c(nom[[j]],colnames(donnee)[quanti[i]])
        }
       }
      }
	}
    dimnames(tabF) <- list(colnames(donnee)[quanti], c("Eta2", "P-value"))
    auxF <- tabF[order(tabF[, 2]),,drop=FALSE]
    select1 <- (1:nrow(auxF))[auxF[, 2,drop=FALSE] <= proba]
    if (length(select1) > 0) resF <- auxF[select1,,drop=FALSE]
    for (j in 1:nb.modalite){
      if (!is.null(result[[j]])){
        oo <- rev(order(result[[j]][,1]))
        result[[j]] <- result[[j]][oo,,drop=FALSE]
        nom[[j]] <- nom[[j]][oo]
#        result[[j]] <- matrix(result[[j]],ncol=6)
        result[[j]] <- matrix(result[[j]],ncol=7)
        rownames(result[[j]]) <- nom[[j]]
#        colnames(result[[j]]) <- c("v.test","Mean in category","Overall mean","sd in category","Overall sd","p.value")
        colnames(result[[j]]) <- c("v.test","Mean in category","Overall mean","sd in category","Overall sd","p.value","n")
      }
    }
    if (length(select1)>0) {
	  res$quanti.var <- resF
	  res$quanti <- result
	}
  }
  res$call <- list(num.var=num.var, proba=proba, row.w=row.w, X=donnee, na.method=na.method)
  options(old.warn)
  class(res) <- c("catdes", "list")
  if (html.table){
    if (!is.null(res$test.chi2)) print(plot.catdes(res, level=proba, output="dt", show="test.chi2"))
    if (!is.null(res$quanti.var)) print(plot.catdes(res, level=proba, output="dt", show="quanti.var"))
    if (!is.null(res$quanti) || !is.null(res$category)) print(plot.catdes(res, level=proba, output="dt", show="all"))
  }
  return(res)
}
