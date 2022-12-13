LinearModel <- function (formula, data, na.action = na.omit, type = c("III", "II", 3, 2), selection=c("none","aic","bic"), ...) 
{
  old.contr <- options()$contrasts
  on.exit(options(contrasts = old.contr))
  cl <- match.call()
  if (missing(data)) {
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", 
                 "na.action", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(stats::model.frame)
    data <- eval(mf, parent.frame())
  }
  type <- type[1]
  data <- as.data.frame(data)
  data <- droplevels(data)
  is.quali <- which(!unlist(lapply(data,is.numeric)))
  data[,is.quali] <- lapply(data[,is.quali,drop=FALSE],as.factor)
  options(contrasts = c("contr.sum", "contr.sum"))
  don <- data
  if (is.null(selection)) selection <- "none"
  selection <- tolower(selection[1])
  selection <-  match.arg(selection,c("bic","aic","none","no"))
  arg <- list(...)
  arg <- c(arg, list(formula = formula, data = don, na.action = na.action))
  modele <- do.call(lm, arg)
	  aic <- extractAIC(modele)[2]
	  bic <- extractAIC(modele, k=log(nrow(data)))[2]
#  modele <- do.call(aov, arg)
  if ((selection=="bic") | (selection=="aic")){
	sumLmComp <- summary.lm(modele)
	sumLmComp$call <- cl
	sumLmComp$call[[1]]=as.name("lm")
	sumLmComp$call <- sumLmComp$call[-length(sumLmComp$call)]
	sumLmComp$aic <- aic
	sumLmComp$bic <- bic
#	cl[[1]] <- quote(lm)  # ne pas mettre car pb si variable quali
#	if ("selection"%in%names(cl)) cl <- cl[-which(names(cl)%in%"selection")]
	clComp <- cl
    if (tolower(selection)=="bic") modSelect <- step(modele, k=log(nrow(data)),trace=0)
    if (tolower(selection)=="aic") modSelect <- step(modele,trace=0)
    if (length(modSelect$coefficients)==1){
	  warning("The selection leads to the null model (with 0 variables).\n  Here are the results of the full model without selection ... but it may be not useful!")
	} else {
	  cl$formula <- modSelect$call$formula
      arg <- list(...)
      arg <- c(arg, list(formula = as.formula(modSelect$call$formula), data = don, na.action = na.action))
	  modele <- do.call(lm, arg)
	  aic <- extractAIC(modele)[2]
	  bic <- extractAIC(modele, k=log(nrow(data)))[2]
	}
  }
  test.F <- car::Anova(modele, type = type)
  if ((type=="III") | (type=="3")) test.F <- test.F[-1, ]
  test.F <- test.F[c(1, 2, 2, 3, 4)]
  test.F[3] <- test.F[1]/test.F[2]
  colnames(test.F)[1] <- "SS"
  colnames(test.F)[2] <- "df"
  colnames(test.F)[3] <- "MS"
  sumLm <- summary.lm(modele)
  sumLm$call <- cl
  sumLm$call$selection <- "none"
  sumLm$call[[1]] <- as.name("lm")
  sumLm$aic <- aic
  sumLm$bic <- bic
  test.T <- sumLm$coef
  cov.mat <- vcov(modele)
  facteurs.old <- facteurs <- as.list(rownames(attr(modele$terms, "factors"))[-1])
  for (i in 1:length(facteurs)){
    if (length(strsplit(facteurs[[i]]," ")[[1]])>1){
	  facteurs[[i]] <- strsplit(strsplit(facteurs[[i]]," ")[[1]][2],"]")[[1]] ## if variable with the name of the dataset
	  if (length(strsplit(facteurs[[i]],"\"")[[1]])>1) {    ## if variables are named
	    facteurs[[i]] <- strsplit(facteurs[[i]],"\"")[[1]][2]
	  } else {
		facteurs[[i]] <- as.integer(facteurs[[i]])
	  }
	}
	if (length(strsplit(facteurs[[i]],"`")[[1]])>1) facteurs[[i]] <- strsplit(facteurs[[i]],"`")[[1]][2]
  }
  interact <- NULL
  if (length(colnames(attr(modele$terms, "factors"))) > length(facteurs)) 
    interact <- colnames(attr(modele$terms, "factors"))[-(1:length(facteurs))]
  niveau <- list()
  for (i in 1:length(facteurs)) {
    if (is.factor(don[, facteurs[[i]]])) 
      niveau[[i]] <- paste(facteurs.old[[i]], levels(don[, facteurs[[i]]]),  sep = " - ")
    else niveau[[i]] <- facteurs.old[[i]]
  }
  res <- test.T[c(1, 1), ]
  iinit <- 2
  for (i in 1:length(facteurs)) {
    old.rownames <- rownames(res)
    if (is.factor(don[, facteurs[[i]]])) {
      indices <- iinit:(iinit + nlevels(don[, facteurs[[i]]]) - 2)
      dern.mod <- c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
             indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices,
			 indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 2)
      res <- rbind(res, test.T[indices, ], dern.mod)
      rownames(res) <- c(old.rownames, niveau[[i]])
      iinit <- iinit + nlevels(don[, facteurs[[i]]]) - 1
    } else {
      indices <- iinit
      res <- rbind(res, test.T[indices, ])
      rownames(res) <- c(old.rownames, niveau[[i]])
      iinit <- iinit + 1
    }
  }
  res <- res[-1, ]
  if (!is.null(interact)) {
    for (k in 1:length(interact)) {
      fact.int.old <- rownames(attr(modele$terms, "factors"))[which(attr(modele$terms, "factors")[, interact[k]] == 1)]
      old.rownames <- rownames(res)
      fact.int <- as.list(fact.int.old)
  for (i in 1:length(fact.int)){
    if (length(strsplit(fact.int[[i]]," ")[[1]])>1){
	  fact.int[[i]] <- strsplit(strsplit(fact.int[[i]]," ")[[1]][2],"]")[[1]] ## if variable with the name of the dataset
	  if (length(strsplit(fact.int[[i]],"\"")[[1]])>1) {    ## if variables are named
	    fact.int[[i]] <- strsplit(fact.int[[i]],"\"")[[1]][2]
	  } else {
		fact.int[[i]] <- as.integer(fact.int[[i]])
	  }
	}
	if (length(strsplit(fact.int[[i]],"`")[[1]])>1) fact.int[[i]] <- strsplit(fact.int[[i]],"`")[[1]][2]
  }

      fact1 <- fact.int[[1]]
      fact2 <- fact.int[[2]]

      iinit0 <- iinit
      if ((is.factor(don[, fact1])) & (is.factor(don[, fact2]))) {
        for (l in 1:(nlevels(don[, fact2]) - 1)) {
          indices <- iinit:(iinit + (nlevels(don[, fact1]) - 2))
          dern.mod <- c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                            indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices,
							indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
							indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 2)
          res <- rbind(res, test.T[indices, ], dern.mod)
          iinit <- iinit + (nlevels(don[, fact1]) - 1)
        }
        iinit <- iinit0
        for (l in 1:(nlevels(don[, fact1]) - 1)) {
          indices <- iinit + (nlevels(don[, fact1]) - 1) * (0:(nlevels(don[, fact2]) - 2))
          dern.mod <- c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                                  indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
								  indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
								  indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 2)
          res <- rbind(res, dern.mod)
          iinit <- iinit + 1
        }
        indices <- iinit0:(iinit0 + (nlevels(don[, fact1]) - 
                                      1) * (nlevels(don[, fact2]) - 1) - 1)
        dern.mod <- c(sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
		             indices])), sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices,
					 indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
					 indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 2)
        res <- rbind(res, dern.mod)
        iinit <- iinit0 + (nlevels(don[, fact1]) - 1) * 
          (nlevels(don[, fact2]) - 1)
        nom <- old.rownames
        aa <- paste(fact.int.old[2], levels(don[, fact2]), sep = " - ")
        for (i in 1:length(aa)) nom <- c(nom, paste(paste(fact.int.old[1], 
                levels(don[, fact1]), sep = " - "), aa[i], sep = " : "))
      }
      if ((is.factor(don[, fact1])) & (!is.factor(don[, fact2]))) {
        indices <- iinit:(iinit + (nlevels(don[, fact1]) - 2))
        dern.mod <- c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
		      indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
			  indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
			  indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 2)
        res <- rbind(res, test.T[indices, ], dern.mod)
        iinit <- iinit + (nlevels(don[, fact1]) - 1)
        nom <- c(old.rownames, paste(paste(fact1, levels(don[, fact1]), sep = " - "), fact2, sep = " : "))
      }
      if ((!is.factor(don[, fact1])) & (is.factor(don[, fact2]))) {
        indices <- iinit:(iinit + (nlevels(don[, fact2]) - 2))
        dern.mod <- c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
		       indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
			   indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
			   indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 2)
        res <- rbind(res, test.T[indices, ], dern.mod)
        iinit <- iinit + (nlevels(don[, fact2]) - 1)
        nom <- c(old.rownames, paste(paste(fact2, levels(don[, fact2]), sep = " - "), fact1, sep = " : "))
      }
      if ((!is.factor(don[, fact1])) & (!is.factor(don[, fact2]))) {
        indices <- iinit
        res <- rbind(res, test.T[indices, ])
        iinit <- iinit + 1
        nom <- c(old.rownames, paste(fact1, fact2, sep = " : "))
      }
      rownames(res) <- nom
    }
  }
  if (selection=="none") result <- list(Ftest = test.F, Ttest = res, call=cl, lmResult = sumLm)
  else result <- list(Ftest = test.F, Ttest = res, call=cl, lmResult = sumLm, callComp = clComp, lmResultComp = sumLmComp)
  class(result) <- "LinearModel"
  options(contrasts = old.contr)
  return(result)
}
