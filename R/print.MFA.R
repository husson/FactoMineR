print.MFA <- function (x, file = NULL, sep = ";", ...){
    res.mfa <- x
    if (!inherits(res.mfa, "MFA")) stop("non convenient data")
    cat("**Results of the Multiple Factor Analysis (MFA)**\n")
    cat("The analysis was performed on", nrow(res.mfa$call$X),
        "individuals, described by", ncol(res.mfa$call$X), "variables\n")
    cat("*Results are available in the following objects :\n\n")
    res <- array("", c(22, 2), list(1:22, c("name", "description")))
    res[1, ] <- c("$eig", "eigenvalues")
    res[2, ] <- c("$separate.analyses", "separate analyses for each group of variables")
    res[3, ] <- c("$group", "results for all the groups")
    res[4, ] <- c("$partial.axes", "results for the partial axes")
    res[5, ] <- c("$inertia.ratio", "inertia ratio")
    res[6, ] <- c("$ind", "results for the individuals")
    indice <- 7
    if (!is.null(res.mfa["ind.sup"]$ind.sup)){
      res[indice, ] <- c("$ind.sup", "results for the supplementary individuals")
      indice <- indice + 1
    }
    if (!is.null(res.mfa["quanti.var"]$quanti.var)){
      res[indice, ] <- c("$quanti.var", "results for the quantitative variables")
      indice <- indice + 1
    }
    if (!is.null(res.mfa["quali.var"]$quali.var)){
      res[indice, ] <- c("$quali.var", "results for the categorical variables")
      indice <- indice + 1
    }
    if (!is.null(res.mfa["quanti.var.sup"]$quanti.var.sup)){
      res[indice, ] <- c("$quanti.var.sup", "results for the quantitative supplementary variables")
      indice <- indice + 1
    }
    if (!is.null(res.mfa["quali.var.sup"]$quali.var.sup)){
      res[indice, ] <- c("$quali.var.sup", "results for the categorical supplementary variables")
      indice <- indice + 1
    }

    if (!is.null(res.mfa["freq"]$freq)){
      res[indice, ] <- c("$freq", "results for the frequencies")
      indice <- indice + 1
    }
    if (!is.null(res.mfa["freq.sup"]$freq.sup)){
      res[indice, ] <- c("$freq.sup", "results for the supplementary frequencies")
      indice <- indice + 1
    }
	
	
    if (!is.null(res.mfa$quanti.var)){
      res[indice, ] <- c("$summary.quanti", "summary for the quantitative variables")
      indice <- indice + 1
    }
    if (!is.null(res.mfa$quali.var)){
      res[indice, ] <- c("$summary.quali", "summary for the categorical variables")
      indice <- indice + 1
    }
    res[indice, ] <- c("$global.pca", "results for the global PCA")
    print(res[1:indice,])
    if (!is.null(file)) {
      write.infile(res.mfa,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
