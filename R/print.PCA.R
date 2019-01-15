print.PCA <- function (x, file = NULL, sep = ";", ...){
    res.pca <- x
    if (!inherits(res.pca, "PCA")) stop("non convenient data")
    cat("**Results for the Principal Component Analysis (PCA)**\n")
    cat("The analysis was performed on", nrow(res.pca$call$X),
        "individuals, described by", ncol(res.pca$call$X), "variables\n")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(24, 2), list(1:24, c("name", "description")))
    res[1, ] <- c("$eig", "eigenvalues")
    res[2, ] <- c("$var", "results for the variables")
    res[3, ] <- c("$var$coord", "coord. for the variables")
    res[4, ] <- c("$var$cor", "correlations variables - dimensions")
    res[5, ] <- c("$var$cos2", "cos2 for the variables")
    res[6, ] <- c("$var$contrib", "contributions of the variables")
    res[7, ] <- c("$ind", "results for the individuals")
    res[8, ] <- c("$ind$coord", "coord. for the individuals")
    res[9, ] <- c("$ind$cos2", "cos2 for the individuals")
    res[10, ] <- c("$ind$contrib", "contributions of the individuals")
    indice <- 11
    if (!is.null(res.pca$ind.sup)){
      res[indice, ] <- c("$ind.sup", "results for the supplementary individuals")
      res[indice+1, ] <- c("$ind.sup$coord", "coord. for the supplementary individuals")
      res[indice+2, ] <- c("$ind.sup$cos2", "cos2 for the supplementary individuals")
      indice <- indice +3
    }
    if (!is.null(res.pca$quanti.sup)){
      res[indice, ] <- c("$quanti.sup", "results for the supplementary quantitative variables")
      res[indice+1, ] <- c("$quanti.sup$coord", "coord. for the supplementary quantitative variables")
      res[indice+2, ] <- c("$quanti.sup$cor", "correlations suppl. quantitative variables - dimensions")
      indice <- indice +3
    }
    if (!is.null(res.pca$quali.sup)){
      res[indice, ] <- c("$quali.sup", "results for the supplementary categorical variables")
      res[indice+1, ] <- c("$quali.sup$coord", "coord. for the supplementary categories")
      res[indice+2, ] <- c("$quali.sup$v.test", "v-test of the supplementary categories")
      indice <- indice +3
    }
    res[indice, ] <- c("$call", "summary statistics")
    res[indice+1, ] <- c("$call$centre", "mean of the variables")
    res[indice+2, ] <- c("$call$ecart.type", "standard error of the variables")
    res[indice+3, ] <- c("$call$row.w", "weights for the individuals")
    res[indice+4, ] <- c("$call$col.w", "weights for the variables")
    indice <- indice + 4
    print(res[1:indice,])
    if (!is.null(file)) {
      write.infile(res.pca,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
