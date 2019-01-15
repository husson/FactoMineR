print.MCA <- function (x, file = NULL, sep = ";", ...){
    res.mca <- x
    if (!inherits(res.mca, "MCA")) stop("non convenient data")
    cat("**Results of the Multiple Correspondence Analysis (MCA)**\n")
    cat("The analysis was performed on", nrow(res.mca$call$X),
        "individuals, described by", ncol(res.mca$call$X), "variables\n")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(22, 2), list(1:22, c("name", "description")))
    res[1, ] <- c("$eig", "eigenvalues")
    res[2, ] <- c("$var", "results for the variables")
    res[3, ] <- c("$var$coord", "coord. of the categories")
    res[4, ] <- c("$var$cos2", "cos2 for the categories")
    res[5, ] <- c("$var$contrib", "contributions of the categories")
    res[6, ] <- c("$var$v.test", "v-test for the categories")
    res[7, ] <- c("$ind", "results for the individuals")
    res[8, ] <- c("$ind$coord", "coord. for the individuals")
    res[9, ] <- c("$ind$cos2", "cos2 for the individuals")
    res[10, ] <- c("$ind$contrib", "contributions of the individuals")
    indice <- 11
    if (!is.null(res.mca$ind.sup)){
      res[indice, ] <- c("$ind.sup", "results for the supplementary individuals")
      res[indice+1, ] <- c("$ind.sup$coord", "coord. for the supplementary individuals")
      res[indice+2, ] <- c("$ind.sup$cos2", "cos2 for the supplementary individuals")
      indice <- indice +3
    }
    if (!is.null(res.mca$quanti.sup)){
      res[indice, ] <- c("$quanti.sup", "results for the supplementary quantitative variables")
      res[indice+1, ] <- c("$quanti.sup$coord", "coord. of the supplementary quantitative variables")
      indice <- indice +2
    }
    if (!is.null(res.mca$quali.sup)){
      res[indice, ] <- c("$quali.sup", "results for the supplementary categorical variables")
      res[indice+1, ] <- c("$quali.sup$coord", "coord. for the supplementary categories")
      res[indice+2, ] <- c("$quali.sup$cos2", "cos2 for the supplementary categories")
      res[indice+3, ] <- c("$quali.sup$v.test", "v-test for the supplementary categories")
      indice <- indice +4
    }
    res[indice, ] <- c("$call", "intermediate results")
    res[indice +1, ] <- c("$call$marge.col", "weights of columns")
    res[indice+2, ] <- c("$call$marge.li", "weights of rows")
    indice <- indice + 2
    print(res[1:indice,])
    if (!is.null(file)) {
      write.infile(res.mca,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
