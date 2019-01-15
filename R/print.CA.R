print.CA <- function (x, file = NULL, sep = ";", ...){
    res.ca <- x
    if (!inherits(res.ca, "CA")) stop("non convenient data")
    cat("**Results of the Correspondence Analysis (CA)**\n")
    cat("The row variable has ",nrow(res.ca$call$X)," categories; the column variable has", ncol(res.ca$call$X), "categories\n")
##    IT <- res.ca$eig[length(res.ca$eig), 3] * sum(res.ca$call$X)
    IT <- sum(res.ca$eig[, 1] )* sum(res.ca$call$X) 
    df <- (nrow(res.ca$call$X) - 1) * (ncol(res.ca$call$X) - 1)
    pc <- pchisq(IT, df = df,lower.tail = FALSE)
    cat("The chi square of independence between the two variables is equal to", IT, 
        "(p-value = ", pc, ").\n")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(17, 2), list(1:17, c("name", "description")))
    res[1, ] <- c("$eig", "eigenvalues")
    res[2, ] <- c("$col", "results for the columns")
    res[3, ] <- c("$col$coord", "coord. for the columns")
    res[4, ] <- c("$col$cos2", "cos2 for the columns")
    res[5, ] <- c("$col$contrib", "contributions of the columns")
    res[6, ] <- c("$row", "results for the rows")
    res[7, ] <- c("$row$coord", "coord. for the rows")
    res[8, ] <- c("$row$cos2", "cos2 for the rows")
    res[9, ] <- c("$row$contrib", "contributions of the rows")
    indice <- 10
    if (!is.null(res.ca$row.sup)){
      res[indice, ] <- c("$row.sup$coord", "coord. for supplementary rows")
      res[indice + 1, ] <- c("$row.sup$cos2", "cos2 for supplementary rows")
      indice <- indice + 2    
    }
    if (!is.null(res.ca$col.sup)){
      res[indice, ] <- c("$col.sup$coord", "coord. for supplementary columns")
      res[indice + 1, ] <- c("$col.sup$cos2", "cos2 for supplementary columns")
      indice <- indice + 2    
    }
    if (!is.null(res.ca$quanti.sup)){
      res[indice, ] <- c("$quanti.sup$coord", "coord. for supplementary continuous var.")
      res[indice+1, ] <- c("$quanti.sup$cos2", "cos2 for supplementary continuous var.")
      indice <- indice + 2    
    }
    if (!is.null(res.ca$quali.sup)){
      res[indice, ] <- c("$quali.sup$coord", "coord. for supplementary categorical var.")
      res[indice+1, ] <- c("$quali.sup$cos2", "cos2 for supplementary categorical var.")
      indice <- indice + 2    
    }
    res[indice, ] <- c("$call", "summary called parameters")
    res[indice + 1, ] <- c("$call$marge.col", "weights of the columns")
    res[indice + 2, ] <- c("$call$marge.row", "weights of the rows")
    indice <- indice + 2
    print(res[1:indice,])
    if (!is.null(file)) {
      write.infile(res.ca,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
