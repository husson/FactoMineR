print.FAMD <- function (x, file = NULL, sep = ";", ...){
    res.FAMD <- x
    if (!inherits(res.FAMD, "FAMD")) stop("non convenient data")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(5, 2), list(1:5, c("name", "description")))
    res[1, ] <- c("$eig", "eigenvalues and inertia")
    res[2, ] <- c("$var", "Results for the variables")
    res[3, ] <- c("$ind", "results for the individuals")
    res[4, ] <- c("$quali.var", "Results for the qualitative variables")
    res[5, ] <- c("$quanti.var", "Results for the quantitative variables")
    print(res)
    if (!is.null(file)) {
      write.infile(res.FAMD,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
