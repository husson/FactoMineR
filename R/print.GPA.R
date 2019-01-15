print.GPA<-function (x, file = NULL, sep = ";", ...) 
{
    res.gpa <- x
    if (!inherits(res.gpa, "GPA")) 
        stop("non convenient data")
    cat("**Results of the Generalized Procrustes Analysis (GPA)**\n\n")
    cat("There are", nrow(res.gpa$call$X), "individuals, characterized by", 
        ncol(res.gpa$call$X), "variables\n\n")
    cat("*Results are available in the following objects :\n\n")
    res <- array("", c(9, 2), list(1:9, c("name", "description")))
    res[1, ] <- c("$RV", "RV Coefficients between partial configurations")
    res[2, ] <- c("$RVs", "standardized RV Coefficients between partial configurations")
    res[3, ] <- c("$simi", "procrustes similarity indexes between partial configurations")
    res[4, ] <- c("$scaling", "isotropic scaling factors")
    res[5, ] <- c("$dep", "PCA of initial configuration ")
    res[6, ] <- c("$consensus", "coordinates of the consensus configuration")
    res[7, ] <- c("$Xfin", "coordinates of partial configurations")
    res[8, ] <- c("$PANOVA", "list of Procrustes Analysis of Variance tables")
    res[9, ] <- c("$correlation", "Correlations by sets")
    print(res[1:9, ])
    if (!is.null(file)) {
        write.infile(res.gpa, file = file, sep = sep)
        print(paste("All the results are in the file", file))
    }
}
