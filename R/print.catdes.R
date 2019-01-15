print.catdes<-function(x, ...){
  if (!inherits(x, "catdes")) 
    stop("need to be a catdes object")
  if (!is.null(x$test.chi2)){
  cat("\nLink between the cluster variable and the categorical variables (chi-square test)\n=================================================================================\n")
    print(x$test.chi2)
  }
  if (!is.null(x$test.chi2)){
  cat("\nDescription of each cluster by the categories\n=============================================\n")
    print(x$category)
  }
  if (!is.null(x$quanti.var)){
  cat("\nLink between the cluster variable and the quantitative variables\n================================================================\n")
    print(x$quanti.var)
  }
  if (!is.null(x$quanti)){
  cat("\nDescription of each cluster by quantitative variables\n=====================================================\n")
    print(x$quanti)
  }
}