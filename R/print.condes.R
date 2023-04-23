print.condes<-function(x, ...){
  if (!inherits(x, "condes")) 
    stop("need to be a condes object")
  if (!is.null(x$quanti)){
  cat("\nLink between the variable and the continuous variables (R-square)\n=================================================================================\n")
    print(x$quanti)
  }
  if (!is.null(x$quali)){
  cat("\nLink between the variable and the categorical variable (1-way anova)\n=============================================\n")
    print(x$quali)
  }
  if (!is.null(x$category)){
  cat("\nLink between variable and the categories of the categorical variables\n================================================================\n")
    print(x$category)
  }
}