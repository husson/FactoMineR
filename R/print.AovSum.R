print.AovSum<-function(x, ...){
  if (!inherits(x, "AovSum")) 
    stop("need to be a AovSum object")
  cat("Ftest\n")
  print(x$Ftest)
  x$Ttest[,4]<-round(x$Ttest[,4],5)
  cat("\nTtest\n")
  printCoefmat(x$Ttest)
}