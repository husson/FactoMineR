print.LinearModel<-function(x, ...){
  if (!is.null(x$callComp)){
	cat("Results for the complete model:\n", sep = "")
	cat("==============================", sep = "")
	cat("\nCall:\n", paste(deparse(x$callComp), sep = "\n", collapse = "\n"), "\n", sep = "")
	sumLm <- x$lmResultComp
    cat("\nResidual standard error:", format(signif(sumLm$sigma,4)), "on", sumLm$df[2L], "degrees of freedom\n")
    if (!is.null(sumLm$fstatistic)) {
      cat("Multiple R-squared: ", formatC(sumLm$r.squared, digits = 4),
#	      ",\tAdjusted R-squared: ", formatC(sumLm$adj.r.squared, digits = 4), 
		  "\nF-statistic:", formatC(sumLm$fstatistic[1L],digits = 4), "on", sumLm$fstatistic[2L], "and", 
          sumLm$fstatistic[3L], "DF,  p-value:", formatC(pf(sumLm$fstatistic[1L], sumLm$fstatistic[2L], sumLm$fstatistic[3L], lower.tail = FALSE), digits = 4),
		  "\nAIC =", formatC(sumLm$aic,digits = 4),"   BIC =",formatC(sumLm$bic,digits = 4))
    }
    cat("\n\n")
	cat("Results for the model selected by ",toupper(x$call$selection)," criterion:\n", sep = "")
	cat("===============================================", sep = "")
  }
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
  sumLm <- x$lmResult
  cat("\nResidual standard error:", format(signif(sumLm$sigma,4)), "on", sumLm$df[2L], "degrees of freedom\n")
  if (!is.null(sumLm$fstatistic)) {
    cat("Multiple R-squared: ", formatC(sumLm$r.squared, digits = 4),
#     ",\tAdjusted R-squared: ", formatC(sumLm$adj.r.squared, digits = 4), 
      "\nF-statistic:", formatC(sumLm$fstatistic[1L],digits = 4), "on", sumLm$fstatistic[2L], "and", 
        sumLm$fstatistic[3L], "DF,  p-value:", formatC(pf(sumLm$fstatistic[1L], sumLm$fstatistic[2L], sumLm$fstatistic[3L], lower.tail = FALSE), digits = 4),
		"\nAIC =", formatC(sumLm$aic,digits = 4),"   BIC =",formatC(sumLm$bic,digits = 4))
  }
  cat("\n\n")
  if (!inherits(x, "LinearModel")) stop("need to be a LinearModel object")
  cat("Ftest\n")
  print(x$Ftest,signif.stars =FALSE)
  cat("\nTtest\n")
  printCoefmat(x$Ttest,signif.stars =FALSE)
}