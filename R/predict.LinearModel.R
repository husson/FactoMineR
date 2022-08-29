predict.LinearModel <- function(object,  newdata, 
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"), ...){
		
  if (inherits(object, "LinearModel")){
    formule <- object$lmResult$call
	if ("selection"%in%names(formule)) formule <- formule[-which(names(formule)%in%"selection")]
    object <- eval(formule)
  }
  res <- stats::predict.lm(object, newdata, 
        interval = interval, level = level, type = type, ...)
  return(res)
}
