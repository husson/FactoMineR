meansComp <- function(object, spec, graph=TRUE, ...){
  if (inherits(object, "LinearModel")){
    formule <- object$lmResult$call
	if ("selection"%in%names(formule)) formule <- formule[-which(names(formule)%in%"selection")]
    object <- eval(formule)
  }
  if (!inherits(object,"lm") & !inherits(object,"aov")) print("object must be of class LinearModel, lm or aov")
  tuk <- emmeans::emmeans(object,spec=spec, ...)
  mat <- matrix(NA,length(summary(tuk)[,1]),length(summary(tuk)[,1]))
  rownames(mat)=colnames(mat)=summary(tuk)[,1]
  aux <- summary(emmeans::contrast(tuk,"pairwise"))$p.value
  mat[lower.tri(mat)]<- aux
  mat <- t(mat)
  mat[lower.tri(mat)]<- aux
  mat <- mat[order(summary(tuk)[,2]),order(summary(tuk)[,2])]
  res <- list(adjMean=tuk, groupComp=multcompView::multcompLetters(mat))
  class(res) <- "meansComp"
  if (graph) print(plot(res))
  return(res)
}
