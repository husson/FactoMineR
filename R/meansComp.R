meansComp <- function(object, spec, graph=TRUE, ...){
  if (inherits(object, "LinearModel")){
    formule <- object$lmResult$call
	if ("selection"%in%names(formule)) formule <- formule[-which(names(formule)%in%"selection")]
    object <- eval(formule)
  }
  if (!inherits(object,"lm") & !inherits(object,"aov")) print("object must be of class LinearModel, lm or aov")
  tuk <- emmeans::emmeans(object,spec=as.formula(spec), ...)
  mat <- matrix(NA,nrow(summary(tuk)),nrow(summary(tuk)))
#    rownames(mat)=colnames(mat) <- summary(tuk)[,1]
  if (ncol(summary(tuk))==6){
    rownames(mat)=colnames(mat) <- summary(tuk)[,1]
  } else {
   rownames(mat)=colnames(mat) <- paste(summary(tuk)[,1],summary(tuk)[,2],sep=":")
 }
 aux <- summary(emmeans::contrast(tuk,"pairwise"))$p.value
  mat[lower.tri(mat)]<- aux
  mat <- t(mat)
  mat[lower.tri(mat)]<- aux
#  mat <- mat[order(summary(tuk)[,2]),order(summary(tuk)[,2])]
  mat <- mat[order(summary(tuk)[,ncol(summary(tuk))-4]),order(summary(tuk)[,ncol(summary(tuk))-4])]
  res <- list(adjMean=tuk, groupComp=multcompView::multcompLetters(mat))
  class(res) <- "meansComp"
  if (graph) print(plot(res))
  return(res)
}
