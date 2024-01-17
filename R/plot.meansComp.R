utils::globalVariables(c("emmean","Effect","lower.CL","upper.CL","Letters"))
plot.meansComp <- function(x,...){
  if (!inherits(x,"meansComp")) stop("x must be a compMean object")
  aux <- summary(x$adjMean)
  if (ncol(aux)==6){
    rownames(aux) <- aux[,1]
  } else {
   rownames(aux) <- paste(aux[,1],aux[,2],sep=":")
   aux[,1] <- rownames(aux)
 }
#  rownames(aux) <- aux[,1]
  if (ncol(aux)==6){
    nameEffect <- colnames(aux)[1]
  } else {
    nameEffect <- paste(colnames(aux)[1],colnames(aux)[2],sep=":")
  }
  colnames(aux)[1] <- "Effect"
#  aux <- aux[order(aux[,2]),]
  aux <- aux[order(aux[,ncol(aux)-4]),]
  aux[,1] <- ordered(aux[,1],levels=aux[,1])
  aux$Letters <- x$groupComp$Letters

#if (graph.type==1){
  p <- ggplot2::ggplot(aux, ggplot2::aes(x=emmean, y=Effect,col=Letters),...) + 
    ggplot2::geom_errorbar(ggplot2::aes(xmin=lower.CL, xmax=upper.CL), width=.1) +
    ggplot2::geom_point() + ggplot2::theme(legend.position="none") +
    ggplot2::geom_text(ggplot2::aes(x=upper.CL+stats::offset(.1), y = Effect,label=Letters,col=Letters),hjust=0) +
	ggplot2::ylab(nameEffect) + ggplot2::xlab("Adjust mean")
#}

# if (graph.type==2){
  # p <- ggplot2::ggplot(aux, ggplot2::aes(x = emmean, y = Effect),...) + 
      # ggplot2::geom_point(size=2) + ggplot2::xlab("Adjust mean")+ggplot2::ylab(nameEffect)
  # for (i in 1:ncol(x$groupComp$LetterMatrix)) {
    # xlim <- aux[rownames(x$groupComp$LetterMatrix)[which(x$groupComp$LetterMatrix[,i])],2]
    # ylim <- which(x$groupComp$LetterMatrix[,i])
    # p <- p + ggplot2::annotate("segment", x=min(xlim),xend=max(xlim), y = min(ylim),yend=min(ylim),colour = palette()[i+1], size = 1.5)
    # for (k in ylim) {
      # p <- p + ggplot2::annotate("segment", x=aux[k,2],xend=aux[k,2], y = min(ylim),yend=k,colour = palette()[i+1], size = 1,linetype = "dashed")
      # p <- p + ggplot2::annotate("point", x=aux[k,2], y = min(ylim), colour = "black", size = 2, shape=21)
    # }
    # p <- p + ggplot2::annotate("point", x=xlim, y = ylim, colour = "black", size = 2)
    # p <- p + ggplot2::annotate("point", x=min(xlim), y = min(ylim), colour = palette()[i+1], size = 2)
  # }
# }
  return(p)
}
