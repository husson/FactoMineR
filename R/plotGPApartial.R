plotGPApartial <- function (x, axes = c(1, 2), 
    lab.ind.moy = TRUE, habillage = "ind", chrono = FALSE,
    draw.partial = NULL, xlim = NULL, ylim = NULL, cex = 1, title = NULL, palette=NULL, ...){
    
    res.gpa <- x
    if (!inherits(res.gpa, "GPA")) stop("non convenient data")
    if (is.null(palette)) palette(c("black","red","green3","blue",      "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
  
tab <- res.gpa$consensus[,axes]
if (is.null(draw.partial)) {
  draw.partial <- rep(FALSE,nrow(tab))
  names(draw.partial)<-rownames(tab)
}  
else draw.partial <- draw.partial[,1]
partial <- rownames(tab[draw.partial,])
if (length(partial)==0) partial <- NULL
disto <- matrix(0,nrow(tab),1)
rownames(disto) <- rownames(tab)

plot.GPA(res.gpa, axes = axes, lab.ind.moy = lab.ind.moy, habillage = habillage,
    xlim = xlim, ylim = ylim, chrono = chrono, cex = cex, title = title, partial = partial, palette=palette)
point.haut <- max(tab[,2])*1.2
if (!is.null(ylim)) point.haut <- ylim[2]
nbpoint <-0
while (nbpoint < 1000){
  pos <- locator(n=1)
  if (is.null(pos$y)) nbpoint = 1000
  else{ 
    for (i in 1:nrow(tab)) disto[i] <- (tab[i,1]-pos$x)^2+(tab[i,2]-pos$y)^2
    draw.partial[order(disto)[1]] <- !draw.partial[order(disto)[1]]
    partial <- rownames(tab)[draw.partial]
    if (length(partial)==0) partial <- NULL
    dev.off()
    plot.GPA(res.gpa, axes = axes, lab.ind.moy = lab.ind.moy, habillage = habillage,
      xlim = xlim, ylim = ylim, chrono = chrono, cex = cex, title = title, partial = partial, palette=palette)
    nbpoint = nbpoint+1
  }
}
return(as.data.frame(draw.partial))
}
