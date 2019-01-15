plotMFApartial <- function (x, axes = c(1, 2), 
    lab.ind = TRUE, lab.par = FALSE, habillage = "ind", chrono = FALSE,
    col.hab = NULL, invisible = NULL, draw.partial = NULL,
    xlim = NULL, ylim = NULL, cex = 1, title = NULL, palette=NULL, ...){
    
    res.mfa <- x
    if (!inherits(res.mfa, "MFA")) stop("non convenient data")
    if (is.null(palette)) palette(c("black","red","green3","blue",      "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
  
if (is.null(title)) title <- "Partial points graph"
tab <- res.mfa$ind$coord[,axes]
if (!is.null(res.mfa$ind.sup)) tab <- rbind.data.frame(tab,res.mfa$ind.sup$coord[,axes])
if (!is.null(res.mfa["quali.var"]$quali.var)) tab <- rbind.data.frame(tab,res.mfa$quali.var$coord[,axes])
if (!is.null(res.mfa$quali.var.sup)) tab <- rbind.data.frame(tab,res.mfa$quali.var.sup$coord[,axes])
if (is.null(draw.partial)) {
  draw.partial <- rep(FALSE,nrow(tab))
  names(draw.partial)<-rownames(tab)
}  
else draw.partial <- draw.partial[,1]
partial <- rownames(tab[draw.partial,])
if (length(partial)==0) partial <- NULL
disto <- matrix(0,nrow(tab),1)
rownames(disto) <- rownames(tab)

plot.MFA(res.mfa, axes = axes, lab.ind = lab.ind, lab.par = lab.par, habillage = habillage,
    col.hab = col.hab, invisible = invisible, xlim = xlim, ylim = ylim, chrono = chrono, cex = cex, title = title, partial = partial, palette=palette)
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
    plot.MFA(res.mfa, axes = axes, lab.ind = lab.ind, lab.par = lab.par, habillage = habillage,
      col.hab = col.hab, invisible = invisible, xlim = xlim, ylim = ylim, chrono = chrono, cex = cex, title = title, partial = partial, palette=palette)
    nbpoint = nbpoint+1
  }
}
return(as.data.frame(draw.partial))
}
