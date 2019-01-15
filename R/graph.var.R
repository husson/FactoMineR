graph.var <- function (x, axes = c(1, 2), 
    xlim = NULL, ylim = NULL, col.sup = "blue", 
    col.var = "black", draw="all", label=draw, lim.cos2.var = 0.1,
    cex = 1, title = NULL, new.plot = TRUE, ...){
    
    if ((!inherits(x, "MFA"))&(!inherits(x, "HMFA"))&(!inherits(x, "MCA"))&(!inherits(x, "PCA"))) stop("non convenient data")
    lab.var <- lab.quanti <- FALSE
    coord.quanti <- NULL
    if (inherits(x, "PCA")|inherits(x, "DMFA")){
      coord.var <- x$var$coord[, axes]
      var.cos2 = x$var$cos2[,axes]
      if (!is.null(x$quanti.sup)){
        coord.quanti <- x$quanti.sup$coord[, axes]
        quanti.cos2 = x$quanti.sup$cos2[,axes]
      }
    }
    if (inherits(x, "MCA")){
      if (!is.null(x$quanti.sup)){
        coord.quanti <- x$quanti.sup$cor[, axes]
        quanti.cos2 = x$quanti.sup$cos2[,axes]
      }
    }
    if (inherits(x, "MFA")|inherits(x, "HMFA")){
      if (!is.null(x["quanti.var"])){
        coord.var <- x$quanti.var$cor[, axes]
        var.cos2 = x$quanti.var$cos2[,axes]
      }
      if (!is.null(x$quanti.var.sup)){
        coord.quanti <- x$quanti.var.sup$cor[, axes]
        quanti.cos2 = x$quanti.var.sup$cos2[,axes]
      }
    }
    draw.var = lab.var <- rep(FALSE,nrow(coord.var))
    if (!is.null(coord.quanti)) draw.quanti = lab.quanti <- rep(FALSE,nrow(coord.quanti))
    if("all"%in%label){
      lab.var <- rep(TRUE,nrow(coord.var))
      if (!is.null(coord.quanti)) lab.quanti <- rep(TRUE,nrow(coord.quanti))
    }
    else {
      if("var" %in% label) lab.var = rep(TRUE,nrow(coord.var))
      else for (j in 1:nrow(coord.var)) if (rownames(coord.var)[j]%in%label) lab.var[j] = TRUE
      if (!is.null(coord.quanti)){
        if ("quanti.sup" %in% label) lab.quanti<- rep(TRUE,nrow(coord.quanti))
        else for (j in 1:nrow(coord.quanti)) if (rownames(coord.quanti)[j]%in%label) lab.quanti[j] = TRUE
      }
    }
    if("all"%in%draw){
      draw.var <- rep(TRUE,nrow(coord.var))
      if (!is.null(coord.quanti)) draw.quanti <- rep(TRUE,nrow(coord.quanti))
    }
    else {
      if("var" %in% draw) draw.var = rep(TRUE,nrow(coord.var))
      else for (j in 1:nrow(coord.var)) if (rownames(coord.var)[j]%in%draw) draw.var[j] = TRUE
      if (!is.null(coord.quanti)){
        if("quanti.sup" %in% draw) draw.quanti<- rep(TRUE,nrow(coord.quanti))
        else for (j in 1:nrow(coord.quanti)) if (rownames(coord.quanti)[j]%in%draw) draw.quanti[j] = TRUE
      }
    }

    cp1 <- round((x$eig[axes[1],2]), digits = 2)
    cp2 <- round((x$eig[axes[2],2]), digits = 2)
    lab.x <- paste("Dimension ",axes[1]," (",cp1,"%)",sep="")
    lab.y <- paste("Dimension ",axes[2]," (",cp2,"%)",sep="")
    plan <- cp1 + cp2
    sub.titre <- NULL
    if (is.null(title)) titre <- "Variables factor map (PCA)"
    else {
      titre <- title
      sub.titre  <- "Variables factor map (PCA)"
    }
    scale.unit <- TRUE
    if (inherits(x, "PCA")) scale.unit = x$call$scale.unit
    if (scale.unit)  xlim <- ylim <- c(-1, 1)
    else {
      xmin <- min(coord.var[, 1], coord.quanti[, 1])
      xmax <- max(coord.var[, 1], coord.quanti[, 1])
      ymin <- min(coord.var[, 2], coord.quanti[, 2])
      ymax <- max(coord.var[, 2], coord.quanti[, 2])
      xlim <- c(xmin, xmax) * 1.2
      ylim <- c(ymin, ymax) * 1.2
    }
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    if (scale.unit) {
      plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex, main=titre)
      title(sub = sub.titre, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle)
      lines(x.cercle, y = -y.cercle)
      abline(v=0,lty=2, cex=cex)
      abline(h=0,lty=2, cex=cex)
    }
    else {
      plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
      title(sub = sub.titre, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
      abline(v=0,lty=2)
      abline(h=0,lty=2)
    }
    col.var<-rep(col.var, length=nrow(coord.var))
    for (v in 1:nrow(coord.var)) {
      if (sum(var.cos2[v, ], na.rm = TRUE) < lim.cos2.var) draw.var[v]=FALSE
      if (draw.var[v]) {
        arrows(0, 0, coord.var[v, 1], coord.var[v, 2], length = 0.1, angle = 15, code = 2, col = col.var[v])
        if (lab.var[v]) {
          if (abs(coord.var[v,1])>abs(coord.var[v,2])){
            if (coord.var[v,1]>=0) pos<-4
            else pos<-2
          }
          else {
            if (coord.var[v,2]>=0) pos<-3
            else pos<-1
          }
          text(coord.var[v, 1], y = coord.var[v, 2], labels = rownames(coord.var)[v], pos = pos, cex=cex, col = col.var[v])
        }
      }
    }
    if (!is.null(coord.quanti)) {
      col.sup<-rep(col.sup, length=nrow(coord.quanti))
      for (q in 1:nrow(coord.quanti)) {
        if (sum(quanti.cos2[q, ], na.rm = TRUE) < lim.cos2.var) draw.quanti[q]=FALSE
        if (draw.quanti[q]) {
         arrows(0, 0, coord.quanti[q, 1], coord.quanti[q, 2], length = 0.1, angle = 15, code = 2, lty = 2, col=col.sup[q])
         if (lab.quanti[q]) {
          if (abs(coord.quanti[q,1])>abs(coord.quanti[q,2])){
            if (coord.quanti[q,1]>=0) pos<-4
            else pos<-2
          }
          else {
            if (coord.quanti[q,2]>=0) pos<-3
            else pos<-1
          }
          text(coord.quanti[q, 1], y = coord.quanti[q, 2], labels = rownames(coord.quanti)[q], pos = pos, col=col.sup[q])
         }
        }
      }
    }
    par(mar = c(5, 4, 4, 2) + 0.1)
}
