plot.GPA<-function (x, axes = c(1, 2), lab.ind.moy = TRUE, 
    habillage = "ind", partial = "all", chrono = FALSE, xlim = NULL, 
    ylim = NULL, cex = 1, title = NULL, palette=NULL, ...) 
{
    res.gpa <- x
    if (!inherits(res.gpa, "GPA")) 
        stop("non convenient data")
    if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
    lab.x <- paste("Dim ", axes[1], sep = "")
    lab.y <- paste("Dim ", axes[2], sep = "")
    nb.ind <- nrow(res.gpa$consensus)
    coord.ind <- res.gpa$consensus[, axes]
    coord.ind.partiel <- res.gpa$Xfin[, axes, ]
    nbre.grpe <- dim(res.gpa$Xfin)[[3]]
    group.ind <- NULL
    if (!is.null(partial)) {
        if (length(partial) == 1) {
            if (partial == "all") 
                group.ind <- 1:nrow(coord.ind)
            else {
                for (i in 1:length(partial)) {
                  if (partial[i] %in% rownames(coord.ind)) 
                    group.ind <- c(group.ind, grep(partial[i], 
                      rownames(coord.ind)))
                }
            }
        }
        else {
            for (i in 1:length(partial)) {
                if (partial[i] %in% rownames(coord.ind)) 
                  group.ind <- c(group.ind, grep(partial[i], 
                    rownames(coord.ind)))
            }
        }
    }

    if (is.null(xlim)) {
        xmin <- min(coord.ind[, 1],na.rm=TRUE) * 1.1
        xmax <- max(coord.ind[, 1],na.rm=TRUE) * 1.1
        if (!is.null(group.ind)) {
            xmin <- min(xmin, coord.ind.partiel[group.ind, 1, 
                ],na.rm=TRUE)
            xmax <- max(xmax, coord.ind.partiel[group.ind, 1, 
                ],na.rm=TRUE)
        }
        xlim = c(xmin, xmax)
    }
    if (is.null(ylim)) {
        ymin <- min(coord.ind[, 2],na.rm=TRUE) * 1.1
        ymax <- max(coord.ind[, 2],na.rm=TRUE) * 1.1
        if (!is.null(group.ind)) {
            ymin <- min(ymin, coord.ind.partiel[group.ind, 2, 
                ],na.rm=TRUE)
            ymax <- max(ymax, coord.ind.partiel[group.ind, 2, 
                ],na.rm=TRUE)
        }
        ylim = c(ymin, ymax)
    }

    if (habillage == "group") {
        col.hab <- 2:(nbre.grpe + 1)
        col.ind <- c(rep("black", nb.ind), rep(col.hab, nb.ind))
    }
    if (habillage == "ind") {
        col.hab <- 1:nb.ind
        col.ind <- c(col.hab, rep(col.hab, each = nbre.grpe))
    }
    if (is.null(title)) title <- "General Procrustes Analysis map"

    plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, xlim = xlim, 
        ylim = ylim, col = "white", asp = 1, cex = cex)
    abline(v = 0, lty = 2, cex = cex)
    abline(h = 0, lty = 2, cex = cex)
    points(coord.ind, pch = 20, col = col.ind[1:nb.ind], cex = cex)

    if (lab.ind.moy) 
        text(coord.ind[, 1], y = coord.ind[, 2], labels = rownames(coord.ind), 
            pos = 3, col = col.ind[1:nb.ind])
    for (i in group.ind) {
        for (j in 1:nbre.grpe) {
            points(coord.ind.partiel[i, 1, j], coord.ind.partiel[i, 
                2, j], cex = 0.8 * cex, col = col.ind[nb.ind + 
                (i - 1) * nbre.grpe + j], pch = 20)
    
            if (chrono) {
                if (j > 1) 
                  lines(c(coord.ind.partiel[i, 1, j - 1], coord.ind.partiel[i, 
                    1, j]), c(coord.ind.partiel[i, 2, j - 1], 
                    coord.ind.partiel[i, 2, j]), col = col.ind[i])
            }
            else lines(c(coord.ind[i, 1], coord.ind.partiel[i, 
                1, j]), c(coord.ind[i, 2], coord.ind.partiel[i, 
                2, j]), col = col.ind[nb.ind + (i - 1) * nbre.grpe + 
                j], lty = j)
        }
    }
    if (habillage == "group") 
        legend("topleft", legend = rownames(res.gpa$RV), text.col = col.hab, 
            cex = 0.8)
}
