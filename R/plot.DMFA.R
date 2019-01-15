plot.DMFA <- function (x, axes = c(1, 2), choix = "ind", label = "all", 
    lim.cos2.var = 0., xlim=NULL, ylim=NULL, title = NULL,palette = NULL, new.plot = FALSE,
    autoLab = c("auto","yes","no"),	...) 
{
    res.dmfa = x
    autoLab <- match.arg(autoLab,c("auto","yes","no"))
	if (autoLab=="yes") autoLab=TRUE
	if (autoLab=="no") autoLab=FALSE
    class(res.dmfa) <- c("PCA", "list ")
    if (choix == "ind"){
	if(is.null(title)) titre = "Individuals factor map (PCA)"
	else titre = title
        plot.PCA(res.dmfa, habillage = 1, axes = axes, label = label, xlim = xlim, ylim = ylim,  
            autoLab = autoLab, title = titre,...)
    }
    if (choix == "quali") {
        if (length(res.dmfa$call$quali.sup$modalite) == 1) 
            stop("There is no supplementary qualitative variable")
        lev = levels(res.dmfa$call$X[, 1])
        ng = length(lev)
        nb.quali = (length(res.dmfa$call$quali.sup$modalite) - 1)/2
        xlim = 1.1 * c(min(res.dmfa$quali.sup$coord[, axes[1]]), 
            max(res.dmfa$quali.sup$coord[, axes[1]]))
        ylim = 1.1 * c(min(res.dmfa$quali.sup$coord[, axes[2]]), 
            max(res.dmfa$quali.sup$coord[, axes[2]]))
        lab.x <- paste("Dim ", axes[1], " (", signif(res.dmfa$eig[axes[1], 
            2], 4), " %)", sep = "")
        lab.y <- paste("Dim ", axes[2], " (", signif(res.dmfa$eig[axes[2], 
            2], 4), " %)", sep = "")
	  if(is.null(title)) titre = "Qualitative representation"
	  else titre = title
        if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
        if (is.null(palette)) palette(c("black", "red", "green3", "blue", "cyan", "magenta","darkgray", "darkgoldenrod", "darkgreen", "violet","turquoise", "orange", "lightpink", "lavender", "yellow","lightgreen", "lightgrey", "lightblue", "darkkhaki","darkmagenta", "darkolivegreen", "lightcyan", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen","darkslateblue", "darkslategray", "darkslategrey","darkturquoise", "darkviolet", "lightgray", "lightsalmon","lightyellow", "maroon"))
        plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, 
            xlim = xlim, ylim = ylim, col = "white", asp = 1, ...)
        abline(v = 0, lty = 2,...)
        abline(h = 0, lty = 2,...)
        for (i in 1:sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
            nb.quali)])) {
            points(res.dmfa$quali.sup$coord[i, axes[1]], res.dmfa$quali.sup$coord[i, 
                axes[2]], pch = 15,...)
            text(res.dmfa$quali.sup$coord[i, axes[1]], res.dmfa$quali.sup$coord[i, 
                axes[2]], rownames(res.dmfa$quali.sup$coord)[i], 
                pos = 3,...)
            for (j in 1:ng) {
                points(res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                  nb.quali)]) + ng * (i - 1) + j, axes[1]], res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                  nb.quali)]) + ng * (i - 1) + j, axes[2]], col = j + 
                  1, pch = 20,...)
                lines(c(res.dmfa$quali.sup$coord[i, axes[1]], 
                  res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                    nb.quali)]) + ng * (i - 1) + j, axes[1]]), 
                  c(res.dmfa$quali.sup$coord[i, axes[2]], res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                    nb.quali)]) + ng * (i - 1) + j, axes[2]]), 
                  col = j + 1,...)
            }
        }
        legend("topleft", legend = rownames(res.dmfa$group$coord), 
            text.col = 2:(1 + ng), cex = 0.8, bg = "white")
    }
    if (choix == "var") {
        lev = levels(res.dmfa$call$X[, 1])
        ng = length(lev)
	  if(is.null(title)) titre = "Variables factor map (PCA)"
	  else titre = title
      plot.PCA(res.dmfa, choix = "var", axes = axes, col.var = ng + 
            1, lim.cos2.var = lim.cos2.var, label = label, autoLab = autoLab, title = titre,...)
        for (j in 1:ng) {
            cor.partiel = res.dmfa$var.partiel[[j]][, axes]
            cor.cos2 = res.dmfa$cor.dim.gr[[j]][axes[1], axes[2]]
            for (v in 1:nrow(cor.partiel)) {
                qualite = (cor.partiel[v, 1]^2 + cor.partiel[v, 
                  2]^2)/sqrt(cor.partiel[v, 1]^2 + cor.partiel[v, 
                  2]^2 + 2 * cos(cor.cos2) * (cor.partiel[v, 
                  1] * cor.partiel[v, 2]))
                arrows(0, 0, cor.partiel[v, 1], cor.partiel[v, 
                  2], length = 0.1 * qualite, angle = 15, code = 2, 
                  col = j,...)
                if (abs(cor.partiel[v, 1]) > abs(cor.partiel[v, 
                  2])) {
                  if (cor.partiel[v, 1] >= 0) 
                    pos <- 4
                  else pos <- 2
                }
                else {
                  if (cor.partiel[v, 2] >= 0) 
                    pos <- 3
                  else pos <- 1
                }
                if (label == "all") 
                  text(cor.partiel[v, 1], y = cor.partiel[v, 
                    2], labels = rownames(cor.partiel)[v], pos = pos, 
                    col = j,...)
            }
        }
        legend("bottomleft", legend = c(lev, "var"), text.col = 1:(ng + 
            1), cex = 0.8, bg = "white")
        Xc = res.dmfa$Xc
        for (j in 1:ng) {
            auxil = res.dmfa$ind$coord[res.dmfa$call$X[, 1] == 
                lev[j], axes]
            prefpls(cbind(auxil, Xc[[j]][rownames(auxil), ]), 
                title = paste("Biplot between axes ", axes[1], 
                  " and ", axes[2], " for group ", lev[j], sep = ""))
        }
    }
    if (choix == "group") {
        if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
        if (is.null(palette)) palette(c("black", "red", "green3", "blue", "cyan", "magenta","darkgray", "darkgoldenrod", "darkgreen", "violet","turquoise", "orange", "lightpink", "lavender", "yellow","lightgreen", "lightgrey", "lightblue", "darkkhaki","darkmagenta", "darkolivegreen", "lightcyan", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen","darkslateblue", "darkslategray", "darkslategrey","darkturquoise", "darkviolet", "lightgray", "lightsalmon","lightyellow", "maroon"))
        coord.gr = res.dmfa$group$coord.n
        lev = levels(res.dmfa$call$X[, 1])
        ng = length(lev)
        xlim = 1.1 * c(0, max(1, max(coord.gr[, axes[1]])))
        ylim = 1.1 * c(0, max(1, max(coord.gr[, axes[2]])))
	  if(is.null(title)) titre = "Projection of the groups"
	  else titre = title
        plot(0, 0, xlab = paste("Dim", axes[1]), ylab = paste("Dim", 
            axes[2]), xlim = xlim, ylim = ylim, col = "white", 
            asp = 1, main = titre,...)
        for (j in 1:ng) {
            points(coord.gr[j, axes[1]], coord.gr[j, axes[2]], 
                col = j, pch = 15,...)
            if (label == "all") 
                text(coord.gr[j, axes[1]], coord.gr[j, axes[2]], 
                  labels = lev[j], pos = 3,...)
        }
        abline(v = 0, lty = 2,...)
        abline(h = 0, lty = 2,...)
    }
}