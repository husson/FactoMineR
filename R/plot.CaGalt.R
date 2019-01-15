plot.CaGalt<-function(x, axes = c(1, 2), choix = c("ind", "freq", "quali.var", "quanti.var"), conf.ellip = FALSE, contr.ellipse = 3, xlim = NULL, ylim = NULL, col.ind = "black", col.freq = "darkred", col.quali = "blue", col.quanti = "blue", label = TRUE, lim.cos2.var = 0, title = NULL, palette = NULL, autoLab = c("auto", "yes", "no"), new.plot = FALSE, select = NULL, unselect = 0.7, shadowtext = FALSE, ...){
	res.cagalt <- x
	if (!inherits(res.cagalt, "CaGalt")) stop("non convenient data")
	if (is.numeric(unselect)) if ((unselect > 1) | (unselect < 0)) stop("unselect should be between 0 and 1")
	autoLab <- match.arg(autoLab, c("auto", "yes", "no"))
	if (autoLab == "yes") autoLab = TRUE
	if (autoLab == "no") autoLab = FALSE
	choix <- match.arg(choix, c("ind", "freq", "quali.var", "quanti.var"))
	lab.x <- paste("Dim ", axes[1], " (", format(res.cagalt$eig[axes[1], 2], nsmall = 2, digits = 2), "%)", sep = "")
	lab.y <- paste("Dim ", axes[2], " (", format(res.cagalt$eig[axes[2], 2], nsmall = 2, digits = 2), "%)", sep = "")
	if (choix == "ind") {
		if (is.null(title)) titre <- "Individuals factor map (CaGalt)"
		else titre <- title
		coord.ind <- res.cagalt$ind$coord[, axes, drop = FALSE]
      	if (is.null(xlim)) xlim <- c(min(coord.ind[,1]),max(coord.ind[,1])) * 1.2
		if (is.null(ylim)) ylim <- c(min(coord.ind[,2]),max(coord.ind[,2])) * 1.2
		selection <- NULL
		if (!is.null(select)) {
			if (mode(select) == "numeric") selection <- select
			else {
				if (sum(rownames(res.cagalt$ind$coord) %in% select) != 0) selection <- which(rownames(res.cagalt$ind$coord) %in% select)
				else {
                			if (grepl("coord", select)) selection <- (rev(order(apply(res.cagalt$ind$coord[, axes]^2, 1, sum))))[1:min(nrow(res.cagalt$ind$coord), sum(as.integer(unlist(strsplit(select, "coord"))), na.rm = T))]
                  			if (grepl("cos2", select)) {
						if (sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T) >= 1) selection <- (rev(order(apply(res.cagalt$ind$cos2[, axes], 1, sum))))[1:min(nrow(res.cagalt$ind$cos2), sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))]
						else selection <- which(apply(res.cagalt$ind$cos2[, axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))
                  			}
					if (is.integer(select)) selection <- select
                		}
       			}
		}
		if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new(width = min(14, 8 * (xlim[2] - xlim[1])/(ylim[2] - ylim[1])), height = 8)
      		if (is.null(palette)) palette(c("black", "red", "green3", "blue", "cyan", "magenta", "darkgray", "darkgoldenrod", "darkgreen", "violet", "turquoise", "orange", "lightpink", "lavender", "yellow", "lightgreen", "lightgrey", "lightblue", "darkkhaki", "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "lightgray", "lightsalmon", "lightyellow", "maroon"))
		plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp = 1, ...)
		abline(v = 0, lty = 2, ...)
		abline(h = 0, lty = 2, ...)
		coo <- labe <- coll <- ipch <- fonte <- NULL
		coo <- coord.ind
      		if (label) labe <- rownames(coord.ind)
      		else labe <- rep("", nrow(coord.ind))
		coll <- rep(col.ind, nrow(coord.ind))
		ipch <- rep(20, nrow(coord.ind))
		fonte <- rep(1, nrow(coord.ind))
		if (!is.null(selection)) {
			if (is.numeric(unselect)) coll[!((1:length(coll)) %in% selection)] = rgb(t(col2rgb(coll[!((1:length(coll)) %in% selection)])), alpha = 255 * (1 - unselect), maxColorValue = 255)
			else coll[!((1:length(coll)) %in% selection)] = unselect
      			labe[!((1:length(coll)) %in% selection)] <- ""
 		}
		if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
		if (any(labe != "")) {
			if (autoLab == "auto") autoLab = (length(which(labe != "")) < 50)
			if (autoLab == TRUE) autoLab(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], col = coll[labe != ""], font = fonte[labe != ""], shadotext = shadowtext, ...)
            		if (autoLab == FALSE) text(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], col = coll[labe != ""], font = fonte[labe != ""], pos = 3, ...)
		}
		if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
	}
	if (choix == "freq") {
		if (is.null(title)) titre <- "Frequencies factor map (CaGalt)"
		else titre <- title
		coord.freq <- res.cagalt$freq$coord[, axes, drop = FALSE]
      		if (is.null(xlim)) xlim <- c(min(coord.freq[,1]),max(coord.freq[,1])) * 1.2
		if (is.null(ylim)) ylim <- c(min(coord.freq[,2]),max(coord.freq[,2])) * 1.2
		selection <- NULL
		if (!is.null(select)) {
			if (mode(select) == "numeric") selection <- select
			else {
				if (sum(rownames(res.cagalt$freq$coord) %in% select) != 0) selection <- which(rownames(res.cagalt$freq$coord) %in% select)
				else {
					if (grepl("contrib", select)) selection <- (rev(order(res.cagalt$freq$contrib[, axes[1], drop = FALSE] * res.cagalt$eig[axes[1], 1] + res.cagalt$freq$contrib[, axes[2], drop = FALSE] * res.cagalt$eig[axes[2], 1])))[1:min(nrow(res.cagalt$freq$contrib), sum(as.integer(unlist(strsplit(select, "contrib"))), na.rm = T))]
                			if (grepl("coord", select)) selection <- (rev(order(apply(res.cagalt$freq$coord[, axes]^2, 1, sum))))[1:min(nrow(res.cagalt$freq$coord), sum(as.integer(unlist(strsplit(select, "coord"))), na.rm = T))]
                  			if (grepl("cos2", select)) {
						if (sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T) >= 1) selection <- (rev(order(apply(res.cagalt$freq$cos2[, axes], 1, sum))))[1:min(nrow(res.cagalt$freq$cos2), sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))]
						else selection <- which(apply(res.cagalt$freq$cos2[, axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))
                  			}
					if (is.integer(select)) selection <- select
                		}
       			}
		}
		if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new(width = min(14, 8 * (xlim[2] - xlim[1])/(ylim[2] - ylim[1])), height = 8)
      		if (is.null(palette)) palette(c("black", "red", "green3", "blue", "cyan", "magenta", "darkgray", "darkgoldenrod", "darkgreen", "violet", "turquoise", "orange", "lightpink", "lavender", "yellow", "lightgreen", "lightgrey", "lightblue", "darkkhaki", "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "lightgray", "lightsalmon", "lightyellow", "maroon"))
		plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp = 1, ...)
		abline(v = 0, lty = 2, ...)
		abline(h = 0, lty = 2, ...)
		coo <- labe <- coll <- ipch <- fonte <- NULL
		coo <- coord.freq
      		if (label) labe <- rownames(coord.freq)
      		else labe <- rep("", nrow(coord.freq))
		coll <- rep(col.freq, nrow(coord.freq))
		ipch <- rep(15, nrow(coord.freq))
		fonte <- rep(1, nrow(coord.freq))
		if (!is.null(selection)) {
			if (is.numeric(unselect)) coll[!((1:length(coll)) %in% selection)] = rgb(t(col2rgb(coll[!((1:length(coll)) %in% selection)])), alpha = 255 * (1 - unselect), maxColorValue = 255)
			else coll[!((1:length(coll)) %in% selection)] = unselect
      			labe[!((1:length(coll)) %in% selection)] <- ""
 		}
		if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
		if (any(labe != "")) {
			if (autoLab == "auto") autoLab = (length(which(labe != "")) < 50)
			if (autoLab == TRUE) autoLab(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], col = coll[labe != ""], font = fonte[labe != ""], shadotext = shadowtext, ...)
            		if (autoLab == FALSE) text(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], col = coll[labe != ""], font = fonte[labe != ""], pos = 3, ...)
		}
		if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
		if (conf.ellip) {
      			sel.freq<-which(res.cagalt$freq$contr[,axes[1]]>contr.ellipse*mean(res.cagalt$freq$contr[,1])|res.cagalt$freq$contr[,2]>contr.ellipse*mean(res.cagalt$freq$contr[,axes[2]]))
			for(i in 1:length(sel.freq)){
				coord.ellip=coord.ellipse(cbind.data.frame(res.cagalt$ellip$freq[which(res.cagalt$ellip$freq$FREQ%in%names(sel.freq))[i],ncol(res.cagalt$freq$contr)+1],res.cagalt$ellip$freq[which(res.cagalt$ellip$freq$FREQ%in%names(sel.freq)[i]),1:ncol(res.cagalt$freq$contr)]),bary=FALSE,axes=axes)
				lines(coord.ellip$res[,2],coord.ellip$res[,3],lty=2,lwd=2,col="black")
			}
		}
	}
	if (choix == "quanti.var") {
		if (is.null(res.cagalt$quanti.var)) stop("Variables are not quantitative")
		if (is.null(title)) titre <- "Variables factor map (CaGalt)"
		else titre <- title
		selection <- NULL
		if (!is.null(select)) {
			if (mode(select) == "numeric") selection <- select
            		else {
				if (sum(rownames(res.cagalt$quanti.var$coord) %in% select) != 0) selection <- which(rownames(res.cagalt$quanti.var$coord) %in% select)
                		else {
                  			if (grepl("coord", select)) selection <- (rev(order(apply(res.cagalt$quanti.var$coord[, axes]^2, 1, sum))))[1:min(nrow(res.cagalt$var$coord), sum(as.integer(unlist(strsplit(select, "coord"))), na.rm = T))]
                  			if (grepl("cos2", select)) {
                    				if (sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T) >= 1) selection <- (rev(order(apply(res.cagalt$quanti.var$cos2[, axes], 1, sum))))[1:min(nrow(res.cagalt$quanti.var$cos2), sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))]
                    				else selection <- which(apply(res.cagalt$quanti.var$cos2[, axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))
                  			}
                  			if (is.integer(select)) selection <- select
                		}
     			}
		}
		coord.var <- res.cagalt$quanti.var$cor[, axes, drop = FALSE]
		xlim <- ylim <- c(-1, 1)
		if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
      		if (is.null(palette)) palette(c("black", "red", "green3", "blue", "cyan", "magenta", "darkgray", "darkgoldenrod", "darkgreen", "violet", "turquoise", "orange", "lightpink", "lavender", "yellow", "lightgreen", "lightgrey", "lightblue", "darkkhaki", "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "lightgray", "lightsalmon", "lightyellow", "maroon"))
		plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp = 1, main = titre, ...)
		x.cercle <- seq(-1, 1, by = 0.01)
		y.cercle <- sqrt(1 - x.cercle^2)
		lines(x.cercle, y = y.cercle, ...)
		lines(x.cercle, y = -y.cercle, ...)
		abline(v = 0, lty = 2, ...)
		abline(h = 0, lty = 2, ...)
		coll <- coo <- labe <- posi <- NULL
		if (!is.null(coord.var[which(apply(res.cagalt$quanti.var$cos2[, axes, drop = FALSE], 1, sum, na.rm = TRUE) >= lim.cos2.var), ]) & (nrow(coord.var) > 0)) {
			coord.var <- coord.var[which(apply(res.cagalt$quanti.var$cos2[, axes, drop = FALSE], 1, sum, na.rm = TRUE) >= lim.cos2.var), , drop = FALSE]
            		coo <- coord.var
            		coll <- rep(col.quanti, nrow(coord.var))
            		if (label) labe <- rownames(coord.var)
            		else labe <- rep("", nrow(coord.var))
            		if (!is.null(selection)) {
                		if (is.numeric(unselect)) coll[!((1:length(coll)) %in% selection)] = rgb(t(col2rgb(coll[!((1:length(coll)) %in% selection)])), alpha = 255 * (1 - unselect), maxColorValue = 255)
                		else coll[!((1:length(coll)) %in% selection)] = unselect
                		labe[!((1:length(coll)) %in% selection)] <- ""
            		}
            		for (v in 1:nrow(coord.var)) {
				arrows(0, 0, coord.var[v, 1], coord.var[v, 2], length = 0.1, angle = 15, code = 2, col = coll[v])
                		if (label) {
                  			if (abs(coord.var[v, 1]) > abs(coord.var[v, 2])) {
                    				if (coord.var[v, 1] >= 0) posi <- c(posi, 4)
                    				else posi <- c(posi, 2)
                  			}else{
                    				if (coord.var[v, 2] >= 0) posi <- c(posi, 3)
                    				else posi <- c(posi, 1)
					}
            			}
			}
		}
		if (any(labe != "")) {
			if (autoLab == "auto") autoLab = (length(which(labe != "")) < 50)
            		if (autoLab == FALSE) text(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], pos = posi[labe != ""], col = coll[labe != ""], ...)
            		if (autoLab == TRUE) autoLab(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], col = coll[labe != ""], shadotext = shadowtext, ...)
		}
		if (conf.ellip) {
      			for(i in 1:nrow(res.cagalt$quanti.var$coord)){
				coord.ellip=coord.ellipse(cbind.data.frame(res.cagalt$ellip$var[which(res.cagalt$ellip$var$VAR%in%rownames(res.cagalt$quanti.var$coord)[i]),ncol(res.cagalt$quanti.var$coord)+1],res.cagalt$ellip$var[which(res.cagalt$ellip$var$VAR%in%rownames(res.cagalt$quanti.var$coord)[i]),1:ncol(res.cagalt$quanti.var$coord)]),bary=FALSE,axes=axes)
				lines(coord.ellip$res[,2],coord.ellip$res[,3],lty=2,lwd=2,col="black")
			}
		}
	}
	if (choix == "quali.var") {
		if (is.null(res.cagalt$quali.var)) stop("Variables are not categorical")
		coord.var <- res.cagalt$quali.var$coord[, axes]
 		if (is.null(xlim)) xlim <- c(min(coord.var[,1]),max(coord.var[,1])) * 1.2
		if (is.null(ylim)) ylim <- c(min(coord.var[,2]),max(coord.var[,2])) * 1.2
		selection <- NULL
		if (!is.null(select)) {
            		if (mode(select) == "numeric") selection <- select
            		else {
				if (sum(rownames(res.cagalt$quali.var$coord) %in% select)!= 0) selection <- which(rownames(res.cagalt$quali.var$coord) %in% select)
                		else {
					if (grepl("coord", select)) selection <- (rev(order(apply(res.cagalt$quali.var$coord[, axes, drop = FALSE]^2, 1, sum))))[1:min(nrow(res.cagalt$quali.var$coord), sum(as.integer(unlist(strsplit(select, "coord"))), na.rm = T))]
					if (grepl("cos2", select)) {
                    				if (sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T) >= 1) selection <- (rev(order(apply(res.cagalt$quali.var$cos2[, axes], 1, sum))))[1:min(nrow(res.cagalt$quali.var$cos2), sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))]
                    				else selection <- which(apply(res.cagalt$quali.var$cos2[, axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, "cos2"))), na.rm = T))
                  			}
					if (is.integer(select)) selection <- select
				}
			}
		}
		titre = title
		if (is.null(title)) titre <- "Categories factor map (CaGalt)"
		if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new(width = min(14, max(8, 8 * (xlim[2] - xlim[1])/(ylim[2] - ylim[1]))), height = 8)
      		if (is.null(palette)) palette(c("black", "red", "green3", "blue", "cyan", "magenta", "darkgray", "darkgoldenrod", "darkgreen", "violet", "turquoise", "orange", "lightpink", "lavender", "yellow", "lightgreen", "lightgrey", "lightblue", "darkkhaki", "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "lightgray", "lightsalmon", "lightyellow", "maroon"))
		plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp = 1, ...)
		abline(v = 0, lty = 2, ...)
		abline(h = 0, lty = 2, ...)
		coo <- labe <- coll <- ipch <- fonte <- NULL
		coo <- coord.var
		if (label) labe <- rownames(coord.var)
		else labe <- rep("", nrow(coord.var))
		coll <- rep(col.quali, nrow(coord.var))
		if (!is.null(select)) {
			if (is.numeric(unselect)) coll[!((1:length(coll)) %in% selection)] = rgb(t(col2rgb(coll[!((1:length(coll)) %in% selection)])), alpha = 255 * (1 - unselect), maxColorValue = 255)
			else coll[!((1:length(coll)) %in% selection)] = unselect
			labe[!((1:length(labe)) %in% selection)] <- ""
		}
		ipch <- rep(17, nrow(coord.var))
		fonte <- rep(1, nrow(coord.var))
		if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
		if (any(labe != "")) {
			if (autoLab == "auto") autoLab = (length(which(labe != "")) < 50)
            		if (autoLab == TRUE) autoLab(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], col = coll[labe != ""], font = fonte[labe != ""], shadotext = shadowtext, ...)
            		if (autoLab == FALSE) text(coo[labe != "", 1], y = coo[labe != "", 2], labels = labe[labe != ""], col = coll[labe != ""], font = fonte[labe != ""], pos = 3, ...)
		}
		if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
		if (conf.ellip) {
      			for(i in 1:nrow(res.cagalt$quali.var$coord)){
				coord.ellip=coord.ellipse(cbind.data.frame(res.cagalt$ellip$var[which(res.cagalt$ellip$var$VAR%in%rownames(res.cagalt$quali.var$coord)[i]),ncol(res.cagalt$quali.var$coord)+1],res.cagalt$ellip$var[which(res.cagalt$ellip$var$VAR%in%rownames(res.cagalt$quali.var$coord)[i]),1:ncol(res.cagalt$quali.var$coord)]),bary=FALSE,axes=axes)
				lines(coord.ellip$res[,2],coord.ellip$res[,3],lty=2,lwd=2,col="black")
			}
		}
	}
}