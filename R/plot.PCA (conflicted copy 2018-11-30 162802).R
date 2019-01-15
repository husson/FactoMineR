plot.PCA <- function (x, axes = c(1, 2), choix = c("ind","var","varcor"),
    ellipse = NULL, xlim = NULL, ylim = NULL, habillage = "none", 
    col.hab = NULL, col.ind = "black", col.ind.sup = "blue", 
    col.quali = "magenta", col.quanti.sup = "blue", 
    col.var = "black", label=c("all","none","ind", "ind.sup", "quali", "var", "quanti.sup"), 
	invisible = c("none","ind", "ind.sup", "quali","var", "quanti.sup"), lim.cos2.var = 0.,
    title = NULL, palette=NULL, autoLab=c("auto","yes","no"),new.plot=FALSE, 
	select=NULL, unselect = 0.7,shadowtext = FALSE, legend = list(bty = "y", x = "topleft"), ...){
    
    res.pca <- x
    if (!inherits(res.pca, "PCA")) stop("non convenient data")
    if (is.numeric(unselect)) if ((unselect>1)|(unselect<0)) stop("unselect should be betwwen 0 and 1")
	autoLab <- match.arg(autoLab,c("auto","yes","no"))
	if (autoLab == "yes") autoLab=TRUE
	if (autoLab == "no") autoLab=FALSE
    label <- match.arg(label,c("all","none","ind", "ind.sup", "quali", "var", "quanti.sup"),several.ok=TRUE)
	invisible <- match.arg(invisible,c("none","ind", "ind.sup", "quali","var", "quanti.sup"),several.ok=TRUE)
    if ("none"%in%invisible) invisible = NULL
    choix <- match.arg(choix,c("ind","var","varcor"))
    lab.ind <- lab.quali <- lab.var <- lab.quanti <- lab.ind.sup <- FALSE
    if (length(label)==1 && label=="all") lab.ind <- lab.quali <- lab.var <- lab.quanti <- lab.ind.sup <-TRUE
    if ("ind" %in% label) lab.ind<-TRUE
    if ("quali" %in% label) lab.quali<-TRUE
    if ("var" %in% label) lab.var<-TRUE
    if ("quanti.sup" %in% label) lab.quanti<-TRUE
    if ("ind.sup" %in% label) lab.ind.sup<-TRUE
    lab.x <- paste("Dim ",axes[1]," (",format(res.pca$eig[axes[1],2],nsmall=2,digits=2),"%)",sep="")
    lab.y <- paste("Dim ",axes[2]," (",format(res.pca$eig[axes[2],2],nsmall=2,digits=2),"%)",sep="")
#    sub.titre <- NULL
    if (choix == "ind") {
        if (is.null(title)) titre <- "Individuals factor map (PCA)"
        else titre <- title

        coord.actif <- res.pca$ind$coord[, axes,drop=FALSE]
        coord.illu <- coord.quali <- coord.ellipse <- NULL
        if (!is.null(res.pca$ind.sup)) coord.illu <- res.pca$ind.sup$coord[, axes,drop=FALSE]
        if (!is.null(res.pca$quali.sup))  coord.quali <- res.pca$quali.sup$coord[, axes,drop=FALSE]
        if (!is.null(ellipse))  coord.ellipse <- ellipse$res

        test.invisible <- vector(length = 2)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("ind", invisible)
            test.invisible[2] <- match("ind.sup", invisible)
            test.invisible[3] <- match("quali", invisible)
        }
        else  test.invisible <- rep(NA, 3)
        if (is.null(xlim)) {
          xmin <- xmax <- 0
          if(is.na(test.invisible[1])) xmin <- min(xmin, coord.actif[,1])
          if(is.na(test.invisible[1])) xmax <- max(xmax, coord.actif[,1])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) xmin <- min(xmin, coord.illu[, 1])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) xmax <- max(xmax, coord.illu[, 1])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) xmin <- min(xmin, coord.quali[, 1])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) xmax <- max(xmax, coord.quali[, 1])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) xmin <- min(xmin, coord.ellipse[, 2])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) xmax <- max(xmax, coord.ellipse[, 2])
          xlim <- c(xmin, xmax) * 1.2
        }
        else {
          xmin = xlim[1]
          xmax = xlim[2]
        }
        if (is.null(ylim)) {
          ymin <- ymax <- 0
          if(is.na(test.invisible[1])) ymin <- min(ymin, coord.actif[,2])
          if(is.na(test.invisible[1])) ymax <- max(ymax, coord.actif[,2])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) ymin <- min(ymin, coord.illu[, 2])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) ymax <- max(ymax, coord.illu[, 2])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) ymin <- min(ymin, coord.quali[, 2])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) ymax <- max(ymax, coord.quali[, 2])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) ymin <- min(ymin, coord.ellipse[, 3])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) ymax <- max(ymax, coord.ellipse[, 3])
          ylim <- c(ymin, ymax) * 1.2
        }
        else {
          ymin = ylim[1]
          ymax = ylim[2]
        }
        selection <- NULL
		if (!is.null(select)) {
		  if (mode(select)=="numeric") selection <- select
		  else {
		    if (sum(rownames(res.pca$ind$coord)%in%select)+sum(rownames(res.pca$ind.sup$coord)%in%select)!=0) selection <- which(rownames(res.pca$ind$coord)%in%select)
			else {
 		    if (grepl("contrib",select)) selection <- (rev(order(res.pca$ind$contrib[,axes[1],drop=FALSE]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2],drop=FALSE]*res.pca$eig[axes[2],1])))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
# 		    if (grepl("contrib",select)) selection <- (rev(order(apply(res.pca$ind$contrib[,axes],1,sum))))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
 		    if (grepl("dist",select)) selection <- (rev(order(res.pca$ind$dist)))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"dist"))),na.rm=T))]
 		    if (grepl("coord",select)) selection <- (rev(order(apply(res.pca$ind$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
 		    if (grepl("cos2",select)) {
			  if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.pca$ind$cos2[,axes],1,sum))))[1:min(nrow(res.pca$ind$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
			  else selection <- which(apply(res.pca$ind$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
			}
			if (is.integer(select)) selection <- select
			}  
		  }
		}
        selectionS <- NULL
		if ((!is.null(select))&(!is.null(res.pca$ind.sup$coord))&is.na(test.invisible[2])) {
		  if (mode(select)=="numeric") selectionS <- select
		  else {
		    if (sum(rownames(res.pca$ind$coord)%in%select)+sum(rownames(res.pca$ind.sup$coord)%in%select)!=0) selectionS <- which(rownames(res.pca$ind.sup$coord)%in%select)
			else {
 		    if (grepl("dist",select)) selectionS <- (rev(order(res.pca$ind.sup$dist)))[1:min(nrow(res.pca$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"dist"))),na.rm=T))]
 		    if (grepl("coord",select)) selectionS <- (rev(order(apply(res.pca$ind.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
 		    if (grepl("cos2",select)) {
			  if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.pca$ind.sup$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.pca$ind.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
			  else selectionS <- which(apply(res.pca$ind.sup$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
			}
			if (is.integer(select)) selectionS <- select
			}  
		  }
		}
       if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new(width=min(14,8*(xmax-xmin)/(ymax-ymin)),height=8)
       if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
        if (habillage == "ind") {
            nb.prod <- nrow(coord.actif)
            if (length(col.hab) != nb.prod) color.ind <- c(1:nb.prod)
            else  color.ind <- col.hab
            if (!is.null(coord.illu)) color.ind.sup <- c((nb.prod+1):(nb.prod+nrow(coord.illu)))
            color.mod <- "darkred"
        }
        if ((habillage != "none")&(habillage != "ind")) {
            liste.quali <- colnames(res.pca$call$quali.sup$quali.sup)
            if (is.numeric(habillage)) nom.quali <- colnames(res.pca$call$X)[habillage]
			else nom.quali <- habillage
            if (!(nom.quali %in% liste.quali)) stop("The variable ", habillage, " is not qualitative")
            n.mod <- res.pca$call$quali.sup$modalite[liste.quali == nom.quali]
            if (length(col.hab) != n.mod) {
                color.mod <- c(1:n.mod)
                color.ind <- as.numeric(as.factor(res.pca$call$X[, nom.quali]))
                col.ind.sup <- color.ind[res.pca$call$ind.sup]
                if (!is.null(res.pca$call$ind.sup)) color.ind <- color.ind[-res.pca$call$ind.sup]
            }
            else {
                color.mod <- col.hab
                color.ind <- as.factor(res.pca$call$X[, nom.quali])
                levels(color.ind) <- col.hab
                col.ind.sup <- color.ind[res.pca$call$ind.sup]
                if (!is.null(res.pca$call$ind.sup)) color.ind <- color.ind[-res.pca$call$ind.sup]
                color.ind <- as.character(color.ind)
            }
        }
        if (habillage == "none") {
            color.ind <- rep(col.ind,nrow(coord.actif))
            color.mod <- col.quali
            if (!is.null(res.pca$ind.sup)&is.na(test.invisible[2])) col.ind.sup <- rep(col.ind.sup,nrow(res.pca$ind.sup$coord))
        }
        color.sup <- col.ind.sup
		
        plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
        abline(v=0,lty=2, ...)
        abline(h=0,lty=2, ...)
		coo <- labe <- coll <- ipch <- fonte <- NULL
        if (is.na(test.invisible[1])) {
			coo <- rbind(coo,coord.actif)
			if (lab.ind){ labe <- c(labe,rownames(coord.actif))
			} else  labe <- c(labe,rep("",nrow(coord.actif)))
			coll <- c(coll,color.ind)
			ipch <- c(ipch,rep(20,nrow(coord.actif)))
			fonte <- c(fonte,rep(1,nrow(coord.actif)))
		    if (!is.null(selection)){
			  if (is.numeric(unselect)) coll[!((1:length(coll))%in%selection)] = rgb(t(col2rgb(coll[!((1:length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255) 
			  else coll[!((1:length(coll))%in%selection)] = unselect
			  labe[!((1:length(coll))%in%selection)] <- ""
			}
		}
		if (!is.null(res.pca$ind.sup) & is.na(test.invisible[2])) {
			coo <- rbind(coo,res.pca$ind.sup$coord[,axes])
			if (lab.ind.sup){ labe2 <- rownames(res.pca$ind.sup$coord)
			} else  labe2 <- rep("",nrow(res.pca$ind.sup$coord))
			coll2 <- color.sup
		    if (!is.null(selectionS)){
			  if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selectionS)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255) 
			  else coll2[!((1:length(coll2))%in%selectionS)] = unselect
			  labe2[!((1:length(coll2))%in%selectionS)] <- ""
			}
		    if (length(select)==1){
			  if (grepl("contrib",select)){
			  if (is.numeric(unselect)) coll2[1:length(coll2)] = rgb(t(col2rgb(coll2[1:length(coll2)])),alpha=255*(1-unselect),maxColorValue=255) 
			  else coll2[1:length(coll2)] = unselect
			  labe2[1:length(coll2)] <- ""
			}}
			coll <- c(coll,coll2)
			labe <- c(labe,labe2)
			ipch <- c(ipch,rep(1,nrow(res.pca$ind.sup$coord)))
			fonte <- c(fonte,rep(3,nrow(res.pca$ind.sup$coord)))
        }
        if (!is.null(coord.quali) & is.na(test.invisible[3])) {
            num.li <- 0
            modalite <- res.pca$call$quali.sup$modalite
            col.quali<-rep(col.quali, length(modalite))
			coo <- rbind(coo,coord.quali)
			ipch <- c(ipch,rep(22,sum(modalite)))
			if (lab.quali){ labe <- c(labe,rownames(coord.quali))
			} else  labe <- c(labe,rep("",sum(modalite)))
 			fonte <- c(fonte,rep(3,sum(modalite)))
            for (q in 1:length(modalite)) {
                if ((habillage != "none")&(habillage != "ind")) {
				  if (q == match(nom.quali, liste.quali)) coll <- c(coll,color.mod)
				   else coll <- c(coll,rep(col.quali[1],modalite[q]))
				  } else coll <- c(coll,rep(col.quali,modalite[q]))
                num.li <- num.li + modalite[q]
            }
        }
	points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
    if (any(labe!="")){
	  if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
	  if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],shadotext=shadowtext,...)
      if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)
    }
	
    if (!is.null(ellipse)) {
      nbre.ellipse <- nlevels(coord.ellipse[, 1])
      for (e in 1:nbre.ellipse) {
        data.elli <- coord.ellipse[ellipse$res[, 1] == levels(coord.ellipse[, 1])[e], -1]
        if ((habillage != "none")&(habillage != "ind")) lines(x=data.elli[, 1], y = data.elli[, 2], col = color.mod[e],...)
        else lines(x=data.elli[, 1], y = data.elli[, 2], col = col.quali,...)
      }
    }
#    if ((habillage != "none")&(habillage != "ind")) legend("topleft",legend= levels(res.pca$call$X[,habillage]),text.col= color.mod,cex=par("cex")*0.8)
    if ((habillage != "none") & (habillage != "ind")) {
      L <- list(x="topleft", legend = levels(res.pca$call$X[, habillage]), text.col = color.mod, cex = par("cex") * 0.8)
      L <- modifyList(L, legend)
      do.call(graphics::legend, L)
    }
  }
    if (choix == "varcor") {
	  sauv <- res.pca$var$coord
	  res.pca$var$coord <- res.pca$var$cor
	  if (!is.null(res.pca$quanti.sup)) res.pca$quanti.sup$coord <- res.pca$quanti.sup$cor
	  res.pca$call$scale.unit <- TRUE
	}
    if ((choix == "var")||(choix == "varcor")) {
        if (is.null(title)) titre <- "Variables factor map (PCA)"
        else titre <- title
        selection <- selectionS <- NULL
		if (!is.null(select)) {
		  if (mode(select)=="numeric") selection <- select
		  else {
		    if (sum(rownames(res.pca$var$coord)%in%select)+sum(rownames(res.pca$quanti.sup$coord)%in%select)!=0) selection <- which(rownames(res.pca$var$coord)%in%select)
			else {
 		      if (grepl("contrib",select)) selection <- (rev(order(res.pca$var$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$var$contrib[,axes[2]]*res.pca$eig[axes[2],1])))[1:min(nrow(res.pca$var$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
 		      if (grepl("coord",select)) selection <- (rev(order(apply(res.pca$var$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$var$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
 		      if (grepl("cos2",select)) {
			    if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.pca$var$cos2[,axes],1,sum))))[1:min(nrow(res.pca$var$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
				else selection <- which(apply(res.pca$var$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
			  }
			  if (is.integer(select)) selection <- select
			}  
		  }
		}
		if ((!is.null(select))&(!is.null(res.pca$quanti.sup))) {
		  if (mode(select)=="numeric") selectionS <- select
		  else {
		    if (sum(rownames(res.pca$var$coord)%in%select)+sum(rownames(res.pca$quanti.sup$coord)%in%select)!=0) selectionS <- which(rownames(res.pca$quanti.sup$coord)%in%select)
			else {
 		      if (grepl("contrib",select)) selectionS <- NULL
 		      if (grepl("coord",select)) selectionS <- (rev(order(apply(res.pca$quanti.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$quanti.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
 		      if (grepl("cos2",select)) {
			    if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.pca$quanti.sup$cos2[,axes],1,sum))))[1:min(nrow(res.pca$quanti.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
				else selectionS <- which(apply(res.pca$quanti.sup$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
			  }
			  if (is.integer(select)) selectionS <- select
			}  
		  }
		}
		
        test.invisible <- vector(length = 2)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("var", invisible)
            test.invisible[2] <- match("quanti.sup", invisible)
        }
        else  test.invisible <- rep(NA, 2)
        scale.unit <- res.pca$call$scale.unit
        coord.var <- res.pca$var$coord[, axes,drop=FALSE]
        if (!is.null(res.pca$quanti.sup))  coord.quanti <- res.pca$quanti.sup$coord[, axes, drop=FALSE]
        else coord.quanti <- NULL
        if (scale.unit)  xlim <- ylim <- c(-1, 1)
        else {
            xmin <- min(0,coord.var[, 1], coord.quanti[, 1])
            xmax <- max(0,coord.var[, 1], coord.quanti[, 1])
            ymin <- min(0,coord.var[, 2], coord.quanti[, 2])
            ymax <- max(0,coord.var[, 2], coord.quanti[, 2])
            xlim <- c(xmin, xmax) * 1.2
            ylim <- c(ymin, ymax) * 1.2
        }
        if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
        if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
        if (scale.unit) {
            plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, main=titre,...)
            x.cercle <- seq(-1, 1, by = 0.01)
            y.cercle <- sqrt(1 - x.cercle^2)
            lines(x.cercle, y = y.cercle,...)
            lines(x.cercle, y = -y.cercle,...)
        }
        else {
            plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
        }
        abline(v=0,lty=2,...)
        abline(h=0,lty=2,...)
        coll <- coo <- labe <- posi <- NULL
        if (!is.null(coord.var[ which(apply(res.pca$var$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),])&is.na(test.invisible[1])&(nrow(coord.var)>0)){
		  coord.var <- coord.var[ which(apply(res.pca$var$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),,drop=FALSE]
          coo <- coord.var
		  col.var <- rep(col.var,nrow(coord.var))
		  coll <- c(coll,col.var)
		  if (!is.null(col.hab)) coll <- col.hab[which(colnames(res.pca$call$X)%in%rownames(res.pca$var$coord))]
		  if (lab.var){ labe <- c(labe,rownames(coord.var))
		  } else  labe <- c(labe,rep("",nrow(coord.var)))
	      if (!is.null(selection)){
		    if (is.numeric(unselect)) coll[!((1:length(coll))%in%selection)] = rgb(t(col2rgb(coll[!((1:length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255) 
	        else coll[!((1:length(coll))%in%selection)] = unselect
			labe[!((1:length(coll))%in%selection)] <- ""
	      }
		  for (v in 1:nrow(coord.var)) {
                arrows(0, 0, coord.var[v, 1], coord.var[v, 2], length = 0.1, angle = 15, code = 2, col = coll[v])
                if (lab.var) {
                if (abs(coord.var[v,1])>abs(coord.var[v,2])){
                 if (coord.var[v,1]>=0) posi<-c(posi,4)
                 else posi<-c(posi,2)
                }
                else {
                 if (coord.var[v,2]>=0) posi<-c(posi,3)
                 else posi<-c(posi,1)
                }
                }
          }
        }
        if (!is.null(coord.quanti)){
		 if (!is.null(coord.quanti[ which(apply(res.pca$quanti.sup$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),])& is.na(test.invisible[2]) & (nrow(coord.quanti)>0)) {
		  coord.quanti <- coord.quanti[ which(apply(res.pca$quanti.sup$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),,drop=FALSE]
          coo <- rbind(coo,coord.quanti)
		  col.quanti.sup<-rep(col.quanti.sup, nrow(coord.quanti))
		  if (is.null(col.hab)) coll2 <- col.quanti.sup
		  else coll2 <- col.hab[which(colnames(res.pca$call$X)%in%colnames(res.pca$call$quanti.sup))]
		  if (lab.quanti){ labe2 <- rownames(coord.quanti)
		  } else  labe2 <- rep("",nrow(coord.quanti))
		    if (length(select)==1){
			 if (grepl("contrib",select)){
			  if (is.numeric(unselect)) coll2[1:length(coll2)] = rgb(t(col2rgb(coll2[1:length(coll2)])),alpha=255*(1-unselect),maxColorValue=255) 
			  else coll2[1:length(coll2)] = unselect
			  labe2[1:length(coll2)] <- ""
			}}
	      if (!is.null(selectionS)){
		    if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selectionS)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255) 
	        else coll2[!((1:length(coll2))%in%selectionS)] = unselect
			labe2[!((1:length(coll2))%in%selectionS)] <- ""
	      }
            for (q in 1:nrow(coord.quanti)) {
                arrows(0, 0, coord.quanti[q, 1], coord.quanti[q, 2], length = 0.1, angle = 15, code = 2, lty = 2, col=coll2[q])
                if (lab.quanti) {
                if (abs(coord.quanti[q,1])>abs(coord.quanti[q,2])){
                 if (coord.quanti[q,1]>=0) posi<-c(posi,4)
                 else posi<-c(posi,2)
                }
                else {
                 if (coord.quanti[q,2]>=0) posi<-c(posi,3)
                 else posi<-c(posi,1)
                }
                }
            }
		  labe <- c(labe,labe2)
		  coll <- c(coll,coll2)
         }
		}
        if (any(labe!="")){
	      if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
          if (autoLab==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], pos = posi[labe!=""], col = coll[labe!=""],...)
          if (autoLab==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""], shadotext=shadowtext,...)
	    }
    }
}
