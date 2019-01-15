plot.MCA <- function (x, axes = c(1, 2), choix=c("ind","var","quanti.sup"),
    xlim = NULL, ylim = NULL, invisible = c("none","ind", "var", "ind.sup", "quali.sup", "quanti.sup"), 
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "blue",
    label=c("all","none","ind", "var", "ind.sup", "quali.sup", "quanti.sup"), title = NULL, habillage = "none", palette=NULL, 
	autoLab = c("auto","yes","no"),new.plot=FALSE,select=NULL,selectMod=NULL, unselect=0.7, shadowtext=FALSE,
	legend = list(bty = "y", x = "topleft"), ...){
    
    label <- match.arg(label,c("all","none","ind", "var", "ind.sup", "quali.sup", "quanti.sup"),several.ok=TRUE)
    choix <- match.arg(choix,c("ind","var","quanti.sup"))
    autoLab <- match.arg(autoLab,c("auto","yes","no"))
	if (autoLab=="yes") autoLab=TRUE
	if (autoLab=="no") autoLab=FALSE
	invisible <- match.arg(invisible,c("none","ind", "var", "ind.sup", "quali.sup", "quanti.sup"),several.ok=TRUE)
    if ("none"%in%invisible) invisible = NULL
	
	res.mca <- x
    if (!inherits(res.mca, "MCA")) stop("non convenient data")
    if (is.numeric(unselect)) if ((unselect>1)|(unselect<0)) stop("unselect should be betwwen 0 and 1")

   lab.x <- paste("Dim ",axes[1]," (",format(res.mca$eig[axes[1],2],nsmall=2,digits=2),"%)",sep="")
   lab.y <- paste("Dim ",axes[2]," (",format(res.mca$eig[axes[2],2],nsmall=2,digits=2),"%)",sep="")

   if (choix =="ind"){
    lab.ind <- lab.var <- lab.quali.sup <- lab.ind.sup <- FALSE
    if(length(label)==1 && label=="all") lab.ind <- lab.var <- lab.quali.sup <- lab.ind.sup <- TRUE
    if("ind" %in% label) lab.ind<-TRUE
    if("var" %in% label) lab.var<-TRUE
    if("quali.sup" %in% label) lab.quali.sup<-TRUE
    if("ind.sup" %in% label) lab.ind.sup<-TRUE

    test.invisible <- vector(length = 5)
    if (!is.null(invisible)) {
        test.invisible[1] <- match("ind", invisible)
        test.invisible[2] <- match("var", invisible)
        test.invisible[3] <- match("quanti.sup", invisible)
        test.invisible[4] <- match("ind.sup", invisible)
        test.invisible[5] <- match("quali.sup", invisible)
    }
    else  test.invisible <- rep(NA, 5)
    coord.var <- res.mca$var$coord[, axes]
    coord.ind <- res.mca$ind$coord[, axes]
    coord.ind.sup <- coord.quali.sup <- NULL
    if (!is.null(res.mca$ind.sup)) coord.ind.sup <- res.mca$ind.sup$coord[, axes,drop=FALSE]
    if (!is.null(res.mca$quali.sup)) coord.quali.sup <- res.mca$quali.sup$coord[, axes,drop=FALSE]
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.ind[,1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.ind[,1])
      if(is.na(test.invisible[4])) xmin <- min(xmin, coord.ind.sup[, 1])
      if(is.na(test.invisible[4])) xmax <- max(xmax, coord.ind.sup[, 1])
      if(is.na(test.invisible[2])) xmin <- min(xmin, coord.var[,1])
      if(is.na(test.invisible[2])) xmax <- max(xmax, coord.var[,1])
      if(is.na(test.invisible[5])) xmin <- min(xmin, coord.quali.sup[, 1])
      if(is.na(test.invisible[5])) xmax <- max(xmax, coord.quali.sup[, 1])
        xlim <- c(xmin, xmax) * 1.2
    }
    else {
      xmin = xlim[1]
      xmax = xlim[2]
    }
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.ind[,2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.ind[,2])
      if(is.na(test.invisible[4])) ymin <- min(ymin, coord.ind.sup[, 2])
      if(is.na(test.invisible[4])) ymax <- max(ymax, coord.ind.sup[, 2])
      if(is.na(test.invisible[2])) ymin <- min(ymin, coord.var[,2])
      if(is.na(test.invisible[2])) ymax <- max(ymax, coord.var[,2])
      if(is.na(test.invisible[5])) ymin <- min(ymin, coord.quali.sup[,2])
      if(is.na(test.invisible[5])) ymax <- max(ymax, coord.quali.sup[,2])
      ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }
    selection <- selectionS <- selection2 <- selection3 <- NULL
	if (!is.null(select)) {
	  if (mode(select)=="numeric") selection <- select
	  else {
	    if (sum(rownames(res.mca$ind$coord)%in%select)!=0) selection <- which(rownames(res.mca$ind$coord)%in%select)
		else {
 	    if (grepl("contrib",select)) selection <- (rev(order(res.mca$ind$contrib[,axes[1],drop=FALSE]*res.mca$eig[axes[1],1]+res.mca$ind$contrib[,axes[2],drop=FALSE]*res.mca$eig[axes[2],1])))[1:min(nrow(res.mca$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
# 	    if (grepl("contrib",select)) selection <- (rev(order(apply(res.mca$ind$contrib[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.mca$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
 	    if (grepl("coord",select)) selection <- (rev(order(apply(res.mca$ind$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.mca$ind$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
 		if (grepl("cos2",select)) {
		  if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.mca$ind$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.mca$ind$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
		  else selection <- which(apply(res.mca$ind$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
		}
	    if (is.integer(select)) selection <- select
		}  
	  }
	}

	if (!is.null(select)&(!is.null(res.mca$call$ind.sup))) {
	  if (mode(select)=="numeric") selectionS <- select
	  else {
	    if (sum(rownames(res.mca$ind.sup$coord)%in%select)!=0) selectionS <- which(rownames(res.mca$ind.sup$coord)%in%select)
		else {
 	    if (grepl("contrib",select)) selectionS <- NULL
 	    if (grepl("coord",select)) selectionS <- (rev(order(apply(res.mca$ind.sup$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.mca$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
 		if (grepl("cos2",select)) {
		  if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.mca$ind.sup$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.mca$ind.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
		  else selectionS <- which(apply(res.mca$ind.sup$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
		}
	    if (is.integer(select)) selectionS <- select
		}  
	  }
	}

	if (!is.null(selectMod)) {
	  if (mode(selectMod)=="numeric") selection2 <- selectMod
	  else {
	    if (sum(rownames(res.mca$var$coord)%in%selectMod)+sum(rownames(res.mca$quali.sup$coord)%in%selectMod)!=0) selection2 <- which(rownames(res.mca$var$coord)%in%selectMod)
		else {
 	      if (grepl("contrib",selectMod)) selection2 <- (rev(order(res.mca$var$contrib[,axes[1],drop=FALSE]*res.mca$eig[axes[1],1]+res.mca$var$contrib[,axes[2],drop=FALSE]*res.mca$eig[axes[2],1])))[1:min(nrow(res.mca$var$coord),sum(as.integer(unlist(strsplit(selectMod,"contrib"))),na.rm=T))]
 	      if (grepl("coord",selectMod)) selection2 <- (rev(order(apply(res.mca$var$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.mca$var$coord),sum(as.integer(unlist(strsplit(selectMod,"coord"))),na.rm=T))]
 		  if (grepl("cos2",selectMod)) {
		    if (sum(as.numeric(unlist(strsplit(selectMod,"cos2"))),na.rm=T)>=1) selection2 <- (rev(order(apply(res.mca$var$cos2[,axes],1,sum))))[1:min(nrow(res.mca$var$coord),sum(as.numeric(unlist(strsplit(selectMod,"cos2"))),na.rm=T))]
		    else selection2 <- which(apply(res.mca$var$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(selectMod,"cos2"))),na.rm=T))
		  }
 	      if (grepl("v.test",selectMod)) selection2 <- union(which(abs(res.mca$var$v.test[,axes[1],drop=FALSE])>sum(as.integer(unlist(strsplit(selectMod,"v.test"))),na.rm=T)),which(abs(res.mca$var$v.test[,axes[2],drop=FALSE])>sum(as.integer(unlist(strsplit(selectMod,"v.test"))),na.rm=T))) 
	      if (is.integer(selectMod)) selection2 <- selectMod
		}  
	  }
	}

	if ((!is.null(selectMod))&(!is.null(res.mca$call$quali.sup))) {
	  if (mode(selectMod)=="numeric") selection3 <- selectMod
	  else {
	    if (sum(rownames(res.mca$var$coord)%in%selectMod)+sum(rownames(res.mca$quali.sup$coord)%in%selectMod)!=0) selection3 <- which(rownames(res.mca$quali.sup$coord)%in%selectMod)
		else {
 	      if (grepl("contrib",selectMod)) selection3 <- NULL
 	      if (grepl("coord",selectMod)) selection3 <- (rev(order(apply(res.mca$quali.sup$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.mca$quali.sup$coord),sum(as.integer(unlist(strsplit(selectMod,"coord"))),na.rm=T))]
 		  if (grepl("cos2",selectMod)) {
		    if (sum(as.numeric(unlist(strsplit(selectMod,"cos2"))),na.rm=T)>=1) selection3 <- (rev(order(apply(res.mca$quali.sup$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.mca$quali.sup$coord),sum(as.numeric(unlist(strsplit(selectMod,"cos2"))),na.rm=T))]
		    else selection3 <- which(apply(res.mca$quali.sup$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(selectMod,"cos2"))),na.rm=T))
		  }
 	      if (grepl("v.test",selectMod)) selection3 <- union(which(abs(res.mca$quali.sup$v.test[,axes[1],drop=FALSE])>sum(as.integer(unlist(strsplit(selectMod,"v.test"))),na.rm=T)),which(abs(res.mca$quali.sup$v.test[,axes[2],drop=FALSE])>sum(as.integer(unlist(strsplit(selectMod,"v.test"))),na.rm=T))) 
	      if (is.integer(selectMod)) selection3 <- selectMod
		}  
	  }
	}

        if (habillage == "quali") {
          aux = 1
          col.var = NULL
          for (j in res.mca$call$quali) {
            col.var <- c(col.var,rep(aux,nlevels(res.mca$call$X[,j])))
            aux = aux + 1
          }
          if (!is.null(res.mca$call$quali.sup)){
            col.quali.sup = NULL
            for (j in res.mca$call$quali.sup) {
              col.quali.sup <- c(col.quali.sup,rep(aux,nlevels(res.mca$call$X[,j])))
              aux = aux + 1
            }
          }
        }
        if ((habillage != "none")&(habillage != "quali")) {
          if (!is.factor(res.mca$call$X[,habillage])) stop("The variable ", habillage, " is not qualitative")
          col.ind <- as.numeric(as.factor(res.mca$call$X[, habillage]))
          n.mod <- nlevels(as.factor(res.mca$call$X[, habillage]))
          col.ind.sup <- col.ind[res.mca$call$ind.sup]
          if (!is.null(res.mca$call$ind.sup)) col.ind <- col.ind[-res.mca$call$ind.sup]
		}
        if (habillage == "none") {
		  col.var <- rep(col.var,nrow(coord.var))
		  if (!is.null(res.mca$call$quali.sup)) col.quali.sup <- rep(col.quali.sup,nrow(coord.quali.sup))
        }
    
    titre = title
    if (is.null(title)) titre <- "MCA factor map"
    if (is.na(test.invisible[1])|is.na(test.invisible[2])|is.na(test.invisible[4])|is.na(test.invisible[5])) {
      if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new(width=min(14,max(8,8*(xmax-xmin)/(ymax-ymin))),height=8)
      if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
      plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
      abline(v=0,lty=2, ...)
      abline(h=0,lty=2, ...)
	  coo <- labe <- coll <- ipch <- fonte <- NULL
      if (is.na(test.invisible[1])) {
		coo <- rbind(coo,coord.ind)
		if (lab.ind){ labe <- c(labe,rownames(coord.ind))
		} else  labe <- c(labe,rep("",nrow(coord.ind)))
		if (length(col.ind)==1) coll <- c(coll,rep(col.ind,nrow(coord.ind)))
		else coll <- c(coll,col.ind)
		if (!is.null(select)) {
		  if (is.numeric(unselect)) coll[!((1:length(coll))%in%selection)] <- rgb(t(col2rgb(coll[!((1:length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255) 
		  else coll[!((1:length(coll))%in%selection)] <- unselect
		  labe[!((1:length(labe))%in%selection)] <- ""
		}
		ipch <- c(ipch,rep(16,nrow(coord.ind)))
		fonte <- c(fonte,rep(1,nrow(coord.ind)))
      }
      if (is.na(test.invisible[2])) {
		coo <- rbind(coo,coord.var)
		if (lab.var){ labe2 <- rownames(coord.var)
		} else  labe2 <- rep("",nrow(coord.var))
		coll2 <- col.var
		if (!is.null(selectMod)) {
		  if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selection2)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selection2)])),alpha=255*(1-unselect),maxColorValue=255) 
		  else coll2[!((1:length(coll2))%in%selection2)] = unselect
		  labe2[!((1:length(labe2))%in%selection2)] <- ""
		}
		coll <- c(coll,coll2)
		labe <- c(labe,labe2)
		ipch <- c(ipch,rep(17,nrow(coord.var)))
		fonte <- c(fonte,rep(1,nrow(coord.var)))
      }
      if (!is.null(res.mca$quali.sup) & is.na(test.invisible[5])) {
		coo <- rbind(coo,coord.quali.sup)
		if (lab.quali.sup){ labe2 <- rownames(coord.quali.sup)
		} else  labe2 <- rep("",nrow(coord.quali.sup))
		coll2 <- col.quali.sup
		if ((!is.null(selectMod))&!is.null(selection3)) {
		  if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selection3)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selection3)])),alpha=255*(1-unselect),maxColorValue=255) 
		  else coll2[!((1:length(coll2))%in%selection3)] = unselect
		  labe2[!((1:length(labe2))%in%selection3)] <- ""
		}
		if (length(selectMod)==1) {
           if (grepl("contrib",selectMod)){
		    if (is.numeric(unselect)) coll2[1:length(coll2)] = rgb(t(col2rgb(coll2[1:length(coll2)])),alpha=255*(1-unselect),maxColorValue=255) 
			else coll2[1:length(coll2)] = unselect
			labe2[1:length(coll2)] <- ""
		}}
		coll <- c(coll,coll2)
		labe <- c(labe,labe2)
		ipch <- c(ipch,rep(17,nrow(coord.quali.sup)))
		fonte <- c(fonte,rep(1,nrow(coord.quali.sup)))
      }
      if (!is.null(res.mca$ind.sup) & is.na(test.invisible[4])) {
		coo <- rbind(coo,coord.ind.sup)
		if (lab.ind.sup){ labe2 <- rownames(coord.ind.sup)
		} else  labe2 <- rep("",nrow(coord.ind.sup))
		coll2 <- rep(col.ind.sup,nrow(coord.ind.sup))
		if ((!is.null(select))&!is.null(selectionS)) {
		  if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selectionS)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255) 
		  else coll2[!((1:length(coll2))%in%selectionS)] = unselect
		  labe2[!((1:length(labe2))%in%selectionS)] <- ""
		}
		if (!is.null(select)){
		  if (grepl("contrib",select)){
		    if (is.numeric(unselect)) coll2[1:length(coll2)] = rgb(t(col2rgb(coll2[1:length(coll2)])),alpha=255*(1-unselect),maxColorValue=255) 
			else coll2[1:length(coll2)] = unselect
			labe2[1:length(coll2)] <- ""
		}}
		coll <- c(coll,coll2)
		labe <- c(labe,labe2)
		ipch <- c(ipch,rep(16,nrow(coord.ind.sup)))
		fonte <- c(fonte,rep(1,nrow(coord.ind.sup)))
      }
#      if ((habillage != "none")&(habillage != "quali")&(is.na(test.invisible[1])|is.na(test.invisible[2])))  legend("topleft",legend= levels(res.mca$call$X[,habillage]),text.col= 1:n.mod,cex=0.8)
      if ((habillage != "none")&(habillage != "quali")&(is.na(test.invisible[1])|is.na(test.invisible[2]))) {
        L <- list(x="topleft", legend = levels(res.mca$call$X[,habillage]), text.col = 1:n.mod, cex = par("cex") * 0.8)
        L <- modifyList(L, legend)
        do.call(graphics::legend, L)
      }
    }
	if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
    if (any(labe!="")){
	  if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
	  if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],shadotext=shadowtext,...)
      if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)
	}
	if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
   }
    if (choix == "quanti.sup") {
     if (!is.null(res.mca$quanti.sup)) {
      if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
      if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
	  if (is.null(title)) title <- "Supplementary variables on the MCA factor map"
      plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), col = "white", asp=1, ...)
      abline(v=0,lty=2, ...)
      abline(h=0,lty=2, ...)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle,...)
      lines(x.cercle, y = -y.cercle,...)
	  
		if (!is.null(select)) {
		  if (mode(select)=="numeric") selection <- select
		  else {
		    if (sum(rownames(res.mca$quanti.sup$coord)%in%select)!=0) selection <- which(rownames(res.mca$quanti.sup$coord)%in%select)
			else {
 		      if (grepl("coord",select)) selection <- (rev(order(apply(res.mca$quanti.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.mca$quanti.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
			  if (is.integer(select)) selection <- select
			}  
		  }
		  res.mca$quanti.sup$coord = res.mca$quanti.sup$coord[selection,,drop=FALSE]
		}

		for (v in 1:nrow(res.mca$quanti.sup$coord)) {
        arrows(0, 0, res.mca$quanti.sup$coord[v, axes[1]], res.mca$quanti.sup$coord[v, axes[2]], length = 0.1, angle = 15, code = 2, col = col.quanti.sup,...)
        if (abs(res.mca$quanti.sup$coord[v,axes[1]])>abs(res.mca$quanti.sup$coord[v,axes[2]])){
          if (res.mca$quanti.sup$coord[v,axes[1]]>=0) pos<-4
          else pos<-2
        }
        else {
          if (res.mca$quanti.sup$coord[v,axes[2]]>=0) pos<-3
          else pos<-1
        }
		if((!is.null(label)) && (label=="all" | "quanti.sup" %in% label)){
        	text(res.mca$quanti.sup$coord[v, axes[1]], y = res.mca$quanti.sup$coord[v, axes[2]], labels = rownames(res.mca$quanti.sup$coord)[v], pos = pos, col = col.quanti.sup,...)
		}
      }
    }
    }

    if (choix == "var") {
      lab.var <- lab.quali.sup <- lab.quanti.sup <- FALSE
      if(length(label)==1 && label=="all") lab.var <- lab.quali.sup <- lab.quanti.sup <- TRUE
      if("var" %in% label) lab.var<-TRUE
      if("quali.sup" %in% label) lab.quali.sup<-TRUE
      if("quanti.sup" %in% label) lab.quanti.sup<-TRUE

      test.invisible <- vector(length = 3)
      if (!is.null(invisible)) {
          test.invisible[1] <- match("var", invisible)
          test.invisible[2] <- match("quali.sup", invisible)
          test.invisible[3] <- match("quanti.sup", invisible)
      }
      else  test.invisible <- rep(NA, 3)
	  
      if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
      if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
	  if (is.null(xlim)) xlim <- c(0,1)
	  if (is.null(ylim)) ylim <- c(0,1)
      plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
      abline(v=0,lty=2, ...)
      abline(h=0,lty=2, ...)
      if (is.null(title)) title <- "Variables representation"
	  coo <- labe <- coll <- ipch <- fonte <- NULL
      coord.actif <- res.mca$var$eta2[, axes,drop=FALSE]
      if (!is.null(res.mca$quali.sup$eta2)) coord.illu <- res.mca$quali.sup$eta2[,axes,drop=FALSE]
      if (!is.null(res.mca$quanti.sup$coord)) coord.illuq <- res.mca$quanti.sup$coord[,axes,drop=FALSE]^2
      if (is.na(test.invisible[1])){
		coo <- rbind(coo,coord.actif)
		if (lab.var){ labe <- c(labe,rownames(coord.actif))
		} else  labe <- c(labe,rep("",nrow(coord.actif)))
		coll <- c(coll,rep(col.var,nrow(coord.actif)))
		ipch <- c(ipch,rep(20,nrow(coord.actif)))
		fonte <- c(fonte,rep(1,nrow(coord.actif)))
      }
      if ((!is.null(res.mca$quali.sup$eta2))&&(is.na(test.invisible[2]))){
		coo <- rbind(coo,coord.illu)
		if (lab.quali.sup){ labe <- c(labe,rownames(coord.illu))
		} else  labe <- c(labe,rep("",nrow(coord.illu)))
		coll <- c(coll,rep(col.quali.sup,nrow(coord.illu)))
		ipch <- c(ipch,rep(1,nrow(coord.illu)))
		fonte <- c(fonte,rep(3,nrow(coord.illu)))
      }
      if ((!is.null(res.mca$quanti.sup$coord))&&(is.na(test.invisible[3]))){
		coo <- rbind(coo,coord.illuq)
		if (lab.quanti.sup){ labe <- c(labe,rownames(coord.illuq))
		} else  labe <- c(labe,rep("",nrow(coord.illuq)))
		coll <- c(coll,rep(col.quanti.sup,nrow(coord.illuq)))
		ipch <- c(ipch,rep(1,nrow(coord.illuq)))
		fonte <- c(fonte,rep(3,nrow(coord.illuq)))
      }
	  
### 22 mars 2018
	  selection <- NULL
	if (!is.null(select)) {
	  if (mode(select)=="numeric") selection <- (rev(order(apply(coo^2,1,sum))))[1:min(nrow(coo), as.integer(select))]
	  else {
	    if (sum(rownames(coo)%in%select)!=0) selection <- which(rownames(coo)%in%select)
		else {
 	    if (grepl("coord",select)) selection <- (rev(order(apply(coo^2,1,sum))))[1:min(nrow(coo),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
	    if (is.integer(select)) selection <- select
		}  
	  }
	}
		if (!is.null(select)) {
		  if (is.numeric(unselect)) coll[!((1:length(coll))%in%selection)] <- rgb(t(col2rgb(coll[!((1:length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255) 
		  else coll[!((1:length(coll))%in%selection)] <- unselect
		  labe[!((1:length(labe))%in%selection)] <- ""
		}
### Fin 22 mars 2018	
	  
	  
      if (any(labe!="")){
	    if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
        if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],...)
        if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)
	  }
	  points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
    }
    
}
