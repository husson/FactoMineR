plot.CA <- function (x, axes = c(1, 2),
    xlim = NULL, ylim = NULL, invisible = c("none","row", "col", "row.sup", "col.sup","quali.sup"), choix = c("CA","quanti.sup"), col.row = "blue",
    col.col = "red", col.row.sup = "darkblue", col.col.sup = "darkred",col.quali.sup ="magenta",
    col.quanti.sup="blue",label = c("all","none","row", "row.sup", "col","col.sup", "quali.sup"), title = NULL, palette=NULL, 
	autoLab = c("auto","yes","no"),new.plot=FALSE, selectRow = NULL, selectCol = NULL,
    unselect = 0.7,shadowtext = FALSE, habillage = "none", legend = list(bty = "y", x = "topleft"),...) {
	
    res.ca <- x
    if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
    if (!inherits(res.ca, "CA")) stop("non convenient data")
    if (is.numeric(unselect)) if ((unselect>1)|(unselect<0)) stop("unselect should be betwwen 0 and 1")
    label <- match.arg(label,c("all","none","row", "row.sup", "col","col.sup", "quali.sup"),several.ok=TRUE)
    choix <- match.arg(choix,c("CA","quanti.sup"))
    choix <- tolower(choix)
    autoLab <- match.arg(autoLab,c("auto","yes","no"))
	if (autoLab=="yes") autoLab=TRUE
	if (autoLab=="no") autoLab=FALSE
	invisible <- match.arg(invisible,c("none","row", "col", "row.sup", "col.sup","quali.sup"),several.ok=TRUE)
    if ("none"%in%invisible) invisible = NULL
	
if (choix=="ca"){
    lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- FALSE
    if(length(label)==1 && label=="all") lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- lab.quali.sup <- TRUE
    if("row" %in% label) lab.row<-TRUE
    if("col" %in% label) lab.col<-TRUE
    if("row.sup" %in% label) lab.row.sup<-TRUE
    if("col.sup" %in% label) lab.col.sup<-TRUE
    if("quali.sup" %in% label) lab.quali.sup<-TRUE

    coord.col <- res.ca$col$coord[, axes]
    coord.row <- res.ca$row$coord[, axes]
    coord.row.sup <- coord.col.sup <- coord.quali.sup <- NULL
    if (!is.null(res.ca$row.sup)) coord.row.sup <- res.ca$row.sup$coord[, axes,drop=FALSE]
    if (!is.null(res.ca$col.sup)) coord.col.sup <- res.ca$col.sup$coord[, axes,drop=FALSE]
    if (!is.null(res.ca$quali.sup)) coord.quali.sup <- res.ca$quali.sup$coord[, axes,drop=FALSE]

    test.invisible <- vector(length = 4)
    if (!is.null(invisible)) {
        test.invisible[1] <- match("row", invisible)
        test.invisible[2] <- match("col", invisible)
        test.invisible[3] <- match("row.sup", invisible)
        test.invisible[4] <- match("col.sup", invisible)
        test.invisible[5] <- match("quali.sup", invisible)
    }
    else  test.invisible <- rep(NA, 4)
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.row[,1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.row[,1])
      if(is.na(test.invisible[3])) xmin <- min(xmin, coord.row.sup[, 1])
      if(is.na(test.invisible[3])) xmax <- max(xmax, coord.row.sup[, 1])
      if(is.na(test.invisible[2])) xmin <- min(xmin, coord.col[,1])
      if(is.na(test.invisible[2])) xmax <- max(xmax, coord.col[,1])
      if(is.na(test.invisible[4])) xmin <- min(xmin, coord.col.sup[, 1])
      if(is.na(test.invisible[4])) xmax <- max(xmax, coord.col.sup[, 1])
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
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.row[,2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.row[,2])
      if(is.na(test.invisible[3])) ymin <- min(ymin, coord.row.sup[,2])
      if(is.na(test.invisible[3])) ymax <- max(ymax, coord.row.sup[,2])
      if(is.na(test.invisible[2])) ymin <- min(ymin, coord.col[,2])
      if(is.na(test.invisible[2])) ymax <- max(ymax, coord.col[,2])
      if(is.na(test.invisible[4])) ymin <- min(ymin, coord.col.sup[,2])
      if(is.na(test.invisible[4])) ymax <- max(ymax, coord.col.sup[,2])
      if(is.na(test.invisible[5])) ymin <- min(ymin, coord.quali.sup[,2])
      if(is.na(test.invisible[5])) ymax <- max(ymax, coord.quali.sup[,2])
        ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }
    selection <- selectionC <- selectionC2 <- selectionR2 <- NULL
	if (!is.null(selectRow)) {
		if (mode(selectRow)=="numeric") selection <- selectRow
		else {
		  if (sum(rownames(res.ca$row$coord)%in%selectRow)+sum(rownames(res.ca$row.sup$coord)%in%selectRow)!=0) selection <- which(rownames(res.ca$row$coord)%in%selectRow)
		  else {
 		    if (grepl("contrib",selectRow)) selection <- (rev(order(res.ca$row$contrib[,axes[1],drop=FALSE]*res.ca$eig[axes[1],1]+res.ca$row$contrib[,axes[2],drop=FALSE]*res.ca$eig[axes[2],1])))[1:min(nrow(res.ca$row$coord),sum(as.integer(unlist(strsplit(selectRow,"contrib"))),na.rm=T))]
# 		    if (grepl("contrib",selectRow)) selection <- (rev(order(apply(res.ca$row$contrib[,axes],1,sum))))[1:min(nrow(res.ca$row$coord),sum(as.integer(unlist(strsplit(selectRow,"contrib"))),na.rm=T))]
 		    if (grepl("inertia",selectRow)) selection <- (rev(order(res.ca$row$inertia)))[1:min(nrow(res.ca$row$coord),sum(as.integer(unlist(strsplit(selectRow,"inertia"))),na.rm=T))]
 		    if (grepl("coord",selectRow)) selection <- (rev(order(apply(res.ca$row$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.ca$row$coord),sum(as.integer(unlist(strsplit(selectRow,"coord"))),na.rm=T))]
 		    if (grepl("cos2",selectRow)) {
			  if (sum(as.numeric(unlist(strsplit(selectRow,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.ca$row$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.ca$row$coord),sum(as.numeric(unlist(strsplit(selectRow,"cos2"))),na.rm=T))]
			  else selection <- which(apply(res.ca$row$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(selectRow,"cos2"))),na.rm=T))
			}
			if (is.integer(selectRow)) selection <- selectRow
		  }  
		}
	}

	if ((!is.null(selectRow))&(!is.null(res.ca$row.sup))) {
		if (mode(selectRow)=="numeric") selectionR2 <- selectRow
		else {
		  if (sum(rownames(res.ca$row$coord)%in%selectRow)+sum(rownames(res.ca$row.sup$coord)%in%selectRow)!=0) selectionR2 <- which(rownames(res.ca$row.sup$coord)%in%selectRow)
		  else {
 		    if (grepl("inertia",selectRow)) selectionR2 <- (rev(order(res.ca$row.sup$inertia)))[1:min(nrow(res.ca$row.sup$coord),sum(as.integer(unlist(strsplit(selectRow,"inertia"))),na.rm=T))]
 		    if (grepl("coord",selectRow)) selectionR2 <- (rev(order(apply(res.ca$row.sup$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.ca$row.sup$coord),sum(as.integer(unlist(strsplit(selectRow,"coord"))),na.rm=T))]
 		    if (grepl("cos2",selectRow)) {
			  if (sum(as.numeric(unlist(strsplit(selectRow,"cos2"))),na.rm=T)>=1) selectionR2 <- (rev(order(apply(res.ca$row.sup$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.ca$row.sup$coord),sum(as.numeric(unlist(strsplit(selectRow,"cos2"))),na.rm=T))]
			  else selectionR2 <- which(apply(res.ca$row.sup$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(selectRow,"cos2"))),na.rm=T))
			}
			if (is.integer(selectRow)) selectionR2 <- selectRow
		  }  
		}
	}

	if (!is.null(selectCol)) {
		if (mode(selectCol)=="numeric") selectionC <- selectCol
		else {
		  if (sum(rownames(res.ca$col.sup$coord)%in%selectCol)+sum(rownames(res.ca$col$coord)%in%selectCol)!=0) selectionC <- which(rownames(res.ca$col$coord)%in%selectCol)
		  else {
 		    if (grepl("contrib",selectCol)) selectionC <- (rev(order(res.ca$col$contrib[,axes[1],drop=FALSE]*res.ca$eig[axes[1],1]+res.ca$col$contrib[,axes[2],drop=FALSE]*res.ca$eig[axes[2],1])))[1:min(nrow(res.ca$col$coord),sum(as.integer(unlist(strsplit(selectCol,"contrib"))),na.rm=T))]
# 		    if (grepl("contrib",selectCol)) selectionC <- (rev(order(apply(res.ca$col$contrib[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.ca$col$coord),sum(as.integer(unlist(strsplit(selectCol,"contrib"))),na.rm=T))]
 		    if (grepl("inertia",selectCol)) selectionC <- (rev(order(res.ca$col$inertia)))[1:min(nrow(res.ca$col$coord),sum(as.integer(unlist(strsplit(selectCol,"inertia"))),na.rm=T))]
 		    if (grepl("coord",selectCol)) selectionC <- (rev(order(apply(res.ca$col$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.ca$col$coord),sum(as.integer(unlist(strsplit(selectCol,"coord"))),na.rm=T))]
 		    if (grepl("cos2",selectCol)) {
			  if (sum(as.numeric(unlist(strsplit(selectCol,"cos2"))),na.rm=T)>=1) selectionC <- (rev(order(apply(res.ca$col$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.ca$col$coord),sum(as.numeric(unlist(strsplit(selectCol,"cos2"))),na.rm=T))]
			  else selectionC <- which(apply(res.ca$col$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(selectCol,"cos2"))),na.rm=T))
			}
			if (is.integer(selectCol)) selectionC <- selectCol
		  }  
		}
	}

	if ((!is.null(selectCol))&(!is.null(res.ca$col.sup$coord))) {
		if (mode(selectCol)=="numeric") selectionC2 <- selectCol
		else {
		  if (sum(rownames(res.ca$col.sup$coord)%in%selectCol)+sum(rownames(res.ca$col$coord)%in%selectCol)!=0) selectionC2 <- which(rownames(res.ca$col.sup$coord)%in%selectCol)
		  else {
 		    if (grepl("inertia",selectCol)) selectionC2 <- (rev(order(res.ca$col.sup$inertia)))[1:min(nrow(res.ca$col.sup$coord),sum(as.integer(unlist(strsplit(selectCol,"inertia"))),na.rm=T))]
 		    if (grepl("coord",selectCol)) selectionC2 <- (rev(order(apply(res.ca$col.sup$coord[,axes,drop=FALSE]^2,1,sum))))[1:min(nrow(res.ca$col.sup$coord),sum(as.integer(unlist(strsplit(selectCol,"coord"))),na.rm=T))]
 		    if (grepl("cos2",selectCol)) {
			  if (sum(as.numeric(unlist(strsplit(selectCol,"cos2"))),na.rm=T)>=1) selectionC2 <- (rev(order(apply(res.ca$col.sup$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.ca$col.sup$coord),sum(as.numeric(unlist(strsplit(selectCol,"cos2"))),na.rm=T))]
			  else selectionC2 <- which(apply(res.ca$col.sup$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(selectCol,"cos2"))),na.rm=T))
			}
			if (is.integer(selectCol)) selectionC2 <- selectCol
		  }  
		}
	}

    if (is.null(title)) titre <- "CA factor map"
    else titre <- title
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    plot(0, 0, main = titre, xlab = paste("Dim ",axes[1]," (",format(res.ca$eig[axes[1],2],nsmall=2,digits=2),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",format(res.ca$eig[axes[2],2],nsmall=2,digits=2),"%)",sep=""), xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
    abline(h=0,lty=2,...)
    abline(v=0,lty=2,...)
	
	if (habillage != "none"){
	    liste.quali <- colnames(res.ca$call$Xtot)[res.ca$call$quali.sup]
        if (is.numeric(habillage)) nom.quali <- colnames(res.ca$call$Xtot)[habillage]
		else nom.quali <- habillage
        if (!(nom.quali %in% liste.quali)) stop("The variable ", habillage, " is not qualitative")
        if (is.null(res.ca$row.sup)) col.row <- 1+as.integer(res.ca$call$Xtot[,nom.quali])
		else col.row <- 1+as.integer(res.ca$call$Xtot[-res.ca$call$row.sup,nom.quali])
		col.quali.sup <- rep(1,nrow(res.ca$quali.sup$coord))
		col.quali.sup[which(rownames(res.ca$quali.sup$coord)%in%paste(colnames(res.ca$call$Xtot[,nom.quali,drop=FALSE]),levels(res.ca$call$Xtot[,nom.quali]),sep="."))] <- 2:(nlevels(res.ca$call$Xtot[,nom.quali])+1)
    } 
	if (length(col.row)==1) col.row <- rep(col.row,nrow(coord.row))
	if (length(col.col)==1) col.col  <- rep(col.col,nrow(coord.col))
	if ((!is.null(res.ca$row.sup))&(length(col.row.sup)==1)) col.row.sup  <- rep(col.row.sup,nrow(coord.row.sup))
	if ((!is.null(res.ca$col.sup))&(length(col.col.sup)==1)) col.col.sup <- rep(col.col.sup,nrow(coord.col.sup))
	if ((!is.null(res.ca$quali.sup))&(length(col.quali.sup)==1)) col.quali.sup <- rep(col.quali.sup,nrow(coord.quali.sup))
	coo <- ipch <- labe <- coll <- fonte <- NULL
	
    if (is.na(test.invisible[1])) {
      coo <- coord.row
      ipch <- rep(20,nrow(coord.row))
      coll <- col.row
      fonte <- rep(1,nrow(coord.row))
      if (lab.row==TRUE) labe <- rownames(coord.row)
	  else labe <- rep("",nrow(coord.row))
	  if (!is.null(selection)){
	    if (is.numeric(unselect)) coll[!((1:length(coll))%in%selection)] = rgb(t(col2rgb(coll[!((1:length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255)
	    else coll[!((1:length(coll))%in%selection)] = unselect
		labe[!((1:length(coll))%in%selection)] <- ""
	  }
    }
    if (is.na(test.invisible[2])) {
      coo <- rbind(coo,coord.col)
      ipch <- c(ipch,rep(17,nrow(coord.col)))
      fonte <- c(fonte,rep(1,nrow(coord.col)))
      coll2 <- col.col
      if (lab.col==TRUE) labe2 <- rownames(coord.col)
	  else labe2 <- rep("",nrow(coord.col))
	  if (!is.null(selectionC)){
	    if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selectionC)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selectionC)])),alpha=255*(1-unselect),maxColorValue=255)
	    else coll2[!((1:length(coll2))%in%selectionC)] = unselect
		labe2[!((1:length(coll2))%in%selectionC)] <- ""
	  }
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
    }
    if (!is.null(res.ca$col.sup) & is.na(test.invisible[4])) {
      coo <- rbind(coo,coord.col.sup)
      ipch <- c(ipch,rep(17,nrow(coord.col.sup)))
      fonte <- c(fonte,rep(3,nrow(coord.col.sup)))
      coll2 <- col.col.sup
      if (lab.col.sup==TRUE) labe2 <- rownames(coord.col.sup)
	  else labe2 <- rep("",nrow(coord.col.sup))
	  if (!is.null(selectionC2)){
	    if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selectionC2)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selectionC2)])),alpha=255*(1-unselect),maxColorValue=255)
	    else coll2[!((1:length(coll2))%in%selectionC2)] = unselect
		labe2[!((1:length(coll2))%in%selectionC2)] <- ""
	  }
	  if (length(selectCol)==1){
	   if (grepl("contrib",selectCol)){
		if (is.numeric(unselect)) coll2[1:length(coll2)] = rgb(t(col2rgb(coll2[1:length(coll2)])),alpha=255*(1-unselect),maxColorValue=255)
		else coll2[1:length(coll2)] = unselect
		labe2[1:length(coll2)] <- ""
	  }}
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
    }
    if (!is.null(res.ca$row.sup) & is.na(test.invisible[3])) {
      coo <- rbind(coo,coord.row.sup)
      ipch <- c(ipch,rep(20,nrow(coord.row.sup)))
      fonte <- c(fonte,rep(3,nrow(coord.row.sup)))
      coll2 <- col.row.sup
      if (lab.row.sup==TRUE) labe2 <- rownames(coord.row.sup)
	  else labe2 <- rep("",nrow(coord.row.sup))
	  if (!is.null(selectionR2)){
	    if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selectionR2)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selectionR2)])),alpha=255*(1-unselect),maxColorValue=255)
	    else coll2[!((1:length(coll2))%in%selectionR2)] = unselect
		labe2[!((1:length(coll2))%in%selectionR2)] <- ""
	  }
	  if (length(selectRow)==1){
	   if (grepl("contrib",selectRow)){
		if (is.numeric(unselect)) coll2[1:length(coll2)] <- rgb(t(col2rgb(coll2[1:length(coll2)])),alpha=255*(1-unselect),maxColorValue=255)
		else coll2[1:length(coll2)] <- unselect
		labe2[1:length(coll2)] <- ""
	  }}
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
    }
    if (!is.null(res.ca$quali.sup) & is.na(test.invisible[5])) {
      coo <- rbind(coo,coord.quali.sup)
      ipch <- c(ipch,rep(22,nrow(coord.quali.sup)))
      coll <- c(coll,col.quali.sup)
      fonte <- c(fonte,rep(2,nrow(coord.quali.sup)))
      labe <- c(labe,rownames(coord.quali.sup))
    }
    if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll,...)
    if (any(labe!="")){
	  if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
      if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],shadotext=shadowtext,...)
      if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)
	}
	if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll,...)

### ajout legend
    if (habillage != "none") {
      L <- list(x="topleft", legend = levels(res.ca$call$Xtot[, habillage]), text.col = 1+(1:nlevels(res.ca$call$Xtot[, habillage])), cex = par("cex") * 0.8)
      L <- modifyList(L, legend)
      do.call(graphics::legend, L)
    }
}
	
	if (choix == "quanti.sup") {
     if (!is.null(res.ca$quanti.sup)) {
      if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
	  if (is.null(title)) title <- "Supplementary variables on the CA factor map"
      plot(0, 0, main = title, xlab = paste("Dim ",axes[1]," (",format(res.ca$eig[axes[1],2],nsmall=2,digits=2),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",format(res.ca$eig[axes[2],2],nsmall=2,digits=2),"%)",sep=""), xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), col = "white", asp=1, ...)
      abline(v=0,lty=2,...)
      abline(h=0,lty=2,...)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle,...)
      lines(x.cercle, y = -y.cercle,...)
      for (v in 1:nrow(res.ca$quanti.sup$coord)) {
        arrows(0, 0, res.ca$quanti.sup$coord[v, axes[1]], res.ca$quanti.sup$coord[v, axes[2]], length = 0.1, angle = 15, code = 2, col = col.quanti.sup,...)
        if (abs(res.ca$quanti.sup$coord[v,axes[1]])>abs(res.ca$quanti.sup$coord[v,axes[2]])){
          if (res.ca$quanti.sup$coord[v,axes[1]]>=0) pos<-4
          else pos<-2
        }
        else {
          if (res.ca$quanti.sup$coord[v,axes[2]]>=0) pos<-3
          else pos<-1
        }
		if((!is.null(label)) && (label=="all" | "quanti.sup" %in% label)){
        	autoLab(res.ca$quanti.sup$coord[v, axes[1]], y = res.ca$quanti.sup$coord[v, axes[2]], labels = rownames(res.ca$quanti.sup$coord)[v], col = col.quanti.sup,...)
		}
      }
    }
    }

}
