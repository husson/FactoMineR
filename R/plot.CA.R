utils::globalVariables(c("y","xfin","yfin","coul"))
plot.CA <- function (x, axes = c(1, 2),
                     xlim = NULL, ylim = NULL, invisible = c("none","row", "col", "row.sup", "col.sup","quali.sup"), choix = c("CA","quanti.sup"), col.row = "blue",
                     col.col = "red", col.row.sup = "darkblue", col.col.sup = "darkred",col.quali.sup ="magenta",
                     col.quanti.sup="blue",label = c("all","none","row", "row.sup", "col","col.sup", "quali.sup"), title = NULL, palette=NULL, 
                     autoLab = c("auto","yes","no"),new.plot=FALSE, selectRow = NULL, selectCol = NULL,
                     unselect = 0.7,shadowtext = FALSE, habillage = "none", legend = list(bty = "y", x = "topleft"), graph.type = c("ggplot","classic"), ggoptions= NULL, ...) {
  
  res.ca <- x
  argument <- list(...)
  if (!is.null(argument[["cex"]]) & is.null(ggoptions["size"]))  ggoptions["size"] <- 4*argument$cex
  ggoptions_default <- list(size = 4, point.shape = 19, line.lty = 2, line.lwd = 0.5, line.color = "black", segment.lty = 1, segment.lwd = 0.5, circle.lty = 1, circle.lwd = 0.5, circle.color = "black", low.col.quanti = "blue", high.col.quanti = "red3")
  if (!is.null(ggoptions[1])) ggoptions_default[names(ggoptions)] = ggoptions[names(ggoptions)]
  old.palette <- palette()
  if (is.null(palette)) palette <- c("black", "red", "green3", "blue", "magenta", "darkgoldenrod","darkgray", "orange", "cyan", "violet", "lightpink", "lavender", "yellow", "darkgreen","turquoise", "lightgrey", "lightblue", "darkkhaki","darkmagenta","lightgreen", "darkolivegreen", "lightcyan", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen","darkslateblue", "darkslategray", "darkslategrey","darkturquoise", "darkviolet", "lightgray", "lightsalmon","lightyellow", "maroon")
  palette(palette)   # that is necessary
  if (!inherits(res.ca, "CA")) stop("non convenient data")
  if (is.numeric(unselect)) if ((unselect>1)|(unselect<0)) stop("unselect should be betwwen 0 and 1")
  label <- match.arg(label,c("all","none","row", "row.sup", "col","col.sup", "quali.sup"),several.ok=TRUE)
  choix <- match.arg(choix,c("CA","quanti.sup"))
  choix <- tolower(choix)
  autoLab <- match.arg(autoLab,c("auto","yes","no"))
  graph.type <- match.arg(graph.type[1],c("ggplot","classic"))
  if (autoLab=="yes") autoLab=TRUE
  if (autoLab=="no") autoLab=FALSE
  invisible <- match.arg(invisible,c("none","row", "col", "row.sup", "col.sup","quali.sup"),several.ok=TRUE)
  if ("none"%in%invisible) invisible = NULL
  if (graph.type == "ggplot"){
    lab.x <- paste("Dim ",axes[1]," (",format(res.ca$eig[axes[1],2],nsmall=2,digits=2),"%)",sep="")
    lab.y <- paste("Dim ",axes[2]," (",format(res.ca$eig[axes[2],2],nsmall=2,digits=2),"%)",sep="")
    theme <- theme(
      axis.title = element_text(hjust = 1, size = if (is.null(argument[["cex.axis"]])) {10} else {10*argument$cex.axis},face = 2),
      plot.title = element_text(hjust = 0.5, size = if (is.null(argument[["cex.main"]])) {11} else {11*argument$cex.main},face = 2),
      legend.position = ifelse(legend$x %in% c("bottom","up","right","left"), legend$x, "right"),
      legend.box.spacing=unit(0.1, 'cm'),legend.margin=margin()
    )
  }
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
	nullxlimylim <- (is.null(xlim) & is.null(ylim))
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
      # xlim <- c(xmin, xmax) * 1.2
      xlim <- c(xmin, xmax)
	  xlim <- (xlim-mean(xlim))*1.2 + mean(xlim)
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
      # ylim <- c(ymin, ymax) * 1.2
      ylim <- c(ymin, ymax)
      ylim <- (ylim-mean(ylim))*1.2 + mean(ylim)
    }
    if (nullxlimylim & diff(xlim)/diff(ylim)>3) ylim <- (ylim-mean(ylim))*diff(xlim)/diff(ylim)/3 + mean(ylim)
    if (nullxlimylim & diff(xlim)/diff(ylim)<1/2) xlim <- (xlim-mean(xlim))*diff(ylim)/diff(xlim)/2 + mean(xlim)
    if(graph.type=="ggplot") nudge_y <- (ylim[2] - ylim[1])*0.03
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
    if (graph.type == "classic"){
      plot(0, 0, main = titre, xlab = paste("Dim ",axes[1]," (",format(res.ca$eig[axes[1],2],nsmall=2,digits=2),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",format(res.ca$eig[axes[2],2],nsmall=2,digits=2),"%)",sep=""), xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
      abline(h=0,lty=2,...)
      abline(v=0,lty=2,...)
    }
    if (habillage != "none"){
      liste.quali <- colnames(res.ca$call$Xtot)[res.ca$call$quali.sup]
      if (is.numeric(habillage)) nom.quali <- colnames(res.ca$call$Xtot)[habillage]
      else nom.quali <- habillage
      if (!(nom.quali %in% liste.quali) & (graph.type=="classic")){
        stop("The variable ", habillage, " is not qualitative")
        if (is.null(res.ca$row.sup)) col.row <- 1+as.integer(res.ca$call$Xtot[,nom.quali])
        else col.row <- 1+as.integer(res.ca$call$Xtot[-res.ca$call$row.sup,nom.quali])
        col.quali.sup <- rep(1,nrow(res.ca$quali.sup$coord))
        col.quali.sup[which(rownames(res.ca$quali.sup$coord)%in%paste(colnames(res.ca$call$Xtot[,nom.quali,drop=FALSE]),levels(res.ca$call$Xtot[,nom.quali]),sep="."))] <- 2:(nlevels(res.ca$call$Xtot[,nom.quali])+1)
      }}
    if (length(col.row)==1) col.row <- rep(col.row,nrow(coord.row))
    if (length(col.col)==1) col.col  <- rep(col.col,nrow(coord.col))
    if ((!is.null(res.ca$row.sup))&(length(col.row.sup)==1)) col.row.sup  <- rep(col.row.sup,nrow(coord.row.sup))
    if ((!is.null(res.ca$col.sup))&(length(col.col.sup)==1)) col.col.sup <- rep(col.col.sup,nrow(coord.col.sup))
    if ((!is.null(res.ca$quali.sup))&(length(col.quali.sup)==1)) col.quali.sup <- rep(col.quali.sup,nrow(coord.quali.sup))
    coo <- ipch <- labe <- coll <- fonte <- NULL
    df_rowa <- df_rowb <- df_cola <- df_colb <- df_quali.sup <- NULL
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
      df_rowa <- data.frame(labe,coord.row,coll,ipch,fonte)
      names(df_rowa) <- c("labe", paste("Dim",axes[1],sep=""),paste("Dim",axes[2],sep=""), "coll","ipch","fonte")
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
      df_cola <- data.frame(labe2,coord.col,coll2,rep(17,nrow(coord.col)),rep(1,nrow(coord.col)))
      names(df_cola) <- c("labe", paste("Dim",axes[1],sep=""),paste("Dim",axes[2],sep=""), "coll","ipch","fonte")
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
      df_colb <- data.frame(labe2,coord.col.sup,coll2,rep(17,nrow(coord.col.sup)),rep(3,nrow(coord.col.sup)))
      names(df_colb) <- c("labe", paste("Dim",axes[1],sep=""),paste("Dim",axes[2],sep=""), "coll","ipch","fonte")
      
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
      df_rowb <- data.frame(labe2,coord.row.sup,coll2,rep(20,nrow(coord.row.sup)),rep(3,nrow(coord.row.sup)))
      names(df_rowb) <- c("labe", paste("Dim",axes[1],sep=""),paste("Dim",axes[2],sep=""), "coll","ipch","fonte")
      
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
    }
    if (!is.null(res.ca$quali.sup) & is.na(test.invisible[5])) {
      coo <- rbind(coo,coord.quali.sup)
      ipch <- c(ipch,rep(22,nrow(coord.quali.sup)))
      coll <- c(coll,col.quali.sup)
      fonte <- c(fonte,rep(2,nrow(coord.quali.sup)))
      labe <- c(labe,rownames(coord.quali.sup))
      
      df_quali.sup <- data.frame(rownames(coord.quali.sup),coord.quali.sup,col.quali.sup,rep(22,nrow(coord.quali.sup)),rep(2,nrow(coord.quali.sup)))
      names(df_quali.sup) <- c("labe", paste("Dim",axes[1],sep=""),paste("Dim",axes[2],sep=""), "coll","ipch","fonte")
    }
    df_row <- rbind(df_rowa,df_rowb)
    df_col <- rbind(df_cola,df_colb)
    
    if(graph.type == "classic"){
      if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll,...)
      if (any(labe!="")){
        if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
        if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],shadotext=shadowtext,...)
        if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)
      }
      if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll,...)
    }
    ### ajout legend
    #if (!(habillage %in% c("none","cos2","contrib"))) {
    if ((habillage != "none") & (graph.type == "classic")) {
      
      L <- list(x="topleft", legend = levels(res.ca$call$Xtot[, habillage]), text.col = 1+(1:nlevels(res.ca$call$Xtot[, habillage])), cex = par("cex") * 0.8)
      L <- modifyList(L, legend)
      do.call(graphics::legend, L)
    }
    
    if(graph.type == "ggplot"){
      
      text_col<- text<- NULL
      df_ind <- data.frame(labe,coo,coll,ipch,fonte)
      if(dim(df_ind)[1] == 0) df_ind <- NULL
      if(!is.null(df_ind)) df_ind[,5][which(df_ind[,5] == 20)] = 19
        gg_graph <- ggplot() +
          coord_fixed(ratio = 1) +
          xlab(lab.x) + ylab(lab.y) +
          xlim(xlim) + ylim(ylim) +
          geom_hline(yintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
          geom_vline(xintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
          theme_light() + 
          ggoptions_default$theme +
          ggtitle(titre)
      
      if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
      if (habillage == "none" & !is.null(df_ind)){
        gg_graph <- gg_graph +
          geom_point(aes(x=df_ind[,2], y=df_ind[,3]), color= df_ind[,4], shape = df_ind[,5])
        if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1]), force = 1, max.iter = 1000,size = ggoptions_default$size, color = df_ind[,4], fontface = df_ind[,6])
        else{text <- geom_text(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1]), size = ggoptions_default$size, color = df_ind[,4], hjust = (-sign(df_ind[,2])+1)/2, vjust = -sign(df_ind[,3])*0.75+0.25, fontface = df_ind[,6])}
      }
      if(habillage != "none"){
        if(class(habillage) == "numeric") habillage = colnames(res.ca$call$Xtot)[habillage]
        if (habillage %in% liste.quali){
        gg_graph <- gg_graph +
            geom_point(aes(x=df_row[,2], y=df_row[,3], color = (res.ca$call$Xtot)[rownames(df_row),habillage]), shape = df_row[,5]) + 
            geom_point(aes(x=df_col[,2], y=df_col[,3]), color = col.col[1], shape = df_col[,5]) +
            scale_color_manual(values = palette[1:length(levels((res.ca$call$Xtot)[,habillage]))]) +
            labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], habillage))
          if (autoLab){ text <- ggrepel::geom_text_repel(aes(x=df_row[,2], y=df_row[,3], label=df_row[,1], color = (res.ca$call$Xtot)[rownames(df_row),habillage]), size = ggoptions_default$size, show.legend = FALSE,fontface=df_row[,6])
          text_col <- ggrepel::geom_text_repel(aes(x=df_col[,2], y=df_col[,3], label=df_col[,1]), color = col.col[1], size = ggoptions_default$size, show.legend = FALSE,fontface=df_col[,6])}
          else{text <- geom_text(aes(x=df_row[,2], y=df_row[,3], label=df_row[,1], color = (res.ca$call$Xtot)[rownames(df_row),habillage]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_row[,2])+1)/2, vjust = -sign(df_row[,3])*0.75+0.25,fontface=df_row[,6])
          text_col <- geom_text(aes(x=df_col[,2], y=df_col[,3], label=df_col[,1]), color = col.col[1], size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_col[,2])+1)/2, vjust = -sign(df_col[,3])*0.75+0.25, fontface=df_col[,6])}
        }
        if(habillage=="cos2"){
          df_ind <- rbind(df_rowa,df_cola,df_colb,df_rowb)
          df_ind[,5][which(df_ind[,5] == 20)] = 19
          coll_col <- coll_row <- coll_col.sup <- coll_row.sup <- NULL
          if(!is.null(res.ca$row$cos2) & (is.na(test.invisible[1]))) coll_row <- apply(res.ca$row$cos2[,axes,drop = FALSE],1,FUN=sum)
          if(!is.null(res.ca$col$cos2) & (is.na(test.invisible[2]))) coll_col <- apply(res.ca$col$cos2[,axes,drop = FALSE],1,FUN=sum)
          if(!is.null(res.ca$row.sup$cos2) & (is.na(test.invisible[3]))) coll_row.sup <- apply(res.ca$row.sup$cos2[,axes,drop = FALSE],1,FUN=sum)
          if(!is.null(res.ca$col.sup$cos2) & (is.na(test.invisible[4]))) coll_col.sup <- apply(res.ca$col.sup$cos2[,axes,drop = FALSE],1,FUN=sum)
          coll_quanti <- c(coll_row,coll_col,coll_col.sup,coll_row.sup)
          df_ind[,4] <- coll_quanti
          
        gg_graph <- gg_graph +
            geom_point(aes(x=df_ind[,2], y=df_ind[,3], color = df_ind[,4]), shape = df_ind[,5]) + 
            scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
            labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "cos2"))
          if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1], color = df_ind[,4]), size = ggoptions_default$size, show.legend = FALSE,fontface=df_ind[,6])
          else{text <- geom_text(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1], color = df_ind[,4]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind[,2])+1)/2, vjust = -sign(df_ind[,3])*0.75+0.25, fontface=df_ind[,6])}
        }
        if(habillage=="contrib"){
          df_ind <- rbind(df_rowa,df_cola,df_colb,df_rowb)
          df_ind[,5][which(df_ind[,5] == 20)] = 19
          
          coll_row <- coll_col <- coll_col.sup <- coll_row.sup <- coll_quali.sup <- NULL
          if(is.na(test.invisible[1]))  coll_row <- res.ca$row$contrib[,axes[1]]*res.ca$eig[axes[1],1] + res.ca$row$contrib[,axes[2]]*res.ca$eig[axes[2],1]
          if(is.na(test.invisible[2])) coll_col <- res.ca$col$contrib[,axes[1]]*res.ca$eig[axes[1],1] + res.ca$col$contrib[,axes[2]]*res.ca$eig[axes[2],1]
          if(!is.null(res.ca$row.sup) & is.na(test.invisible[3])) coll_row.sup <- rep(0, nrow(res.ca$row.sup$coord))
          if(!is.null(res.ca$col.sup) & is.na(test.invisible[4])) coll_col.sup <- rep(0, nrow(res.ca$col.sup$coord))

          coll_quanti <- c(coll_row,coll_col,coll_col.sup,coll_row.sup)
          df_ind[,4] <- coll_quanti
        gg_graph <- gg_graph +
            geom_point(aes(x=df_ind[,2], y=df_ind[,3], color = df_ind[,4]), shape = df_ind[,5]) + 
            scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
            labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "Ctr"))
          if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1], color = df_ind[,4]), size = ggoptions_default$size, show.legend = FALSE,fontface=df_ind[,6])
          else{text <- geom_text(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1], color = df_ind[,4]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind[,2])+1)/2, vjust = -sign(df_ind[,3])*0.75+0.25, fontface=df_ind[,6])}
        }
        if(!is.null(res.ca$quali.sup) & is.na(test.invisible[5])){
          if(habillage %in% c("cos2","contrib")){
            gg_graph <- gg_graph +
              geom_point(aes(x=df_quali.sup[,2], y=df_quali.sup[,3]), color = df_quali.sup[,4], size = ggoptions_default$size/2.8, shape = df_quali.sup[,5])
            if (autoLab) text_quali.sup <- ggrepel::geom_text_repel(aes(x=df_quali.sup[,2], y=df_quali.sup[,3], label=df_quali.sup[,1]), color = df_quali.sup[,4], size = ggoptions_default$size, fontface=df_quali.sup[,6])
            else{text_quali.sup <- geom_text(aes(x=df_quali.sup[,2], y=df_quali.sup[,3], label=df_quali.sup[,1]), color = df_quali.sup[,4], size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_quali.sup[,2])+1)/2, vjust = -sign(df_quali.sup[,3])*0.75+0.25, fontface=df_quali.sup[,6])}
            gg_graph <- gg_graph + text_quali.sup
          } else{
            if (habillage %in% liste.quali) {
              levels(res.ca$call$Xtot[,habillage]) = paste(habillage,".",levels(res.ca$call$Xtot[,habillage]),sep="")
              gg_graph <- gg_graph +
                geom_point(aes(x = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),2], y = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),3]), size = ggoptions_default$size/2.8, color = palette[1:length(levels(res.ca$call$Xtot[,habillage]))], shape = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),5])
              if (autoLab) text_quali.sup.hab <- ggrepel::geom_text_repel(aes(x = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),2], y = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),3], label=levels(res.ca$call$Xtot[,habillage])), color = palette[1:length(levels(res.ca$call$Xtot[,habillage]))], size = ggoptions_default$size, fontface = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),6])
              else{text_quali.sup.hab <- geom_text(aes(x = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),2], y = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),3], label=levels(res.ca$call$Xtot[,habillage])), color = palette[1:length(levels(res.ca$call$Xtot[,habillage]))], size = ggoptions_default$size, fontface = df_quali.sup[levels(res.ca$call$Xtot[,habillage]),6],nudge_y=nudge_y)}
              gg_graph <- gg_graph + text_quali.sup.hab
            }
            text_quali.sup <- NULL
            if(length(liste.quali) > 1){
              df_quali.nohab <- df_ind[which(!(rownames(res.ca$quali.sup$coord) %in% levels(res.ca$call$Xtot[,habillage]))), ,drop = FALSE]
              gg_graph <- gg_graph +
                geom_point(aes(x = df_quali.nohab[,2], y = df_quali.nohab[,3]), size = ggoptions_default$size/2.8, color = col.quali.sup[1], shape = 0)
              if (autoLab) text_quali.sup <- ggrepel::geom_text_repel(aes(x = df_quali.nohab[,2], y = df_quali.nohab[,3], label = df_quali.nohab[,1]), color = col.quali.sup[1], size = ggoptions_default$size, fontface = 2)
              else{text_quali.sup <- geom_text(aes(x = df_quali.nohab[,2], y = df_quali.nohab[,3], label = df_quali.nohab[,1]), color = col.quali.sup[1], size = ggoptions_default$size, fontface = 2, hjust = (-sign(df_quali.nohab[,2])+1)/2, vjust = -sign(df_quali.nohab[,3])*0.75+0.25,)}
            }
            gg_graph <- gg_graph + text_quali.sup
          }
        }
      }
      gg_graph <- gg_graph + theme + text + text_col
    }
  }
  
  if (choix == "quanti.sup") {
    if (is.null(title)) title <- "Supplementary variables on the CA factor map"
    if (!is.null(res.ca$quanti.sup)) {
      if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
      if (graph.type == "classic"){
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
      if(graph.type=="ggplot"){
        if (autoLab=="auto") autoLab = (length(which(rownames(res.ca$quanti.sup$coord)!=""))<50)
        df_var <- data.frame(rownames(res.ca$quanti.sup$coord),res.ca$quanti.sup$coord[,axes[1]],res.ca$quanti.sup$coord[,axes[2]])
        circle <- annotate("path",
                           x=0+1*cos(seq(0,2*pi,length.out=100)),
                           y=0+1*sin(seq(0,2*pi,length.out=100)),
                           lty = ggoptions_default$circle.lty,
                           lwd = ggoptions_default$circle.lwd,
                           color = ggoptions_default$circle.color)
          gg_graph <- ggplot() + 
            coord_fixed(ratio = 1) + 
            geom_line(aes(x=x, y=y), data=data.frame(x=-1:1,y=0),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
            geom_line(aes(x=x, y=y), data=data.frame(x=0,y=-1:1),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
            theme_light()  + 
            ggoptions_default$theme
        if(habillage=="none"){
          gg_graph <- gg_graph + 
            aes(x=df_var[,2], y=df_var[,3]) +
            geom_segment(aes(x=0,y=0,xend=df_var[,2], yend=df_var[,3]),arrow=arrow(length=unit(0.2,"cm")), lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd, color = col.quanti.sup) 
          if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1]), size = ggoptions_default$size, color = col.quanti.sup)
          else{text <- geom_text(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1]), size = ggoptions_default$size, color = col.quanti.sup, hjust = (-sign(df_var[,2])+1)/2, vjust = -sign(df_var[,3])*0.75+0.25)}
        }
        if(habillage=="cos2"){
          gg_graph <- gg_graph + 
            aes(x=df_var[,2], y=df_var[,3], color = res.ca$quanti.sup$cos2[,axes[1]] + res.ca$quanti.sup$cos2[,axes[2]]) +
            geom_segment(aes(x=0,y=0,xend=df_var[,2], yend=df_var[,3]),arrow=arrow(length=unit(0.2,"cm")),lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd) + 
            scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
            labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "cos2"))
          if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1], color = res.ca$quanti.sup$cos2[,axes[1]] + res.ca$quanti.sup$cos2[,axes[2]]), size = ggoptions_default$size)
          else{text <- geom_text(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1], color = res.ca$quanti.sup$cos2[,axes[1]] + res.ca$quanti.sup$cos2[,axes[2]]), size = ggoptions_default$size, hjust = (-sign(df_var[,2])+1)/2, vjust = -sign(df_var[,3])*0.75+0.25)}
        }
        gg_graph <- gg_graph + text + theme + circle + xlab(lab.x) + ylab(lab.y) + ggtitle(title)
      }
    }
  }
  palette(old.palette)
  if (graph.type == "ggplot")  return(gg_graph)
}
