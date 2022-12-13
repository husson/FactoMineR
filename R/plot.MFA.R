plot.MFA <- function (x, axes = c(1, 2), choix = c("ind","var","group","axes","freq"), ellipse = NULL, ellipse.par = NULL, 
                   lab.grpe = TRUE, lab.var = TRUE, lab.ind = TRUE, lab.par = FALSE, lab.col = TRUE, ncp=2,
                   habillage = "group", col.hab = NULL, invisible = c("none","ind", "ind.sup", "quanti","quanti.sup","quali","quali.sup","row", "row.sup","col", "col.sup"), partial = NULL, 
                   lim.cos2.var = 0., chrono = FALSE, xlim = NULL, ylim = NULL, 
                   title = NULL, palette = NULL, autoLab = c("auto","yes","no"),new.plot = FALSE, select = NULL,
                   unselect = 0.7,shadowtext=FALSE, legend = list(bty = "y", x = "topleft"), graph.type = c("ggplot","classic"), ggoptions = NULL, ...) 
{
  res.mfa <- x
  argument <- list(...)
  if (!is.null(argument[["cex"]]) & is.null(ggoptions["size"]))  ggoptions["size"] <- 4*argument$cex
  if (!inherits(res.mfa, "MFA")) stop("non convenient data")
  if (is.numeric(unselect)) if ((unselect>1)|(unselect<0)) stop("unselect should be betwwen 0 and 1")
  autoLab <- match.arg(autoLab,c("auto","yes","no"))
  if (autoLab=="yes") autoLab <- TRUE
  if (autoLab=="no") autoLab <- FALSE
  choix <- match.arg(choix,c("ind","var","group","axes","freq"))
  graph.type <- match.arg(graph.type[1],c("ggplot","classic"))
  ggoptions_default <- list(size = 4, point.shape = 19, line.lty = 2, line.lwd = 0.5, line.color = "black", segment.lty = 1, segment.lwd = 0.5, circle.lty = 1, circle.lwd = 0.5, circle.color = "black", low.col.quanti = "blue", high.col.quanti = "red3")
  if (!is.null(ggoptions[1])) ggoptions_default[names(ggoptions)] <- ggoptions[names(ggoptions)]
  invisible <- match.arg(invisible,c("none","ind", "ind.sup", "quanti","quanti.sup","quali","quali.sup","row", "row.sup","col", "col.sup"),several.ok=TRUE)
  if ("none"%in%invisible) invisible <- NULL
  lab.x <- paste("Dim ",axes[1]," (",format(res.mfa$eig[axes[1],2],nsmall=2,digits=2),"%)",sep="")
  lab.y <- paste("Dim ",axes[2]," (",format(res.mfa$eig[axes[2],2],nsmall=2,digits=2),"%)",sep="")
  group <- res.mfa$call$group
  nbre.grpe <- length(group)
  type <- res.mfa$call$type
  type.act <- type
  num.group.sup <- NULL
  old.palette <- palette()
  if (is.null(palette)) palette <- c("black", "red", "green3", "blue", "magenta", "darkgoldenrod","darkgray", "orange", "cyan", "violet", "lightpink", "lavender", "yellow", "darkgreen","turquoise", "lightgrey", "lightblue", "darkkhaki","darkmagenta","lightgreen", "darkolivegreen", "lightcyan", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen","darkslateblue", "darkslategray", "darkslategrey","darkturquoise", "darkviolet", "lightgray", "lightsalmon","lightyellow", "maroon")
  palette(palette)
  if (!is.null(res.mfa$call$num.group.sup)) {
    num.group.sup <- res.mfa$call$num.group.sup
    nbre.grpe.sup <- length(num.group.sup)
    type.sup <- type[num.group.sup]
    type.act <- type[-num.group.sup]
    nbre.grpe <- nbre.grpe - length(num.group.sup)
  }
  if (graph.type == "ggplot"){
    theme <- theme(
      axis.title = element_text(hjust = 1, size = if (is.null(argument[["cex.axis"]])) {10} else {10*argument$cex.axis},face = 2),
      plot.title = element_text(hjust = 0.5, size = if (is.null(argument[["cex.main"]])) {11} else {11*argument$cex.main},face = 2),
      legend.position = ifelse(legend$x %in% c("bottom","up","right","left"), legend$x, "right"),
      legend.box.spacing=unit(0.1, 'cm'),legend.margin=margin()
    )
  }
  if (choix == "axes") {
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    if (is.null(title)) title <- "Partial axes"
    if(graph.type == "classic"){
      plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), col = "white", asp = 1, main = title,...)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle,...)
      lines(x.cercle, y = -y.cercle,...)
      abline(v = 0, lty = 2,...)
      abline(h = 0, lty = 2,...)
    }
    ## 3 lines added
    selectByNCP <- grep("Dim1",rownames(res.mfa$partial.axes$coord))
    for (k in 2:ncp) selectByNCP<- c(selectByNCP,grep(paste("Dim",k,sep=""),rownames(res.mfa$partial.axes$coord)))
    selectByNCP <- sort(selectByNCP)
    res.mfa$partial.axes$coord <- res.mfa$partial.axes$coord[selectByNCP,, drop = FALSE]
    res.mfa$partial.axes$contrib <- res.mfa$partial.axes$contrib[selectByNCP,, drop = FALSE]
    res.mfa$partial.axes$cos2 <- res.mfa$partial.axes$cos2[selectByNCP,, drop = FALSE]
    ## end of 3 lines added
    coord.axes <- res.mfa$partial.axes$coord[, axes, drop = FALSE]
    if (!is.null(select)) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.mfa$partial.axes$coord)%in%select)!=0) selection <- which(rownames(res.mfa$partial.axes$coord)%in%select)
        else {
          if (grepl("contrib",select)) selection <- (rev(order(res.mfa$partial.axes$contrib[,axes[1],drop=FALSE]*res.mfa$eig[axes[1],1]+res.mfa$partial.axes$contrib[,axes[2],drop=FALSE]*res.mfa$eig[axes[2],1])))[1:min(nrow(res.mfa$partial.axes$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("coord",select)) selection <- (rev(order(apply(res.mfa$partial.axes$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$partial.axes$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (is.integer(select)) selection <- select
        }  
      }
    }
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) < length(group)) {
        if (is.null(res.mfa$call$num.group.sup)) col.hab <- 2:(length(group) + 1)
        else {
          col.hab[which(!seq_len(length(group))%in%(res.mfa$call$num.group.sup))] <- 2:(1+length(group)-length(res.mfa$call$num.group.sup))
          col.hab[res.mfa$call$num.group.sup] <- length(group)-length(res.mfa$call$num.group.sup)+1+seq_len(length(res.mfa$call$num.group.sup))
        }
      }
      i <- 1
      couleur.axes <- col.hab[i]
      auxil <- strsplit(rownames(res.mfa$partial.axes$coord)[1], ".", fixed = TRUE)[[1]]
      auxil2 <- auxil[length(auxil)]
      for (j in 2:nrow(res.mfa$partial.axes$coord)) {
        auxil <- strsplit(rownames(res.mfa$partial.axes$coord)[j], ".", fixed = TRUE)[[1]]
        if (auxil2 != auxil[length(auxil)]) {
          i <- i + 1
          auxil2 <- auxil[length(auxil)]
        }
        couleur.axes <- c(couleur.axes, col.hab[i])
      }
    } else {
      couleur.axes <- NULL
      for (i in seq_len(length(group))) couleur.axes <- c(couleur.axes, rep("black", ncol(res.mfa$partial.axes$coord)))
    }
    posi <- coll <- NULL
    col.legend <- unique(couleur.axes)
    if (!is.null(select)){
      coord.axes <- coord.axes[selection,,drop=FALSE]
      couleur.axes <- couleur.axes[selection]
    }
    for (v in seq_len(nrow(coord.axes))) {
      if(graph.type == "classic"){
        arrows(0, 0, coord.axes[v, 1], coord.axes[v, 2], length = 0.1, angle = 15, code = 2, col = couleur.axes[v], ...)
      }
      if (abs(coord.axes[v,1])>abs(coord.axes[v,2])){
        if (coord.axes[v,1]>=0) posi<-c(posi,4)
        else posi<-c(posi,2)
      } else {
        if (coord.axes[v, 2] >= 0) posi <- c(posi,3)
        else posi <- c(posi,1)
      }
      labe <- rownames(coord.axes)
    }
    if (autoLab=="auto") autoLab <- (length(labe)<50)
    if(graph.type == "classic"){
      if (autoLab==FALSE) text(coord.axes[, 1], y = coord.axes[, 2], labels = labe, pos = posi, col = couleur.axes,...)
      if (autoLab==TRUE) autoLab(coord.axes[, 1], y = coord.axes[, 2], labels = labe, col=couleur.axes, shadotext=shadowtext,...)
    }
    #        if (habillage == "group") legend("topleft", legend = rownames(res.mfa$group$Lg)[-length(rownames(res.mfa$group$Lg))], text.col = unique(couleur.axes), ...)
    if (habillage == "group") {
      L <- list(x="topleft", legend = rownames(res.mfa$group$Lg)[-length(rownames(res.mfa$group$Lg))], text.col = col.legend)
      L <- modifyList(L, legend)
      if(graph.type == "classic") do.call(graphics::legend, L)
    }
    if(graph.type == "ggplot"){
      circle <- annotate("path",
                         x=0+1*cos(seq(0,2*pi,length.out=100)),
                         y=0+1*sin(seq(0,2*pi,length.out=100)),
                         lty = ggoptions_default$circle.lty,
                         lwd = ggoptions_default$circle.lwd,
                         color = ggoptions_default$circle.color)
      df_axes <- data.frame(labe,coord.axes)
      group2 <- strsplit(as.character(df_axes[,1]),".",fixed=TRUE)
      group <- NULL
      for (i in seq_len(length(group2))){group <- c(group, group2[[i]][2])}
      if(habillage == "group"){
      df_axes <- data.frame(df_axes, couleur.axes, group)
      if(!is.null(select)) df_axes <- df_axes[order(df_axes[,4]),]
      gg_graph <- ggplot() + 
        aes(x=df_axes[,2], y=df_axes[,3]) +
        coord_fixed(ratio = 1) + 
        geom_line(aes(x=x, y=y), data=data.frame(x=-1:1,y=0),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
        geom_line(aes(x=x, y=y), data=data.frame(x=0,y=-1:1),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
        geom_segment(aes(x=0,y=0,xend=df_axes[,2], yend=df_axes[,3],color = as.factor(as.character(df_axes[,4]))), arrow=arrow(length=unit(0.2,"cm")), lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd) + 
        xlab(lab.x) + ylab(lab.y) +
        ggtitle(title) +
        labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "")) +
        theme_light()  +
        ggoptions_default$theme
      if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_axes[,2], y=df_axes[,3],label=df_axes[,1], color = as.factor(as.character(df_axes[,4]))), size = ggoptions_default$size, show.legend = FALSE)
      else{text <- geom_text(aes(x=df_axes[,2], y=df_axes[,3],label=df_axes[,1]),color = palette[as.numeric(as.character(df_axes[,4]))], size = ggoptions_default$size, hjust = (-sign(df_axes[,2])+1)/2, vjust = -sign(df_axes[,3])*0.75+0.25)}
      
      if(is.null(select)) gg_graph <- gg_graph + scale_color_manual(values = palette[L$text.col[order(L$text.col)]], labels = unique(df_axes[,5])[order(unique(df_axes[,4]))])
      if(!is.null(select)) gg_graph <- gg_graph + scale_color_manual(values = palette[unique(df_axes[,4])], labels = unique(df_axes[,5]))
      
      }
      if(habillage == "none"){
        gg_graph <- ggplot() + 
          aes(x=df_axes[,2], y=df_axes[,3]) +
          coord_fixed(ratio = 1) + 
          geom_line(aes(x=x, y=y), data=data.frame(x=-1:1,y=0),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
          geom_line(aes(x=x, y=y), data=data.frame(x=0,y=-1:1),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
          geom_segment(aes(x=0,y=0,xend=df_axes[,2], yend=df_axes[,3]), arrow=arrow(length=unit(0.2,"cm")), lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd) + 
          xlab(lab.x) + ylab(lab.y) +
          ggtitle(title) +
          theme_light()  +
          ggoptions_default$theme
        if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_axes[,2], y=df_axes[,3],label=df_axes[,1]), size = ggoptions_default$size, show.legend = FALSE)
        else{text <- geom_text(aes(x=df_axes[,2], y=df_axes[,3],label=df_axes[,1]), size = ggoptions_default$size, hjust = (-sign(df_axes[,2])+1)/2, vjust = -sign(df_axes[,3])*0.75+0.25)}
        
      }
      gg_graph <- gg_graph + text + theme + circle
    }
  }
  if (choix == "group") {
    coord.actif <- res.mfa$group$coord[, axes, drop = FALSE]
    if (!is.null(res.mfa$group$coord.sup))  coord.illu <- res.mfa$group$coord.sup[, axes, drop = FALSE]
    ## Debut ajout 2015/04/23
    selection <- selectionS <- NULL
    if (!is.null(select)) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.mfa$group$coord)%in%select)+sum(rownames(res.mfa$group$coord.sup)%in%select)!=0) selection <- which(rownames(res.mfa$group$coord)%in%select)
        else {
          if (grepl("contrib",select)) selection <- (rev(order(res.mfa$group$contrib[,axes[1]]*res.mfa$eig[axes[1],1]+res.mfa$group$contrib[,axes[2]]*res.mfa$eig[axes[2],1])))[1:min(nrow(res.mfa$group$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("coord",select)) selection <- (rev(order(apply(res.mfa$group$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$group$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.mfa$group$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$group$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selection <- which(apply(res.mfa$group$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selection <- select
        }  
      }
    }
    if ((!is.null(select))&(!is.null(res.mfa$group$coord.sup))) {
      if (mode(select)=="numeric") selectionS <- select
      else {
        if (sum(rownames(res.mfa$group$coord)%in%select)+sum(rownames(res.mfa$group$coord.sup)%in%select)!=0) selectionS <- which(rownames(res.mfa$group$coord.sup)%in%select)
        else {
          if (grepl("contrib",select)) selectionS <- NULL
          if (grepl("coord",select)) selectionS <- (rev(order(apply(res.mfa$group$coord.sup[,axes]^2,1,sum))))[1:min(nrow(res.mfa$group$coord.sup),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.mfa$group$cos2.sup[,axes],1,sum))))[1:min(nrow(res.mfa$group$coord.sup),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selectionS <- which(apply(res.mfa$group$cos2.sup[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selectionS <- select
        }  
      }
    }
    ## Fin ajout 2015/04/23
    
    if (habillage == "group" & is.null(col.hab)) col.hab <- (2:(length(group) + 1))
    if (length(col.hab)==1) col.hab <- rep(col.hab,length(group))
    if (is.null(col.hab)) {
      col.hab <- rep("darkred", nrow(coord.actif))
      if (!is.null(res.mfa$group$coord.sup))  col.hab <- c(col.hab, rep("darkolivegreen", nrow(coord.illu)))
    }
    
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    coo <- labe <- coll <- ipch <- fonte <- NULL
    if (is.null(xlim)) xlim <- c(0,1)
    if (is.null(ylim)) ylim <- c(0,1)
    if (is.null(title))  title <- "Groups representation"
    if(graph.type=="classic"){
      plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, asp=1, col="white", ...)
      abline(v=0,lty=2, ...)
      abline(h=0,lty=2, ...)
    }
    coo <- rbind(coo,coord.actif)
    if (lab.grpe){ labe <- c(labe,rownames(coord.actif))
    } else  labe <- c(labe,rep("",nrow(coord.actif)))
    coll <- c(coll,col.hab[seq_len(nrow(coord.actif))])
    ipch <- c(ipch,rep(17,nrow(coord.actif)))
    fonte <- c(fonte,rep(1,nrow(coord.actif)))
    if (!is.null(selection)){
      if (is.numeric(unselect)) coll[!(seq_len(length(coll))%in%selection)] <- rgb(t(col2rgb(coll[!(seq_len(length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255) 
      else coll[!(seq_len(length(coll))%in%selection)] <- unselect
      labe[!(seq_len(length(coll))%in%selection)] <- ""
    }
    
    if (!is.null(res.mfa$group$coord.sup)) {
      coo <- rbind(coo,coord.illu)
      if (lab.grpe){ labe2 <- rownames(coord.illu)
      } else  labe2 <- rep("",nrow(coord.illu))
      coll2 <- col.hab[(nrow(coord.actif) + 1):(nrow(coord.actif) + nrow(coord.illu))]
      ipch2 <- rep(2,nrow(coord.illu))
      fonte2 <- rep(3,nrow(coord.illu))
      #	    if (lab.grpe){ labe <- c(labe,rownames(coord.illu))
      #	    } else  labe <- c(labe,rep("",nrow(coord.illu)))
      #	    coll <- c(coll,col.hab[(nrow(coord.actif) + 1):(nrow(coord.actif) + nrow(coord.illu))])
      #	    ipch <- c(ipch,rep(2,nrow(coord.illu)))
      #	    fonte <- c(fonte,rep(3,nrow(coord.illu)))
      if (length(select)==1){
        if (grepl("contrib",select)){
          if (is.numeric(unselect)) coll2[seq_len(length(coll2))] <- rgb(t(col2rgb(coll2[seq_len(length(coll2))])),alpha=255*(1-unselect),maxColorValue=255) 
          else coll2[seq_len(length(coll2))] <- unselect
          labe2[seq_len(length(coll2))] <- ""
        }}
      if (!is.null(selectionS)){
        if (is.numeric(unselect)) coll2[!(seq_len(length(coll2))%in%selectionS)] <- rgb(t(col2rgb(coll2[!(seq_len(length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255) 
        else coll2[!(seq_len(length(coll2))%in%selectionS)] <- unselect
        labe2[!(seq_len(length(coll2))%in%selectionS)] <- ""
      }
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
      fonte <- c(fonte,fonte2)
      ipch <- c(ipch,ipch2)
    }
    if(graph.type=="classic"){
      if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
      if (autoLab=="auto") autoLab <- (length(labe)<50)
      if (autoLab ==TRUE) autoLab(coo[, 1], y = coo[, 2], labels = labe, col = coll,  font=fonte,shadotext=shadowtext,...)
      if (autoLab ==FALSE) text(coo[, 1], y = coo[, 2], labels = labe, col = coll,  font=fonte,pos=3,...)
      if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
    }
    if(graph.type=="ggplot"){
      if (autoLab=="auto") autoLab <- (length(which(labe!=""))<50)
      df_group <- data.frame(labe,coo,coll,ipch,fonte)
      gg_graph <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_point(aes(x=df_group[,2], y=df_group[,3]), color= rgb(t(col2rgb(df_group[,4])),maxColorValue=255) , shape = df_group[,5], size = ggoptions_default$size/3) + 
        xlab(lab.x) + ylab(lab.y) +
        xlim(xlim) + ylim(ylim) +
        geom_hline(yintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
        geom_vline(xintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
        theme_light() + 
        ggoptions_default$theme +
        ggtitle(title)
      if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_group[,2], y=df_group[,3], label=df_group[,1]), size = ggoptions_default$size, color = rgb(t(col2rgb(df_group[,"coll"])),maxColorValue=255) , fontface = df_group[,"fonte"])
      else{text <- geom_text(aes(x=df_group[,2], y=df_group[,3], label=df_group[,1]), size = ggoptions_default$size, color = rgb(t(col2rgb(df_group[,4])),maxColorValue=255) , hjust = (-sign(df_group[,2])+1)/2, vjust = -sign(df_group[,3])*0.75+0.25, fontface = df_group[,"fonte"])}
      gg_graph <- gg_graph + text + theme
    }
  }
  if (choix == "var") {
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    test.invisible <- vector(length = 2)
    if (!is.null(invisible)) {
      test.invisible[1] <- match("quanti", invisible)
      test.invisible[2] <- match("quanti.sup", invisible)
    }
    else test.invisible <- rep(NA, 2)
    col <- NULL
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) < length(group[type == "c"])){
        if (!is.null(res.mfa$call$num.group.sup)){
          col.hab[which(!seq_len(length(group))%in%(res.mfa$call$num.group.sup))] <- 2:(1+length(group)-length(res.mfa$call$num.group.sup))
          col.hab[res.mfa$call$num.group.sup] <- length(group)-length(res.mfa$call$num.group.sup)+1+seq_len(length(res.mfa$call$num.group.sup))
          col <- c(1+rep(which(res.mfa$call$nature.group[-res.mfa$call$num.group.sup]=="quanti"),times=group[which(res.mfa$call$nature.group=="quanti")]),length(group)-length(res.mfa$call$num.group.sup)+1+rep(which((res.mfa$call$nature.group[res.mfa$call$num.group.sup])=="quanti.sup"),times=group[which(res.mfa$call$nature.group=="quanti.sup")]))
        } else {
          col.hab <- 2:(length(group)+1)
          col <- 1+rep(which(type=="c"),times=group[type=="c"])
        }
      }
      
    } else {
      if (is.null(col.hab) | length(col.hab) < sum(group[type == "c"])) col <- rep(1, sum(group[type == "c"]))
      else col <- col.hab
    }
    if (is.null(title))  title <- "Correlation circle"
    if(graph.type=="classic"){
      plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
           xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), col = "white", 
           asp = 1, ...)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle,...)
      lines(x.cercle, y = -y.cercle,...)
      abline(v = 0, lty = 2, ...)
      abline(h = 0, lty = 2, ...)
    }
    if ((!is.null(select))&(!is.null(res.mfa["quanti.var"]$quanti.var))) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.mfa$quanti.var$coord)%in%select)+sum(rownames(res.mfa$quanti.var.sup$coord)%in%select)!=0) selection <- which(rownames(res.mfa$quanti.var$coord)%in%select)
        else {
          if (grepl("contrib",select)) selection <- (rev(order(res.mfa$quanti.var$contrib[,axes[1],drop=FALSE]*res.mfa$eig[axes[1],1]+res.mfa$quanti.var$contrib[,axes[2],drop=FALSE]*res.mfa$eig[axes[2],1])))[1:min(nrow(res.mfa$quanti.var$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          # 		      if (grepl("contrib",select)) selection <- (rev(order(apply(res.mfa$quanti.var$contrib[,axes],1,sum))))[1:min(nrow(res.mfa$quanti.var$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("coord",select)) selection <- (rev(order(apply(res.mfa$quanti.var$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$quanti.var$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.mfa$quanti.var$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$quanti.var$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selection <- which(apply(res.mfa$quanti.var$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selection <- select
        }  
      }
    }
    if ((!is.null(select))&(!is.null(res.mfa$quanti.var.sup))) {
      if (mode(select)=="numeric") selectionS <- select
      else {
        if (sum(rownames(res.mfa$quanti.var$coord)%in%select)+sum(rownames(res.mfa$quanti.var.sup$coord)%in%select)!=0) selectionS <- which(rownames(res.mfa$quanti.var.sup$coord)%in%select)
        else {
          if (grepl("contrib",select)) selectionS <- NULL
          if (grepl("coord",select)) selectionS <- (rev(order(apply(res.mfa$quanti.var.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$quanti.var.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.mfa$quanti.var.sup$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$quanti.var.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selectionS <- which(apply(res.mfa$quanti.var.sup$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selectionS <- select
        }  
      }
    }
    
    labe <- labe2 <- coll <- coll2 <- NULL
    if (!is.null(res.mfa["quanti.var"]$quanti.var)){
      coll <- col[seq_len(nrow(res.mfa["quanti.var"]$quanti.var$coord))]
      if (lab.var) labe <- rownames(res.mfa["quanti.var"]$quanti.var$coord)
      else  labe <- rep("",nrow(res.mfa["quanti.var"]$quanti.var$coord))
    }
    if (!is.null(res.mfa$quanti.var.sup)){
      if (lab.var) labe2 <- rownames(res.mfa$quanti.var.sup$coord)
      else  labe2 <- rep("",nrow(res.mfa$quanti.var.sup$coord))
      coll2 <- col[(length(coll)+1):length(col)]
    }
    
    if (!is.null(select)){
      if (!is.null(res.mfa["quanti.var"]$quanti.var)&is.na(test.invisible[1])){	
        if(graph.type=="classic"){
          if (is.numeric(unselect)) coll[!(seq_len(length(coll))%in%selection)] <- rgb(t(col2rgb(coll[!(seq_len(length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255) 
          else coll[!(seq_len(length(coll))%in%selection)] <- unselect
        }
        labe[!(seq_len(length(coll))%in%selection)] <- ""
      }
      if (!is.null(res.mfa$quanti.var.sup)&is.na(test.invisible[2])){
        if(graph.type=="classic"){
          if (is.numeric(unselect)) coll2[!(seq_len(length(coll2))%in%selectionS)] <- rgb(t(col2rgb(coll2[!(seq_len(length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255) 
          else coll2[!(seq_len(length(coll2))%in%selectionS)] <- unselect
        }
        labe2[!(seq_len(length(coll2))%in%selectionS)] <- ""
      }
    }
    col <- c(coll,coll2)
    labe <- c(labe,labe2)
    #		if (habillage == "group" & is.na(test.invisible[1]) & is.na(test.invisible[2])) 
    #            legend("topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg),,drop=FALSE])[type == "c"], text.col = col.hab[type == "c"], cex = 0.8*par("cex"))
    if (habillage == "group" & is.na(test.invisible[1]) & is.na(test.invisible[2])) {
      L <- list(x="topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg),,drop=FALSE])[type == "c"], text.col = col.hab[type == "c"], cex = 0.8*par("cex"))
      L <- modifyList(L, legend)
      if(graph.type=="classic") do.call(graphics::legend, L)
    }
    if (habillage == "group" & is.na(test.invisible[1]) & !is.na(test.invisible[2])){
      #            if ("quanti.sup"%in%res.mfa$call$nature.var) legend("topleft", legend = rownames(res.mfa$group$Lg[-c(num.group.sup, nrow(res.mfa$group$Lg)),,drop=FALSE])[type.act == "c"], 
      #                text.col = col.hab[which(!((1:length(group))%in%res.mfa$call$num.group.sup))[type.act == "c"]], cex = 0.8*par("cex"))
      if ("quanti.sup"%in%res.mfa$call$nature.var) {
        L <- list(x="topleft", legend = rownames(res.mfa$group$Lg[-c(num.group.sup, nrow(res.mfa$group$Lg)),,drop=FALSE])[type.act == "c"], text.col = col.hab[which(!(seq_len(length(group))%in%res.mfa$call$num.group.sup))[type.act == "c"]], cex = 0.8*par("cex"))
      } else {
        #		    legend("topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg), ])[type == "c"], text.col = col.hab[type == "c"], cex = 0.8*par("cex"))
        L <- list(x="topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg), ])[type == "c"], text.col = col.hab[type == "c"], cex = 0.8*par("cex"))
      }
      L <- modifyList(L, legend)
      if(graph.type=="classic") do.call(graphics::legend, L)
    }
    if (habillage == "group" & !is.na(test.invisible[1]) & is.na(test.invisible[2])){
      #          if ("quanti"%in%res.mfa$call$nature.var) legend("topleft", legend = rownames(res.mfa$group$Lg[num.group.sup,,drop=FALSE])[type.sup == "c"], text.col = col.hab[res.mfa$call$num.group.sup[type.sup == "c"]], cex = 0.8*par("cex"))
      #		  else legend("topleft", legend = rownames(res.mfa$group$Lg[num.group.sup,,drop=FALSE])[type.sup == "c"], text.col = col.hab[res.mfa$call$num.group.sup[type.sup == "c"]], cex = 0.8*par("cex"))
      if ("quanti"%in%res.mfa$call$nature.var) L <- list(x="topleft", legend = rownames(res.mfa$group$Lg[num.group.sup,,drop=FALSE])[type.sup == "c"], text.col = col.hab[res.mfa$call$num.group.sup[type.sup == "c"]], cex = 0.8*par("cex"))
      else L <- list(x="topleft", legend = rownames(res.mfa$group$Lg[num.group.sup,,drop=FALSE])[type.sup == "c"], text.col = col.hab[res.mfa$call$num.group.sup[type.sup == "c"]], cex = 0.8*par("cex"))
      L <- modifyList(L, legend)
      if(graph.type=="classic") do.call(graphics::legend, L)
    }
    nrow.coord.var <- 0
    coo <- posi <- NULL
    if ((!is.null(res.mfa["quanti.var"]$quanti.var))&(!is.na(test.invisible[1]))){
      col[seq_len(nrow(res.mfa$quanti.var$cor[,axes,drop=FALSE]))] <- "transparent"
    }
    if (!is.null(res.mfa["quanti.var"]$quanti.var)){
      ### modif 2016-02-16
      coord.var <- res.mfa$quanti.var$cor[,axes,drop=FALSE]
      coo <- coord.var
      if (length(which(apply(res.mfa$quanti.var$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) < lim.cos2.var))>0) col[which(apply(res.mfa$quanti.var$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) < lim.cos2.var)] <- "transparent"
      if(graph.type=="classic"){
        for (v in seq_len(nrow(coord.var))) {
	     if (!(is.nan(coord.var[v,1])|is.infinite(coord.var[v,1]))){
          arrows(0, 0, coord.var[v, 1], coord.var[v, 2], length = 0.1, angle = 15, code = 2, col = col[v])
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
      }
    }
    if ((!is.null(res.mfa$quanti.var.sup$coord))&(!is.na(test.invisible[2]))){
      col[nrow(coo)+seq_len(nrow(res.mfa$quanti.var.sup$cor[,axes,drop=FALSE]))] <- "transparent"
    }
    if (!is.null(res.mfa$quanti.var.sup$coord)){
      coord.quanti <- res.mfa$quanti.var.sup$cor[ ,axes,drop=FALSE]
      coo <- rbind(coo,coord.quanti)
      if (length(which(apply(res.mfa$quanti.var.sup$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) < lim.cos2.var))>0) col[nrow(coo)-nrow(coord.quanti)+which(apply(res.mfa$quanti.var.sup$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) < lim.cos2.var)]<-"transparent"
      if(graph.type=="classic"){  
        for (q in seq_len(nrow(coord.quanti))) {
	     if (!(is.nan(coord.quanti[q,1])|is.infinite(coord.quanti[q,1]))){
          arrows(0, 0, coord.quanti[q, 1], coord.quanti[q, 2], length = 0.1, angle = 15, code = 2, lty = 2, col=col[nrow(coo)-nrow(coord.quanti)+q],...)
          if (lab.var) {
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
        }
      }
    }
    if(graph.type=="classic"){
      if (autoLab=="auto") autoLab <- (length(labe)-sum(col=="transparent")<50)
	  AGarder <- which(!(is.nan(coo[,1])|is.infinite(coo[,1])))
	  coo <- coo[AGarder,]
	  col <- col[AGarder]
	  labe <- labe[AGarder]
      if (autoLab==FALSE) text(coo[, 1], y = coo[, 2], labels = labe, pos = posi, col = col,...) ### pas mettre sur posi
      if (autoLab==TRUE) autoLab(coo[which(col!="transparent"), 1], y = coo[which(col!="transparent"), 2], labels = labe[which(col!="transparent")], col=col[which(col!="transparent")], shadotext=shadowtext,...)
      par(mar = c(5, 4, 4, 2) + 0.1)
    }
    if(graph.type=="ggplot"){
      if (autoLab=="auto") autoLab <- (length(labe)<50)
      circle <- annotate("path",
                         x=0+1*cos(seq(0,2*pi,length.out=100)),
                         y=0+1*sin(seq(0,2*pi,length.out=100)),
                         lty = ggoptions_default$circle.lty,
                         lwd = ggoptions_default$circle.lwd,
                         color = ggoptions_default$circle.color)
      df_var <- data.frame(labe,coo)
      transparency_var <- 1
      if (!is.null(select)) transparency_var <- ifelse(df_var[,1] != "", 1, 1-unselect)

      if(habillage == "group"){
        df_var <- data.frame(df_var,col,transparency_var)
        df_var[,4] <- as.factor(df_var[,4])
        df_var <- df_var[which(df_var[,4] != "transparent"),]
        df_var <- df_var[which(!(is.nan(df_var[,2])|is.infinite(df_var[,2]))),]   # suppress variable with standard deviation = 0
      gg_graph <- ggplot() + 
        coord_fixed(ratio = 1) + 
        geom_line(aes(x=x, y=y), data=data.frame(x=-1:1,y=0),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
        geom_line(aes(x=x, y=y), data=data.frame(x=0,y=-1:1),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
        geom_segment(aes(x=0,y=0,xend=df_var[,2], yend=df_var[,3], color = df_var[,4]),arrow=arrow(length=unit(0.2,"cm")), lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd, alpha = df_var[,5]) + 
        xlab(lab.x) + ylab(lab.y) +
        xlim(c(-1.3,1.3)) + ylim(c(-1.1,1.1)) +
        ggtitle(title) +
        labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "")) +
        scale_color_manual(values = palette[L$text.col[order(L$text.col)]], labels = L$legend[order(L$text.col)]) +
        theme_light()  + 
        ggoptions_default$theme
      if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1], color = df_var[,4]), size = ggoptions_default$size,show.legend = FALSE)
      else{text <- geom_text(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1], color = df_var[,4]), size = ggoptions_default$size, hjust = (-sign(df_var[,2])+1)/2, vjust = -sign(df_var[,3])*0.75+0.25,show.legend = FALSE)}
      }
      if(habillage=="none"){
        df_var <- df_var[which(!(is.nan(df_var[,2])|is.infinite(df_var[,2]))),]
        gg_graph <- ggplot() + 
          #aes(x=df_var[,2], y=df_var[,3]) +
          coord_fixed(ratio = 1) + 
          geom_line(aes(x=x, y=y), data=data.frame(x=-1:1,y=0),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
          geom_line(aes(x=x, y=y), data=data.frame(x=0,y=-1:1),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
          geom_segment(aes(x=0,y=0,xend=df_var[,2], yend=df_var[,3]),arrow=arrow(length=unit(0.2,"cm")), lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd, alpha = df_var[,5]) + 
          xlab(lab.x) + ylab(lab.y) +
          ggtitle(title) +
          theme_light()  + 
          ggoptions_default$theme
        if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1]), size = ggoptions_default$size,show.legend = FALSE)
        else{text <- geom_text(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1]), size = ggoptions_default$size, hjust = (-sign(df_var[,2])+1)/2, vjust = -sign(df_var[,3])*0.75+0.25,show.legend = FALSE)}
        
      }
      gg_graph <- gg_graph + text + theme + circle      
    }
  }
  if (choix=="freq"){
    if (is.null(res.mfa$freq)&is.null(res.mfa$freq.sup)) stop("You have no frequencies groups")
	if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    col.row <- 1
    col.row.sup <- "grey60"
    coord.col <- res.mfa$freq$coord[, axes, drop = FALSE]
    coord.row <- res.mfa$ind$coord[, axes]
    coord.row.sup <- coord.col.sup <- NULL
    if (!is.null(res.mfa$ind.sup)) coord.row.sup <- res.mfa$ind.sup$coord[, axes, drop = FALSE]
    if (!is.null(res.mfa$freq.sup)) coord.col.sup <- res.mfa$freq.sup$coord[, axes, drop = FALSE]
    
    test.invisible <- vector(length = 4)
    if (!is.null(invisible)) {
      test.invisible[1] <- match("row", invisible)|match("ind", invisible)
      test.invisible[2] <- match("col", invisible)
      test.invisible[3] <- match("row.sup", invisible)|match("ind.sup", invisible)
      test.invisible[4] <- match("col.sup", invisible)
    }
    else  test.invisible <- rep(NA, 4)
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.row[,1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.row[,1])
      if(is.na(test.invisible[2])) xmin <- min(xmin, coord.col[,1])
      if(is.na(test.invisible[2])) xmax <- max(xmax, coord.col[,1])
      if(is.na(test.invisible[3])) xmin <- min(xmin, coord.row.sup[, 1])
      if(is.na(test.invisible[3])) xmax <- max(xmax, coord.row.sup[, 1])
      if(is.na(test.invisible[4])) xmin <- min(xmin, coord.col.sup[, 1])
      if(is.na(test.invisible[4])) xmax <- max(xmax, coord.col.sup[, 1])
      xlim <- c(xmin, xmax) * 1.2
    }
    else {
      xmin <- xlim[1]
      xmax <- xlim[2]
    }
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.row[,2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.row[,2])
      if(is.na(test.invisible[2])) ymin <- min(ymin, coord.col[,2])
      if(is.na(test.invisible[2])) ymax <- max(ymax, coord.col[,2])
      if(is.na(test.invisible[3])) ymin <- min(ymin, coord.row.sup[,2])
      if(is.na(test.invisible[3])) ymax <- max(ymax, coord.row.sup[,2])
      if(is.na(test.invisible[4])) ymin <- min(ymin, coord.col.sup[,2])
      if(is.na(test.invisible[4])) ymax <- max(ymax, coord.col.sup[,2])
      ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin <- ylim[1]
      ymax <- ylim[2]
    }
    if(graph.type=="ggplot") nudge_y  <- (xlim[2] - xlim[1])*0.03
    col <- NULL    
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) < length(group[type == "f"])){
        if (!is.null(res.mfa$call$num.group.sup)){
          col.hab[which(!seq_len(length(group))%in%(res.mfa$call$num.group.sup))] <- 2:(1+length(group)-length(res.mfa$call$num.group.sup))
          col.hab[res.mfa$call$num.group.sup] <- length(group)-length(res.mfa$call$num.group.sup)+1+seq_len(length(res.mfa$call$num.group.sup))
		  if (!is.null(res.mfa$call$nature.group=="contingency")) col.col <- rep(col.hab[res.mfa$call$nature.group=="contingency"],times=group[res.mfa$call$nature.group=="contingency"])
		  if (!is.null(res.mfa$call$nature.group=="contingency.sup")) col.col.sup <- rep(col.hab[res.mfa$call$nature.group=="contingency.sup"],times=group[res.mfa$call$nature.group=="contingency.sup"])
 	    } else {
		  col.hab <- 2:(length(group) + 1)
          col.col <- 1+rep(which(type=="f"),times=group[type=="f"])
		}
	  }
    } else {
      if (is.null(col.hab) | length(col.hab) < sum(group[type == "f"])) {
 	    col.hab <- rep(1, sum(group[type == "f"]))
		col <- col.hab
		col.col <- "blue"
        col.col.sup <- "darkblue"
      } else col.col <- col.hab
    }

    if (is.null(title)) titre <- "Factor map for the contingency table(s)"
    else titre <- title
    if(graph.type=="classic"){
      plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
      abline(h=0,lty=2,...)
      abline(v=0,lty=2,...)
    }
    selection <- selectionC <- selectionS <- selectionCS <- NULL
    if (!is.null(select)) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.mfa$freq.sup$coord)%in%select)+sum(rownames(res.mfa$freq$coord)%in%select)+sum(rownames(res.mfa$ind$coord)%in%select)+sum(rownames(res.mfa$ind.sup$coord)%in%select)!=0) selection <- which(rownames(res.mfa$ind$coord)%in%select)
        else {
          if (grepl("contrib",select)) selection <- (rev(order(res.mfa$ind$contrib[,axes[1],drop=FALSE]*res.mfa$eig[axes[1],1]+res.mfa$ind$contrib[,axes[2],drop=FALSE]*res.mfa$eig[axes[2],1])))[1:min(nrow(res.mfa$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          # 		    if (grepl("contrib",select)) selection <- (rev(order(apply(res.mfa$ind$contrib[,axes],1,sum))))[1:min(nrow(res.mfa$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("inertia",select)) selection <- (rev(order(apply(res.mfa$ind$within.inertia[,axes],1,sum))))[1:min(nrow(res.mfa$ind$coord),sum(as.integer(unlist(strsplit(select,"inertia"))),na.rm=T))]
          if (grepl("coord",select)) selection <- (rev(order(apply(res.mfa$ind$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$ind$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.mfa$ind$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$ind$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selection <- which(apply(res.mfa$ind$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selection <- select
        }  
      }
    }
    if ((!is.null(select))&(!is.null(res.mfa$ind.sup$coord))) {
      if (mode(select)=="numeric") selectionS <- select
      else {
        if (sum(rownames(res.mfa$freq.sup$coord)%in%select)+sum(rownames(res.mfa$freq$coord)%in%select)+sum(rownames(res.mfa$ind$coord)%in%select)+sum(rownames(res.mfa$ind.sup$coord)%in%select)!=0) selectionS <- which(rownames(res.mfa$ind.sup$coord)%in%select)
        else {
          if (grepl("contrib",select)) selectionS <- NULL
          if (grepl("inertia",select)) selectionS <- (rev(order(apply(res.mfa$ind.sup$within.inertia[,axes]^2,1,sum))))[1:min(nrow(res.mfa$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"inertia"))),na.rm=T))]
          if (grepl("coord",select)) selectionS <- (rev(order(apply(res.mfa$ind.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.mfa$ind.sup$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$ind.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selectionS <- which(apply(res.mfa$ind.sup$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selectionS <- select
        }  
      }
    }
    if ((!is.null(select))&(!is.null(res.mfa$freq$coord))) {
      if (mode(select)=="numeric") selectionC <- select
      else {
        if (sum(rownames(res.mfa$freq.sup$coord)%in%select)+sum(rownames(res.mfa$freq$coord)%in%select)+sum(rownames(res.mfa$ind$coord)%in%select)+sum(rownames(res.mfa$ind.sup$coord)%in%select)!=0) selectionC <- which(rownames(res.mfa$freq$coord)%in%select)
        else {
          if (grepl("contrib",select)) selectionC <- (rev(order(res.mfa$freq$contrib[,axes[1],drop=FALSE]*res.mfa$eig[axes[1],1]+res.mfa$freq$contrib[,axes[2],drop=FALSE]*res.mfa$eig[axes[2],1])))[1:min(nrow(res.mfa$freq$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("coord",select)) selectionC <- (rev(order(apply(res.mfa$freq$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$freq$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionC <- (rev(order(apply(res.mfa$freq$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$freq$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selectionC <- which(apply(res.mfa$freq$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selectionC <- select
        }  
      }
    }
    if ((!is.null(select))&(!is.null(res.mfa$freq.sup$coord))) {
      if (mode(select)=="numeric") selectionCS <- select
      else {
        if (sum(rownames(res.mfa$freq.sup$coord)%in%select)+sum(rownames(res.mfa$freq$coord)%in%select)+sum(rownames(res.mfa$ind$coord)%in%select)+sum(rownames(res.mfa$ind.sup$coord)%in%select)!=0) selectionCS <- which(rownames(res.mfa$freq.sup$coord)%in%select)
        else {
          if (grepl("contrib",select)) selectionCS <- NULL
          if (grepl("coord",select)) selectionCS <- (rev(order(apply(res.mfa$freq.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$freq.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionCS <- (rev(order(apply(res.mfa$freq.sup$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$freq.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selectionCS <- which(apply(res.mfa$freq.sup$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selectionCS <- select
        }  
      }
    }
    
    coo <- labe <- coll <- ipch <- fonte <- NULL
    if (is.na(test.invisible[1])) {
      coo <- rbind(coo,coord.row)
      if (lab.ind){ labe <- rownames(coord.row)
      } else  labe <- rep("",nrow(coord.row))
      coll <- rep(col.row,nrow(coord.row))
      ipch <- c(ipch,rep(20,nrow(coord.row)))
      fonte <- c(fonte,rep(1,nrow(coord.row)))
      if (!is.null(selection)){
        if (is.numeric(unselect)) coll[!(seq_len(length(coll))%in%selection)] <- rgb(t(col2rgb(coll[!(seq_len(length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255)
        else coll[!(seq_len(length(coll))%in%selection)] <- unselect
        labe[!(seq_len(length(coll))%in%selection)] <- ""
      }
    }
    if (is.na(test.invisible[2])) {
      coo <- rbind(coo,coord.col)
      if (lab.col){ labe2 <- rownames(coord.col)
      } else  labe2 <- rep("",nrow(coord.col))
      if (length(col.col)==1) coll2 <- rep(col.col,nrow(coord.col))
      else coll2 <- col.col
      ipch <- c(ipch,rep(17,nrow(coord.col)))
      fonte <- c(fonte,rep(1,nrow(coord.col)))
      if (!is.null(selectionC)){
        if (is.numeric(unselect)) coll2[!(seq_len(length(coll2))%in%selectionC)] <- rgb(t(col2rgb(coll2[!(seq_len(length(coll2))%in%selectionC)])),alpha=255*(1-unselect),maxColorValue=255)
        else coll2[!(seq_len(length(coll2))%in%selectionC)] <- unselect
        labe2[!(seq_len(length(coll2))%in%selectionC)] <- ""
      }
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
    }
    if (!is.null(res.mfa$freq.sup) & is.na(test.invisible[4])) {
      coo <- rbind(coo,coord.col.sup)
      if (lab.col){ labe2 <- rownames(coord.col.sup)
      } else  labe2 <- rep("",nrow(coord.col.sup))
      if (length(col.col.sup)==1) coll2 <- rep(col.col.sup,nrow(coord.col.sup))
	  else coll2 <- col.col.sup
      ipch <- c(ipch,rep(17,nrow(coord.col.sup)))
      fonte <- c(fonte,rep(1,nrow(coord.col.sup)))
      if (!is.null(selectionCS)){
        if (is.numeric(unselect)) coll2[!(seq_len(length(coll2))%in%selectionCS)] <- rgb(t(col2rgb(coll2[!(seq_len(length(coll2))%in%selectionCS)])),alpha=255*(1-unselect),maxColorValue=255)
        else coll2[!(seq_len(length(coll2))%in%selectionCS)] <- unselect
        labe2[!(seq_len(length(coll2))%in%selectionCS)] <- ""
      }
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
    }
    if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[3])) {
      coo <- rbind(coo,coord.row.sup)
      if (lab.ind){ labe2 <- rownames(coord.row.sup)
      } else  labe2 <- rep("",nrow(coord.row.sup))
      coll2 <- rep(col.row.sup,nrow(coord.row.sup))
      ipch <- c(ipch,rep(17,nrow(coord.row.sup)))
      fonte <- c(fonte,rep(1,nrow(coord.row.sup)))
      if (!is.null(selectionS)){
        if (is.numeric(unselect)) coll2[!(seq_len(length(coll2))%in%selectionS)] <- rgb(t(col2rgb(coll2[!(seq_len(length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255)
        else coll2[!(seq_len(length(coll2))%in%selectionS)] <- unselect
        labe2[!(seq_len(length(coll2))%in%selectionS)] <- ""
      }
      if (length(select)==1){
        if (grepl("contrib",select)){
          if (is.numeric(unselect)) coll2[seq_len(length(coll2))] <- rgb(t(col2rgb(coll2[seq_len(length(coll2))])),alpha=255*(1-unselect),maxColorValue=255) 
          else coll2[seq_len(length(coll2))] <- unselect
          labe2[seq_len(length(coll2))] <- ""
        }}
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
    }
    if(graph.type=="classic"){
      if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
      if (any(labe!="")){
        if (autoLab=="auto") autoLab <- (length(which(labe!=""))<50)
        if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],shadotext=shadowtext,...)
        if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)
      }
      if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
    }
    if (habillage == "group"){
      if (is.na(test.invisible[1])) L <- list(x="topleft", legend = c("Individuals",rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg),,drop=FALSE ])[type == "f"]), text.col = c(1,col.hab[type == "f"]), cex = 0.8*par("cex"))
	  else L <- list(x="topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg),,drop=FALSE ])[type == "f"], text.col = col.hab[type == "f"], cex = 0.8*par("cex"))
      
      L <- modifyList(L, legend)
      if(graph.type == "classic") do.call(graphics::legend, L)	
    }
    if(graph.type == "ggplot"){
      if (autoLab=="auto") autoLab <- (length(which(labe!=""))<50)
      df_freq <- data.frame(labe,coo,coll,ipch,fonte)
#	  df_freq[,4] <- as.factor(df_freq[,4])
      if(habillage == "group"){
      gg_graph <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_point(aes(x=df_freq[,2], y=df_freq[,3]),color=palette[df_freq[,4]], shape = ggoptions_default$point.shape, size = ggoptions_default$size/3) +
        xlab(lab.x) + ylab(lab.y) +
        xlim(xlim) + ylim(ylim) +
        geom_hline(yintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
        geom_vline(xintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
        labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "")) +
        scale_color_manual(values = palette[L$text.col[order(L$text.col)]], labels = L$legend[order(L$text.col)]) +
        theme_light() +
        ggoptions_default$theme +
        ggtitle(titre)
      if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_freq[,2], y=df_freq[,3], label=df_freq[,1]), size = ggoptions_default$size, color = palette[df_freq[,4]])
      else{text <- geom_text(aes(x=df_freq[,2], y=df_freq[,3], label=df_freq[,1]), size = ggoptions_default$size, color = palette[df_freq[,4]], hjust = (-sign(df_freq[,2])+1)/2, vjust = -sign(df_freq[,3])*0.75+0.25)}
      }
      if(habillage == "none"){
        gg_graph <- ggplot() +
          coord_fixed(ratio = 1) +
          geom_point(aes(x=df_freq[,2], y=df_freq[,3]), color=palette[df_freq[,4]], shape = ggoptions_default$point.shape, size = ggoptions_default$size/3) +
          xlab(lab.x) + ylab(lab.y) +
          xlim(xlim) + ylim(ylim) +
          geom_hline(yintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
          geom_vline(xintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
          theme_light() +
          ggoptions_default$theme +
          ggtitle(titre)
        if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_freq[,2], y=df_freq[,3], label=df_freq[,1]), size = ggoptions_default$size, color = palette[df_freq[,4]])
        else{text <- geom_text(aes(x=df_freq[,2], y=df_freq[,3], label=df_freq[,1]), size = ggoptions_default$size, color = palette[df_freq[,4]], hjust = (-sign(df_freq[,2])+1)/2, vjust = -sign(df_freq[,3])*0.75+0.25)}
      }
      gg_graph <- gg_graph + theme + text
    }
  }
  if (choix == "ind") {
    test.invisible <- vector(length = 3)
    if (!is.null(invisible)) {
      test.invisible[1] <- match("ind", invisible)
      test.invisible[2] <- match("ind.sup", invisible)
      test.invisible[3] <- match("quali", invisible)
      test.invisible[4] <- match("quali.sup", invisible)
    }
    else test.invisible <- rep(NA, 3)
    nb.ind.actif <- nrow(res.mfa$ind$coord)
    nb.ind.illu <- 0
    if (!is.null(res.mfa$ind.sup)) nb.ind.illu <- nrow(res.mfa$ind.sup$coord)
    nb.ind <- nb.ind.actif + nb.ind.illu
    coord.ind <- res.mfa$ind$coord[, axes, drop = FALSE]
    coord.ind.partiel <- res.mfa$ind$coord.partiel[, axes, drop = FALSE]
    coord.ind.sup <- NULL
    if (!is.null(res.mfa$ind.sup)) {
      coord.ind.sup <- res.mfa$ind.sup$coord[, axes, drop = FALSE]
      coord.ind.partiel.sup <- res.mfa$ind.sup$coord.partiel[, axes, drop = FALSE]
    }
    coord_quali <- coord_quali_sup <- coord_quali.partiel <- coord_quali_sup.partiel <- NULL
    if (!is.null(res.mfa["quali.var"]$quali.var)) {
      coord_quali <- res.mfa$quali.var$coord[, axes, drop = FALSE]
      coord_quali.partiel <- res.mfa$quali.var$coord.partiel[, axes, drop = FALSE]
    }
    if (!is.null(res.mfa["quali.var.sup"])) {
      coord_quali_sup <- res.mfa$quali.var.sup$coord[, axes, drop = FALSE]
      coord_quali.partiel.sup <- res.mfa$quali.var.sup$coord.partiel[, axes, drop = FALSE]
    }
    group.ind.actif <- group.ind.sup <- group.quali <- group.quali.sup <- NULL
    if (!is.null(partial)) {
      if (length(partial) == 1) {
        if (partial == "all") {
          group.ind.actif <- seq_len(nrow(coord.ind))
          if (!is.null(res.mfa$ind.sup))  group.ind.sup <- seq_len(nrow(coord.ind.sup))
          if (!is.null(res.mfa["quali.var"]$quali.var)) group.quali <- seq_len(nrow(coord_quali))
          if (!is.null(res.mfa["quali.var.sup"]$quali.var.sup)) group.quali.sup <- seq_len(nrow(coord_quali_sup))
        }
        else {
          for (i in seq_len(length(partial))) {
            if (partial[i] %in% rownames(coord.ind)) 
              group.ind.actif <- c(group.ind.actif, match(partial[i], 
                                                          rownames(coord.ind)))
            if (partial[i] %in% rownames(coord.ind.sup)) 
              group.ind.sup <- c(group.ind.sup, match(partial[i], 
                                                      rownames(coord.ind.sup)))
            if (partial[i] %in% rownames(coord_quali)) 
              group.quali <- c(group.quali, match(partial[i], 
                                                  rownames(coord_quali)))
            if (partial[i] %in% rownames(coord_quali_sup)) 
              group.quali.sup <- c(group.quali.sup, match(partial[i], 
                                                          rownames(coord_quali_sup)))
          }
        }
      }
      else {
        for (i in seq_len(length(partial))) {
          if (partial[i] %in% rownames(coord.ind)) 
            group.ind.actif <- c(group.ind.actif, match(partial[i], 
                                                        rownames(coord.ind)))
          if (partial[i] %in% rownames(coord.ind.sup)) 
            group.ind.sup <- c(group.ind.sup, match(partial[i], 
                                                    rownames(coord.ind.sup)))
          if (partial[i] %in% rownames(coord_quali)) 
            group.quali <- c(group.quali, match(partial[i], 
                                                rownames(coord_quali)))
          if (partial[i] %in% rownames(coord_quali_sup)) 
            group.quali.sup <- c(group.quali.sup, match(partial[i], 
                                                        rownames(coord_quali_sup)))
        }
      }
    }
    if (!is.null(ellipse)) {
      coord.ellipse <- ellipse$res
      npoint.ellipse <- ellipse$call
    }
    else coord.ellipse <- NULL
    if (!is.null(ellipse.par)) {
      coord.ellipse.par <- ellipse.par$res
      npoint.ellipse.par <- ellipse.par$call
    }
    else coord.ellipse.par <- NULL
	nullxlimylim <- (is.null(xlim) & is.null(ylim))
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if (is.na(test.invisible[1]))  xmin <- min(xmin, coord.ind[, 1])
      if (is.na(test.invisible[1]))  xmax <- max(xmax, coord.ind[, 1])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) xmin <- min(xmin, coord.ind.sup[, 1])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) xmax <- max(xmax, coord.ind.sup[, 1])
      if (is.na(test.invisible[1])) xmin <- min(xmin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                                                                                      function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 1])
      if (is.na(test.invisible[1])) xmax <- max(xmax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                                                                                      function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 1])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) xmin <- min(xmin, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                                                                                                                      function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 1])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
        xmax <- max(xmax, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                                1])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmin <- min(xmin, coord_quali[, 1])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmax <- max(xmax, coord_quali[, 1])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmin <- min(xmin, coord_quali.partiel[unlist(lapply(group.quali,  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))),  1])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmax <- max(xmax, coord_quali.partiel[unlist(lapply(group.quali, 
                                                            function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                              1])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        xmin <- min(xmin, coord_quali[, 1], coord_quali_sup[,1])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        xmax <- max(xmax, coord_quali[, 1], coord_quali_sup[, 1])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        xmin <- min(xmin, coord_quali.partiel.sup[unlist(lapply(group.quali.sup, 
                                                                function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                                  1])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        xmax <- max(xmax, coord_quali.partiel.sup[unlist(lapply(group.quali.sup, 
                                                                function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                                  1])
      # xlim <- c(xmin, xmax) * 1.1
      xlim <- c(xmin, xmax)
      xlim <- (xlim-mean(xlim))*1.1 + mean(xlim)
    }
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if (is.na(test.invisible[1])) 
        ymin <- min(ymin, coord.ind[, 2])
      if (is.na(test.invisible[1])) 
        ymax <- max(ymax, coord.ind[, 2])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
        ymin <- min(ymin, coord.ind.sup[, 2])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
        ymax <- max(ymax, coord.ind.sup[, 2])
      if (is.na(test.invisible[1])) 
        ymin <- min(ymin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                             function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 2])
      if (is.na(test.invisible[1])) 
        ymax <- max(ymax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                             function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 2])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
        ymin <- min(ymin, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                           function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 2])
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
        ymax <- max(ymax, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                            function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 2])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymin <- min(ymin, coord_quali[, 2])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymax <- max(ymax, coord_quali[, 2])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymin <- min(ymin, coord_quali.partiel[unlist(lapply(group.quali, 
                     function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 2])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymax <- max(ymax, coord_quali.partiel[unlist(lapply(group.quali, 
                   function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 2])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        ymin <- min(ymin, coord_quali[, 1], coord_quali_sup[, 2])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        ymax <- max(ymax, coord_quali[, 1], coord_quali_sup[, 2])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        ymin <- min(ymin, coord_quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 2])
      if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[4])) 
        ymax <- max(ymax, coord_quali.partiel.sup[unlist(lapply(group.quali.sup, 
                       function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))),  2])
      ylim <- c(ymin, ymax)
      ylim <- (ylim-mean(ylim))*1.1 + mean(ylim)
    }
    if (nullxlimylim & diff(xlim)/diff(ylim)>3) ylim <- (ylim-mean(ylim))*diff(xlim)/diff(ylim)/3 + mean(ylim)
    if (nullxlimylim & diff(xlim)/diff(ylim)<1/2) xlim <- (xlim-mean(xlim))*diff(ylim)/diff(xlim)/2 + mean(xlim)
    if(graph.type=="ggplot") nudge_y <- (ylim[2] - ylim[1])*0.03
    selection <- NULL
    if (!is.null(select)) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.mfa$ind$coord)%in%select)!=0) selection <- which(rownames(res.mfa$ind$coord)%in%select)
        else {
          if (grepl("contrib",select)) selection <- (rev(order(res.mfa$ind$contrib[,axes[1],drop=FALSE]*res.mfa$eig[axes[1],1]+res.mfa$ind$contrib[,axes[2],drop=FALSE]*res.mfa$eig[axes[2],1])))[1:min(nrow(res.mfa$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("dist",select)) selection <- (rev(order(res.mfa$ind$dist)))[1:min(nrow(res.mfa$ind$coord),sum(as.integer(unlist(strsplit(select,"dist"))),na.rm=T))]
          if (grepl("coord",select)) selection <- (rev(order(apply(res.mfa$ind$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$ind$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.mfa$ind$cos2[,axes],1,sum))))[1:min(nrow(res.mfa$ind$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selection <- which(apply(res.mfa$ind$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selection <- select
        }  
      }
    }
    selectionS <- NULL
    if ((!is.null(select))&(!is.null(res.mfa$ind.sup$coord))&is.na(test.invisible[2])) {
      if (mode(select)=="numeric") selectionS <- select
      else {
        if (sum(rownames(res.mfa$ind$coord)%in%select)+sum(rownames(res.mfa$ind.sup$coord)%in%select)!=0) selectionS <- which(rownames(res.mfa$ind.sup$coord)%in%select)
        else {
          if (grepl("dist",select[1])) selectionS <- (rev(order(res.mfa$ind.sup$dist)))[1:min(nrow(res.mfa$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"dist"))),na.rm=T))]
          if (grepl("coord",select[1])) selectionS <- (rev(order(apply(res.mfa$ind.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.mfa$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select[1])) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.mfa$ind.sup$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.mfa$ind.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selectionS <- which(apply(res.mfa$ind.sup$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selectionS <- select
        }  
      }
    }
	
    if (habillage == "none"){
      col.ind <- rep(1, nb.ind.actif*(nbre.grpe + 1))
      if (!is.null(res.mfa$ind.sup)) col.ind.sup <- rep(4, (nb.ind - nb.ind.actif)*(nbre.grpe + 1))
      if (!is.null(res.mfa[["quali.var"]])) col.quali <- rep(2, (nbre.grpe + 1)*sum(res.mfa$call$group.mod[res.mfa$call$nature.group=="quali"]))
      if (!is.null(res.mfa$quali.var.sup)) col.quali.sup <- rep("darkred", (nbre.grpe + 1)*sum(res.mfa$call$group.mod[res.mfa$call$nature.group=="quali.sup"]))
      if (!is.null(ellipse)) col.ellipse <- rep(25, nb.ind.actif)
      if (!is.null(ellipse.par)) col.ellipse.par <- rep(2, nb.ind.actif*(nbre.grpe + 1))
	}
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) != (nbre.grpe)) col.hab <- 2:(nbre.grpe + 1)
      col.ind <- c(rep(1, nb.ind.actif), rep(col.hab, nb.ind.actif))
      if (!is.null(res.mfa$ind.sup)) col.ind.sup <- c(rep(1, nb.ind - nb.ind.actif), rep(col.hab, nb.ind - nb.ind.actif))
      if (!is.null(res.mfa[["quali.var"]])) col.quali <- c(rep(1, sum(res.mfa$call$group.mod[res.mfa$call$nature.group=="quali"])), rep(col.hab, sum(res.mfa$call$group.mod[res.mfa$call$nature.group=="quali"])))
      if (!is.null(res.mfa$quali.var.sup)) col.quali.sup <- c(rep(1, sum(res.mfa$call$group.mod[res.mfa$call$nature.group=="quali.sup"])), rep(col.hab, sum(res.mfa$call$group.mod[res.mfa$call$nature.group=="quali.sup"])))
      if (!is.null(ellipse)) col.ellipse <- rep(1, nb.ind.actif)
      if (!is.null(ellipse.par)) col.ellipse.par <- rep(col.hab, nb.ind.actif)
    }
    if (habillage == "ind") {
      if (is.null(col.hab) | length(col.hab) != nb.ind) col.hab <- 1:nb.ind
      col.ind <- c(col.hab[1:nb.ind.actif], rep(col.hab[1:nb.ind.actif], each = nbre.grpe))
      if (!is.null(res.mfa$ind.sup)) col.ind.sup <- c(col.hab[(nb.ind.actif + 1):nb.ind], rep(col.hab[(nb.ind.actif + 1):nb.ind], each = nbre.grpe))
      if (length(group[type == "n"]) != 0) col.quali <- col.quali.sup <- rep("black", (1 + nbre.grpe) * sum(res.mfa$call$group.mod[type == "n"]))
      if (!is.null(ellipse)) col.ellipse <- col.hab[1:nb.ind.actif]
      if (!is.null(ellipse.par)) col.ellipse.par <- rep(col.hab[1:nb.ind.actif], each = nbre.grpe)
    }
    if ((habillage != "none") & (habillage != "ind") & (habillage != "group")) {
      group.act <- seq_len(length(group))
      if (!is.null(num.group.sup))  group.act <- group.act[-num.group.sup]
      nbre.modalite <- nbre.modalite.sup <- NULL
      liste.quali <- liste.quali.sup <- NULL
      for (i in group.act) {
        if (type[i] == "n") {
          for (k in seq_len(ncol(res.mfa$separate.analyses[[i]]$call$X))) nbre.modalite <- c(nbre.modalite, nlevels(res.mfa$separate.analyses[[i]]$call$X[, k]))
          if (i == 1) liste.quali <- c(liste.quali, colnames(res.mfa$call$X[1:group[1]]))
          else liste.quali <- c(liste.quali, colnames(res.mfa$call$X[(sum(group[1:(i - 1)]) + 1):sum(group[1:i])]))
        }
      }
      if (!is.null(num.group.sup)) {
        for (i in num.group.sup) {
          if (type[i] == "n") {
            if (i == 1) liste.quali.sup <- c(liste.quali.sup, colnames(res.mfa$call$X[1:group[1]]))
            else liste.quali.sup <- c(liste.quali.sup, colnames(res.mfa$call$X[(sum(group[1:(i - 1)]) + 1):sum(group[1:i])]))
            for (k in seq_len(ncol(res.mfa$separate.analyses[[i]]$call$X))) nbre.modalite.sup <- c(nbre.modalite.sup, nlevels(res.mfa$separate.analyses[[i]]$call$X[, k]))
          }
        }
      }
      if (is.double(habillage)) nom.quali <- colnames(res.mfa$call$X)[habillage]
      else nom.quali <- habillage
      if (!(nom.quali %in% c(liste.quali,liste.quali.sup))) stop("The variable ", habillage, " is not qualitative")
      modalite <- levels(as.factor(res.mfa$call$X[, nom.quali]))
      col.ind <- as.numeric(as.factor(res.mfa$call$X[, nom.quali]))
      if (is.null(col.hab) | length(col.hab) != length(modalite))  col.hab <- 2:(1 + length(modalite))
      col.ind <- col.hab[col.ind]
      if (!is.null(res.mfa$call$ind.sup)) {
        col.ind.sup <- col.ind[res.mfa$call$ind.sup]
        col.ind <- col.ind[-res.mfa$call$ind.sup]
        col.ind.sup <- c(col.ind.sup, rep(col.ind.sup, each = nbre.grpe))
      }
      col.ind <- c(col.ind, rep(col.ind, each = nbre.grpe))
      col.ellipse <- col.ind[1:nb.ind.actif]
      col.ellipse.par <- col.ind[-c(1:nb.ind.actif)]
      col.quali <- rep("black", sum(res.mfa$call$group.mod[type == "n"]))
      if (nom.quali %in% liste.quali){
        indice.inf <- sum(nbre.modalite[0:(match(nom.quali, liste.quali) - 1)]) + 1
        indice.sup <- indice.inf + length(modalite) - 1
        if (length(group[type == "n"]) != 0) {
          for (i in seq_len(length(liste.quali))) {
            if (liste.quali[i] == nom.quali) col.quali[indice.inf:indice.sup] <- col.hab
          }
        }
      }
      col.quali <- c(col.quali, rep(col.quali, each = nbre.grpe))
      col.quali.sup <- rep("black", sum(res.mfa$call$group.mod[(type == "n")%in%num.group.sup]))
      if (nom.quali %in% liste.quali.sup){
        indice.inf.sup <- sum(nbre.modalite.sup[0:(match(nom.quali, liste.quali.sup) - 1)]) + 1
        indice.sup.sup <- indice.inf.sup + length(modalite) - 1
        if (length(group[type == "n"]) != 0) {
          for (i in seq_len(length(liste.quali.sup))) {
            if (liste.quali.sup[i] == nom.quali) col.quali.sup[indice.inf.sup:indice.sup.sup] <- col.hab
          }
        }
      }
      col.quali.sup <- c(col.quali.sup, rep(col.quali.sup, each = nbre.grpe))
    }
	
	if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY")))  dev.new(width = min(14, max(8, 8 * diff(xlim)/diff(ylim))), height = 8)
    if (is.null(title))  title <- "Individual factor map"
    if(graph.type=="classic"){
      plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp = 1, ...)
      abline(v = 0, lty = 2, ...)
      abline(h = 0, lty = 2, ...)
    }
    coo <- labe <- coll <- ipch <- fonte <- NULL
    if(graph.type=="ggplot"){
      text_ind <- text_ind.sup <- text_quali <- text_quali.sup <- NULL
      if (autoLab=="auto") autoLab <- (length(which(labe!=""))<50)
    }
    if(graph.type=="ggplot"){
      gg_graph <- ggplot() +
        coord_fixed(ratio = 1) +
        xlab(lab.x) + ylab(lab.y) +
        xlim(xlim) + ylim(ylim) +
        geom_hline(yintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
        geom_vline(xintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
        theme_light() + 
        ggoptions_default$theme +
        ggtitle(title)
    }
    if (is.na(test.invisible[1])) {
      coo <- rbind(coo,coord.ind)
      if (lab.ind){ labe <- c(labe,rownames(coord.ind))
      } else  labe <- c(labe,rep("",nrow(coord.ind)))
      coll <- c(coll,col.ind[1:nb.ind.actif])
	  coll_num <- coll   ## useful for the palette with hab = a variable, in linux !
      ipch <- c(ipch,rep(20,nrow(coord.ind)))
      fonte <- c(fonte,rep(1,nrow(coord.ind)))
      if (!is.null(selection)){
        if (is.numeric(unselect)) coll[!(seq_len(length(coll))%in%selection)] <- rgb(t(col2rgb(coll[!(seq_len(length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255)
        else coll[!(seq_len(length(coll))%in%selection)] <- unselect
		coll[(seq_len(length(coll))%in%selection)] <- rgb(t(col2rgb(coll[(seq_len(length(coll))%in%selection)])),maxColorValue=255)
        labe[!(seq_len(length(coll))%in%selection)] <- ""
      }
	  transparency_ind <- col2rgb(coll,alpha=TRUE)[4,]/255
	  coll_bis <- rgb(t(col2rgb(coll)),maxColorValue=255)
      if(graph.type == "ggplot"){
        df_ind <- cbind.data.frame(labe, coord.ind, coll, ipch, fonte,coll_bis)
          df_ind[,5][which(df_ind[,5] == 20)] <- 19
          if(habillage %in% c("none","ind","group")){
            gg_graph <- gg_graph +
              geom_point(aes(x=df_ind[,2], y=df_ind[,3]), colour= df_ind[,7],alpha = transparency_ind, shape = df_ind[,5],size = ggoptions_default$size/3)
            if(autoLab) text_ind <- ggrepel::geom_text_repel(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1]), size = ggoptions_default$size, colour = df_ind[,7], fontface = df_ind[,6])
            else{text_ind <- geom_text(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1]), size = ggoptions_default$size, colour = df_ind[,7], hjust = (-sign(df_ind[,2])+1)/2, vjust = -sign(df_ind[,3])*0.75+0.25, fontface = df_ind[,6])}
          } else{
            transparency_ind <- ifelse(rownames(df_ind) %in% df_ind[,1], 1, 1-unselect)
            if(class(habillage) %in% c("numeric","integer")) habillage <- colnames(res.mfa$call$X)[habillage]
            if(habillage %in% colnames(res.mfa$call$X)){
              gg_graph <- gg_graph +
                geom_point(aes(x=df_ind[,2], y=df_ind[,3], color = res.mfa$call$X[rownames(df_ind),habillage]), shape = ggoptions_default$point.shape, size = ggoptions_default$size/3, alpha = transparency_ind) + 
                # geom_point(aes(x=df_ind[,2], y=df_ind[,3]), color = coll_bis, shape = ggoptions_default$point.shape, size = ggoptions_default$size/3, alpha = transparency_ind) + 
                scale_color_manual(values = palette[sort(unique(coll_num))]) +
			  labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], habillage))
              if (autoLab) text_ind <- ggrepel::geom_text_repel(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1], color = (res.mfa$call$X)[rownames(df_ind),habillage]), size = ggoptions_default$size, show.legend = FALSE)
              else{text_ind <- geom_text(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1], color = res.mfa$call$X[rownames(df_ind),habillage]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind[,2])+1)/2, vjust = -sign(df_ind[,3])*0.75+0.25)}
              # if (autoLab) text_ind <- ggrepel::geom_text_repel(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1]), color = coll_bis, size = ggoptions_default$size, show.legend = FALSE)
              # else{text_ind <- geom_text(aes(x=df_ind[,2], y=df_ind[,3], label=df_ind[,1]), color = coll_bis, size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind[,2])+1)/2, vjust = -sign(df_ind[,3])*0.75+0.25)}
            } 
          }
      }
      if (!is.null(partial)){
      if(graph.type == "classic"){
        for (i in group.ind.actif) {
          if (col2rgb(coll[i],alpha=TRUE)[4]== 255){
            for (j in 1:nbre.grpe) {
			  points(coord.ind.partiel[(i - 1) * nbre.grpe + j,1],coord.ind.partiel[(i - 1) * nbre.grpe + j, 2], cex = 0.8 * par("cex"), col = col.ind[nb.ind.actif + (i - 1) * nbre.grpe + j], pch = 20)
              if (lab.par) text(coord.ind.partiel[(i - 1) * nbre.grpe + j, 1], y = coord.ind.partiel[(i - 1) * nbre.grpe + j, 2], labels = rownames(coord.ind.partiel)[(i - 1) * nbre.grpe + j], pos = 3, col = col.ind[nb.ind.actif + (i - 1) * nbre.grpe + j],...)
              if (chrono) {
                if (j > 1) lines(c(coord.ind.partiel[(i - 1) * nbre.grpe + (j - 1), 1], coord.ind.partiel[(i - 1) * nbre.grpe + j, 1]), c(coord.ind.partiel[(i - 1) * nbre.grpe + (j - 1), 2], coord.ind.partiel[(i - 1) * nbre.grpe + j, 2]), col = col.ind[i],...)
              } else lines(c(coord.ind[i, 1], coord.ind.partiel[(i - 1) * nbre.grpe + j, 1]), c(coord.ind[i, 2],coord.ind.partiel[(i - 1) * nbre.grpe + j, 2]), col = col.ind[nb.ind.actif + (i - 1) * nbre.grpe + j], lty = j,...)
			}
		  }
        }
      } else {
        liste_ind_partiel <- rep(nbre.grpe*(group.ind.actif-1),each=nbre.grpe) + rep(1:nbre.grpe,length(group.ind.actif))
	    df_ind_partial <- cbind.data.frame(matrix(rep(cbind(coord.ind[group.ind.actif,,drop=FALSE],col2rgb(coll[group.ind.actif],alpha=TRUE)[4,]),each=nbre.grpe),ncol=3),x=coord.ind.partiel[liste_ind_partiel,1],y=coord.ind.partiel[liste_ind_partiel,2], coul = col.ind[nb.ind.actif+(1:(length(group.ind.actif)*nbre.grpe))], group=rep(1:nbre.grpe,length(group.ind.actif)))
		colnames(df_ind_partial)[1:3] <- c("xfin","yfin","sel")
		df_ind_partial <- df_ind_partial[df_ind_partial[,"sel"]==255,]
        text <- NULL
        if(lab.par){
          if (autoLab) text <- ggrepel::geom_text_repel(data=df_ind_partial, aes(x=x, y = y, label = rownames(df_ind_partial), color = as.factor(coul)))
          else text <- geom_text(aes_string(data=df_ind_partial, x=x, y = y, label = rownames(df_ind_partial), color = coul))
        }
        if (habillage=="group") gg_graph <- gg_graph + geom_point(data=df_ind_partial, aes(x=x, y = y, color = as.factor(coul)), shape = 19, size = ggoptions_default$size/3) + text
        else gg_graph <- gg_graph + geom_point(data=df_ind_partial, aes(x=x, y = y), col=palette[df_ind_partial[,"coul"]], shape = 19, size = ggoptions_default$size/3) + text
		if (!chrono) {
		  if (habillage=="group") gg_graph <- gg_graph + geom_segment(aes(x = x, y = y, xend = xfin, yend = yfin, col=as.factor(coul)), lty=df_ind_partial$coul, data = df_ind_partial)
		  else gg_graph <- gg_graph + geom_segment(data = df_ind_partial, aes(x = x, y = y, xend = xfin, yend = yfin), col=palette[df_ind_partial[,"coul"]], lty=df_ind_partial$group)
		} else {
          for (i in group.ind.actif) {
            if (col2rgb(coll[i],alpha=TRUE)[4]== 255){
              for (j in 2:nbre.grpe) {
                gg_graph <- gg_graph + geom_line(aes_string(x=c(coord.ind.partiel[(i - 1) * nbre.grpe + (j - 1), 1], coord.ind.partiel[(i - 1) * nbre.grpe + j, 1]), y = c(coord.ind.partiel[(i - 1) * nbre.grpe + (j - 1), 2], coord.ind.partiel[(i - 1) * nbre.grpe + j, 2])), color = col.ind[i])
		  }}}
		}
      }
    }
	}
    if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) {
      coo <- rbind(coo,coord.ind.sup)
      if (lab.ind){ labe2 <- rownames(coord.ind.sup)
      } else  labe2 <- rep("",nrow(coord.ind.sup))
	  coll2 <- col.ind.sup[1:(nb.ind - nb.ind.actif)]
	  ipch2 <- rep(21,nrow(coord.ind.sup))
	  fonte2 <- rep(3,nrow(coord.ind.sup))
      labe <- c(labe,labe2)
      coll <- c(coll,coll2)
      ipch <- c(ipch,ipch2)
      fonte <- c(fonte,fonte2)
      if (!is.null(selectionS)){
        if (is.numeric(unselect)) coll2[!(seq_len(length(coll2))%in%selectionS)] <- rgb(t(col2rgb(coll2[!(seq_len(length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255) 
        else coll2[!(seq_len(length(coll2))%in%selectionS)] <- unselect
        labe2[!(seq_len(length(coll2))%in%selectionS)] <- ""
      }
      if (length(select)==1){
        if (grepl("contrib",select)){
          if (is.numeric(unselect)) coll2[seq_len(length(coll2))] <- rgb(t(col2rgb(coll2[seq_len(length(coll2))])),alpha=255*(1-unselect),maxColorValue=255) 
          else coll2[seq_len(length(coll2))] <- unselect
          labe2[seq_len(length(coll2))] <- ""
        }}
	if (graph.type=="ggplot"){
	    transparency_ind <- col2rgb(coll2,alpha=TRUE)[4,]/255
	    coll_bis <- rgb(t(col2rgb(coll2)),maxColorValue=255)
        df_ind.sup <- data.frame(labe2, coord.ind.sup, coll2, rep(21,nrow(coord.ind.sup)), rep(3,nrow(coord.ind.sup)),coll_bis)
        gg_graph <- gg_graph + geom_point(aes(x=df_ind.sup[,2], y=df_ind.sup[,3]), color= df_ind.sup[,7], alpha = transparency_ind, shape = df_ind.sup[,5],size=ggoptions_default$size/3)
        if(autoLab) text_ind.sup <- ggrepel::geom_text_repel(aes(x=df_ind.sup[,2], y=df_ind.sup[,3], label=df_ind.sup[,1]), size = ggoptions_default$size, color = df_ind.sup[,7], fontface = df_ind.sup[,6])
        else{text_ind.sup <- geom_text(aes(x=df_ind.sup[,2], y=df_ind.sup[,3], label=df_ind.sup[,1]), size = ggoptions_default$size, color = df_ind.sup[,7], hjust = (-sign(df_ind.sup[,2])+1)/2, vjust = -sign(df_ind.sup[,3])*0.75+0.25, fontface = df_ind.sup[,6])}
   }
  if (!is.null(partial)){
      if(graph.type == "classic"){
        for (i in group.ind.sup) {
          for (j in 1:nbre.grpe) {
              points(coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 1],coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 2], cex = 0.8 * par("cex"), col = col.ind.sup[nb.ind - nb.ind.actif + (i - 1) * nbre.grpe + j], pch = 21)
              if (lab.par) text(coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 1], y = coord.ind.partiel.sup[nb.ind + (i - 1) * nbre.grpe + j, 2], labels = rownames(coord.ind.partiel.sup)[(i - 1) * nbre.grpe + j], pos = 3, col = col.ind.sup[nb.ind - nb.ind.actif + (i - 1) * nbre.grpe + j],cex=par("cex")*0.8) 
              if (chrono) {
                if (j > 1){
                  if(graph.type == "classic") lines(c(coord.ind.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 1], coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 1]), c(coord.ind.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 2], coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 2]), col = col.ind[nb.ind.actif + i])
              }} else lines(c(coord.ind.sup[i, 1], coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 1]), c(coord.ind.sup[i, 2], coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 2]), col = col.ind.sup[nb.ind - nb.ind.actif + (i - 1) * nbre.grpe + j], lty = j)
			}
        }
      } else {
        liste_ind_sup_partiel <- rep(nbre.grpe*(group.ind.sup-1),each=nbre.grpe) + rep(1:nbre.grpe,length(group.ind.sup))
	    df_ind_partial_sup <- cbind.data.frame(matrix(rep(cbind(coord.ind.sup[group.ind.sup,,drop=FALSE],col2rgb(coll2[group.ind.sup],alpha=TRUE)[4,]),each=nbre.grpe),ncol=3),x=coord.ind.partiel.sup[liste_ind_sup_partiel,1],y=coord.ind.partiel.sup[liste_ind_sup_partiel,2], coul = col.ind.sup[(nb.ind - nb.ind.actif)+liste_ind_sup_partiel], group=rep(1:nbre.grpe,length(group.ind.sup)) )
		colnames(df_ind_partial_sup)[1:3] <- c("xfin","yfin","sel")
		df_ind_partial_sup <- df_ind_partial_sup[df_ind_partial_sup[,"sel"]==255,]
        if (habillage=="group") gg_graph <- gg_graph + geom_point(data=df_ind_partial_sup, aes(x=x, y = y, color = as.factor(coul)), shape = 21, size = ggoptions_default$size/3)
        else gg_graph <- gg_graph + geom_point(data=df_ind_partial_sup, aes(x=x, y = y), color = palette[df_ind_partial_sup[,"coul"]], shape = 21, size = ggoptions_default$size/3)
		if (!chrono) {
		  if (habillage=="group") gg_graph <- gg_graph + geom_segment(aes(x = x, y = y, xend = xfin, yend = yfin, col=as.factor(coul)), lty=df_ind_partial_sup$coul, data = df_ind_partial_sup)
		  else gg_graph <- gg_graph + geom_segment(aes(x = x, y = y, xend = xfin, yend = yfin), col=palette[df_ind_partial_sup[,"coul"]], lty=df_ind_partial_sup$group, data = df_ind_partial_sup)
		} else {
          for (i in group.ind.sup) {
            if (col2rgb(coll[i],alpha=TRUE)[4]== 255){
              for (j in 2:nbre.grpe) {
			  gg_graph <- gg_graph + geom_line(aes_string(x=c(coord.ind.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 1], coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 1]), y=c(coord.ind.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 2], coord.ind.partiel.sup[(i - 1) * nbre.grpe + j, 2])), color = col.ind[nb.ind.actif + i])
		  }}}
		}
	  }
    }
	}
    if (!is.null(coord_quali) & is.na(test.invisible[3])) {
      coo <- rbind(coo,coord_quali)
      if (lab.var){ labe3 <- rownames(coord_quali)
      } else  labe3 <- rep("",nrow(coord_quali))
      labe <- c(labe,labe3)
	  coll3 <- col.quali[seq_len(nrow(coord_quali))]
	  ipch3 <- rep(15,nrow(coord_quali))
	  fonte3 <- rep(2,nrow(coord_quali))
      coll <- c(coll,coll3)
      ipch <- c(ipch,ipch3)
      fonte <- c(fonte,fonte3)

      if(graph.type == "ggplot"){
	    coll_bis <- rgb(t(col2rgb(col.quali[seq_len(nrow(coord_quali))])),maxColorValue=255)
        df_quali <- data.frame(labe3, coord_quali, col.quali[seq_len(nrow(coord_quali))], rep(15,nrow(coord_quali)), rep(2,nrow(coord_quali)),coll_bis)
        gg_graph <- gg_graph + geom_point(aes(x=df_quali[,2], y=df_quali[,3]), color= df_quali[,7], shape = df_quali[,5],size=ggoptions_default$size/3)
        if(autoLab) text_quali <- ggrepel::geom_text_repel(aes(x=df_quali[,2], y=df_quali[,3], label=df_quali[,1]), size = ggoptions_default$size, color = df_quali[,7], fontface = df_quali[,6])
        else{text_quali <- geom_text(aes(x=df_quali[,2], y=df_quali[,3], label=df_quali[,1]), size = ggoptions_default$size, color = df_quali[,7], hjust = (-sign(df_quali[,2])+1)/2, vjust = -sign(df_quali[,3])*0.75+0.25, fontface = df_quali[,6])}
      }
     if (!is.null(partial)){
      if(graph.type == "classic"){
        for (i in group.quali) {
          for (j in 1:nbre.grpe) {
              points(coord_quali.partiel[(i - 1) * nbre.grpe +  j, 1],coord_quali.partiel[(i - 1) * nbre.grpe +  j, 2], pch = 15, col = col.quali[nrow(coord_quali) + (i - 1) * nbre.grpe + j], cex = par("cex") * 0.8)
              if (lab.var & lab.par) text(coord_quali.partiel[(i - 1) * nbre.grpe + j, 1], y = coord_quali.partiel[(i - 1) * nbre.grpe + j, 2], labels = rownames(coord_quali.partiel)[(i - 1) * nbre.grpe + j], pos = 3, col = col.quali[nrow(coord_quali) + (i - 1) * nbre.grpe + j],...)
              if (chrono) {
                if (j > 1){
                  if(graph.type == "classic") lines(c(coord_quali.partiel[(i - 1) * nbre.grpe + (j - 1), 1], coord_quali.partiel[(i - 1) * nbre.grpe + j, 1]), c(coord_quali.partiel[(i - 1) * nbre.grpe + (j - 1), 2], coord_quali.partiel[(i - 1) * nbre.grpe + j, 2]), col = col.quali[i])
              }} else lines(c(coord_quali[i, 1], coord_quali.partiel[(i - 1) * nbre.grpe + j, 1]), c(coord_quali[i, 2], coord_quali.partiel[(i - 1) * nbre.grpe + j, 2]), col = col.quali[nrow(coord_quali) + (i - 1) * nbre.grpe + j], lty = j)
		  }
        }
      } else {	  
        liste_quali_partiel <- rep(nbre.grpe*(group.quali-1),each=nbre.grpe) + rep(1:nbre.grpe,length(group.quali))
	    df_quali_partial <- cbind.data.frame(matrix(rep(cbind(coord_quali[group.quali,,drop=FALSE],col2rgb(coll3[group.quali],alpha=TRUE)[4,]),each=nbre.grpe),ncol=3),x=coord_quali.partiel[liste_quali_partiel,1],y=coord_quali.partiel[liste_quali_partiel,2], coul = col.quali[nrow(coord_quali)+liste_quali_partiel], group=rep(1:nbre.grpe,length(group.quali)))
		colnames(df_quali_partial)[1:3] <- c("xfin","yfin","sel")
		df_quali_partial <- df_quali_partial[df_quali_partial[,"sel"]==255,]
        if (habillage=="group") gg_graph <- gg_graph + geom_point(data=df_quali_partial, aes(x=x, y = y, color = as.factor(coul)), shape = 15, size = ggoptions_default$size/3)		
        else gg_graph <- gg_graph + geom_point(data=df_quali_partial, aes(x=x, y = y), color = as.factor(df_quali_partial$coul), shape = 15, size = ggoptions_default$size/3)		
		if (!chrono) {
		  if (habillage=="group") gg_graph <- gg_graph + geom_segment(aes(x = x, y = y, xend = xfin, yend = yfin, col=as.factor(coul)), lty=df_quali_partial$coul, data = df_quali_partial)
		  gg_graph <- gg_graph + geom_segment(aes(x = x, y = y, xend = xfin, yend = yfin), col=as.factor(df_quali_partial$coul), lty=df_quali_partial$group, data = df_quali_partial)
		} else {
          for (i in group.quali) {
            if (col2rgb(coll[i],alpha=TRUE)[4]== 255){
              for (j in 2:nbre.grpe) {
#			  gg_graph <- gg_graph + geom_line(aes_string(x=c(coord_quali[i, 1], coord_quali.partiel[(i - 1) * nbre.grpe + j, 1]), y=c(coord_quali[i, 2], coord_quali.partiel[(i - 1) * nbre.grpe + j, 2])), color = col.quali[nrow(coord_quali) + (i - 1) * nbre.grpe + j])}
			  gg_graph <- gg_graph + geom_line(aes_string(x=c(coord_quali.partiel[(i - 1) * nbre.grpe + (j - 1), 1], coord_quali.partiel[(i - 1) * nbre.grpe + j, 1]), y=c(coord_quali.partiel[(i - 1) * nbre.grpe + (j - 1), 2], coord_quali.partiel[(i - 1) * nbre.grpe + j, 2])), color = col.quali[nrow(coord_quali) + (i - 1) * nbre.grpe + j])}
		  }}}
		}
	  }
    }
    if (!is.null(coord_quali_sup) & is.na(test.invisible[4])) {
      coo <- rbind(coo,coord_quali_sup)
      if (lab.var){ labe4 <- rownames(coord_quali_sup)
      } else  labe4 <- rep("",nrow(coord_quali_sup))
	  coll4 <- col.quali.sup[seq_len(nrow(coord_quali_sup))]
	  ipch4 <- rep(22,nrow(coord_quali_sup))
	  fonte4 <- rep(4,nrow(coord_quali_sup))
      labe <- c(labe,labe4)
      coll <- c(coll,coll4)
      ipch <- c(ipch,ipch4)
      fonte <- c(fonte,fonte4)
      if(graph.type == "ggplot"){
	    coll_bis <- rgb(t(col2rgb(coll4)),maxColorValue=255)
        df_quali_sup <- data.frame(labe4, coord_quali_sup, coll4, ipch4, fonte4,coll_bis)
        gg_graph <- gg_graph + geom_point(aes(x=df_quali_sup[,2], y=df_quali_sup[,3]), color= df_quali_sup[,7], shape = df_quali_sup[,5],size = ggoptions_default$size/3)
        if(autoLab) text_quali.sup <- ggrepel::geom_text_repel(aes(x=df_quali_sup[,2], y=df_quali_sup[,3], label=df_quali_sup[,1]), size = ggoptions_default$size, color = df_quali_sup[,7], fontface = df_quali_sup[,6])
        else{text_quali.sup <- geom_text(aes(x=df_quali_sup[,2], y=df_quali_sup[,3], label=df_quali_sup[,1]), size = ggoptions_default$size, color = df_quali_sup[,7], hjust = (-sign(df_quali_sup[,2])+1)/2, vjust = -sign(df_quali_sup[,3])*0.75+0.25, fontface = df_quali_sup[,6])}
     }
     if (!is.null(partial)){
      if(graph.type == "classic"){
        for (i in group.quali.sup) {
          for (j in 1:nbre.grpe) {
              points(coord_quali.partiel.sup[(i - 1) * nbre.grpe +j, 1],coord_quali.partiel.sup[(i - 1) * nbre.grpe +j, 2], pch = 22, col = col.quali.sup[nrow(coord_quali_sup) + (i - 1) * nbre.grpe + j], cex = par("cex") * 0.8)
              if (lab.var & lab.par) text(coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 1], y = coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 2], labels = rownames(coord_quali.partiel.sup)[(i - 1) * nbre.grpe + j], pos = 3, col = col.quali.sup[nrow(coord_quali_sup) + (i - 1) * nbre.grpe + j],...)
             
              if (chrono) {
                if (j > 1){
                  if(graph.type == "classic") lines(c(coord_quali.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 1], coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 1]), c(coord_quali.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 2], coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 2]), col = col.quali.sup[nrow(coord_quali_sup) + i])
              }} else lines(c(coord_quali_sup[i, 1], coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 1]), c(coord_quali_sup[i, 2], coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 2]), col = col.quali.sup[nrow(coord_quali_sup) + (i - 1) * nbre.grpe + j], lty = j)
			}
        }
      } else {
        liste_quali_sup_partiel <- rep(nbre.grpe*(group.quali.sup-1),each=nbre.grpe) + rep(1:nbre.grpe,length(group.quali.sup))
	    df_quali_partial_sup <- cbind.data.frame(matrix(rep(cbind(coord_quali_sup[group.quali.sup,,drop=FALSE],col2rgb(coll4[group.quali.sup],alpha=TRUE)[4,]),each=nbre.grpe),ncol=3),x=coord_quali.partiel.sup[liste_quali_sup_partiel,1],y=coord_quali.partiel.sup[liste_quali_sup_partiel,2], coul = col.quali.sup[nrow(coord_quali_sup)+liste_quali_sup_partiel], group=rep(1:nbre.grpe,length(group.quali.sup)) )
		colnames(df_quali_partial_sup)[1:3] <- c("xfin","yfin","sel")
		df_quali_partial_sup <- df_quali_partial_sup[df_quali_partial_sup[,"sel"]==255,]
        if (habillage=="group") gg_graph <- gg_graph + geom_point(data=df_quali_partial_sup, aes(x=x, y = y, color = as.factor(coul)), shape = 15, size = ggoptions_default$size/3)
        else gg_graph <- gg_graph + geom_point(data=df_quali_partial_sup, aes(x=x, y = y), col = as.factor(df_quali_partial_sup$coul), shape = 15, size = ggoptions_default$size/3)
		if (!chrono){
		  if (habillage=="group") gg_graph <- gg_graph + geom_segment(data = df_quali_partial_sup, aes(x = x, y = y, xend = xfin, yend = yfin, color=as.factor(coul)), lty=df_quali_partial_sup$coul)
		  else gg_graph <- gg_graph + geom_segment(data = df_quali_partial_sup, aes(x = x, y = y, xend = xfin, yend = yfin), col=as.factor(df_quali_partial_sup[,"coul"]), lty=df_quali_partial_sup$group)
		} else {
		  for (i in group.quali.sup) {
            if (col2rgb(coll[i],alpha=TRUE)[4]== 255){
              for (j in 2:nbre.grpe) {
			  gg_graph <- gg_graph + geom_line(aes_string(x=c(coord_quali.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 1], coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 1]), y=c(coord_quali.partiel.sup[(i - 1) * nbre.grpe + (j - 1), 2], coord_quali.partiel.sup[(i - 1) * nbre.grpe + j, 2])), color = col.quali.sup[nrow(coord_quali_sup) + (i - 1) * nbre.grpe + j])}
		  }}}
		}
	  }
    }
    if(graph.type == "ggplot") {
	  gg_graph <- gg_graph + theme + text_ind + text_ind.sup + text_quali + text_quali.sup
	  if (!is.null(partial)) gg_graph <- gg_graph + labs(color = "")
	} else {
      if (shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
      if (any(labe!="")){
        if (autoLab=="auto") autoLab <- (length(which(labe!=""))<50)
        if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],shadotext=shadowtext,...)
        if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)		
      }
      if (!shadowtext) points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
    }
    if ((!is.null(partial)) & (habillage == "group")) {
      L <- list(x="topleft", legend = rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                                                                    length(rownames(res.mfa$group$Lg)))], lty = seq_len(length(rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                                                                    length(rownames(res.mfa$group$Lg)))])), text.col = col.hab, col = col.hab, cex = par("cex")*0.8)
        L <- modifyList(L, legend)
        if(graph.type == "classic") do.call(graphics::legend, L)
    }
    if ((!is.null(partial)) & (habillage != "group")) {
        L <- list(x="topleft", legend = rownames(res.mfa$group$Lg)[-c(num.group.sup, length(rownames(res.mfa$group$Lg)))], 
                  lty = seq_len(length(rownames(res.mfa$group$Lg)[-c(num.group.sup, length(rownames(res.mfa$group$Lg)))])), cex = par("cex")*0.8)
        L <- modifyList(L, legend)
        if(graph.type == "classic") do.call(graphics::legend, L)
      }
      if ((habillage != "none") & (habillage != "ind") & (habillage != "group")) {
        L <- list(x="topleft", legend = levels(res.mfa$call$X[, habillage]), text.col = col.hab, cex = par("cex")*0.8)
        L <- modifyList(L, legend)
        if(graph.type == "classic") do.call(graphics::legend, L)
      }
    if(graph.type=="ggplot" & habillage == "group" & !is.null(partial)){
      gg_graph <- gg_graph + scale_color_manual(values = palette[L$text.col[order(L$text.col)]], labels = L$legend[order(L$text.col)])
    }
      
    if (!is.null(coord.ellipse) & is.na(test.invisible[2])) {
      for (e in 1:nb.ind.actif) {
        debut <- ((nb.ind.actif - 1) * npoint.ellipse) + 1
        fin <- debut + npoint.ellipse - 1
        data.elli <- coord.ellipse[debut:fin, -1]
        if (graph.type == "classic") lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse[e])
      }
    }
    if (!is.null(coord.ellipse)) {
      for (e in 1:nlevels(coord.ellipse[, 1])) {
        data.elli <- coord.ellipse[(npoint.ellipse * 
                                      (e - 1) + 1):(npoint.ellipse * e), -1]
        if (graph.type == "classic") lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse[e])
      }
    }
    if (!is.null(coord.ellipse.par)) {
      for (i in group.ind.actif) {
        for (j in 1:nbre.grpe) {
          ind.e <- (i - 1) * nbre.grpe + j
          data.elli <- coord.ellipse.par[(npoint.ellipse.par * 
                                            (ind.e - 1) + 1):(npoint.ellipse.par * ind.e), -1]
          if (graph.type == "classic") lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse.par[ind.e], 
                                             lty = 2)
        }
      }
    }
  }
   palette(old.palette)
  if (graph.type == "ggplot") return(gg_graph)
}