plot.PCA <- function (x, axes = c(1, 2), choix = c("ind","var","varcor"),
                      ellipse = NULL, xlim = NULL, ylim = NULL, habillage = "none", 
                      col.hab = NULL, col.ind = "black", col.ind.sup = "blue", 
                      col.quali = "magenta", col.quanti.sup = "blue", 
                      col.var = "black", label=c("all","none","ind", "ind.sup", "quali", "var", "quanti.sup"), 
                      invisible = c("none","ind", "ind.sup", "quali","var", "quanti.sup"), lim.cos2.var = 0.,
                      title = NULL, palette=NULL, autoLab=c("auto","yes","no"),new.plot=FALSE, 
                      select=NULL, unselect = 0.7,shadowtext = FALSE, legend = list(bty = "y", x = "topleft"),
                      graph.type = c("ggplot","classic"), ggoptions = NULL,  ...){
  
  res.pca <- x
  argument <- list(...)
  if (!is.null(argument[["cex"]]) & is.null(ggoptions["size"]))  ggoptions["size"] <- 4*argument$cex
  ggoptions_default <- list(size = 4, point.shape = 19, line.lty = 2, line.lwd = 0.5, line.color = "black", segment.lty = 1, segment.lwd = 0.5, circle.lty = 1, circle.lwd = 0.5, circle.color = "black", low.col.quanti = "blue", high.col.quanti = "red3")
  if (!is.null(ggoptions[1])) ggoptions_default[names(ggoptions)] = ggoptions[names(ggoptions)]
  if (!inherits(res.pca, "PCA")) stop("non convenient data")
  if (is.numeric(unselect)) if ((unselect>1)|(unselect<0)) stop("unselect should be betwwen 0 and 1")
  autoLab <- match.arg(autoLab,c("auto","yes","no"))
  if (autoLab == "yes") autoLab=TRUE 
  if (autoLab == "no") autoLab=FALSE
  label <- match.arg(label,c("all","none","ind", "ind.sup", "quali", "var", "quanti.sup"),several.ok=TRUE)
  invisible <- match.arg(invisible,c("none","ind", "ind.sup", "quali","var", "quanti.sup"),several.ok=TRUE)
  if ("none"%in%invisible) invisible = NULL
  choix <- match.arg(choix,c("ind","var","varcor"))
  graph.type <- match.arg(graph.type[1],c("ggplot","classic"))
  lab.ind <- lab.quali <- lab.var <- lab.quanti <- lab.ind.sup <- FALSE
  if (length(label)==1 && label=="all") lab.ind <- lab.quali <- lab.var <- lab.quanti <- lab.ind.sup <-TRUE
  if ("ind" %in% label) lab.ind<-TRUE
  if ("quali" %in% label) lab.quali<-TRUE
  if ("var" %in% label) lab.var<-TRUE
  if ("quanti.sup" %in% label) lab.quanti<-TRUE
  if ("ind.sup" %in% label) lab.ind.sup<-TRUE
  lab.x <- paste("Dim ",axes[1]," (",format(res.pca$eig[axes[1],2],nsmall=2,digits=2),"%)",sep="")
  lab.y <- paste("Dim ",axes[2]," (",format(res.pca$eig[axes[2],2],nsmall=2,digits=2),"%)",sep="")
  if (graph.type == "ggplot"){
    if(!is.null(col.hab)) palette <- col.hab
    theme <- theme(
      axis.title = element_text(hjust = 1, size = if (is.null(argument[["cex.axis"]])) {10} else {10*argument$cex.axis},face = 2),
      plot.title = element_text(hjust = 0.5, size = if (is.null(argument[["cex.main"]])) {11} else {11*argument$cex.main},face = 2),
      legend.position = ifelse(legend$x %in% c("bottom","up","right","left"), legend$x, "right"),
      legend.box.spacing=unit(0.1, 'cm'),legend.margin=margin()
    )
    
    
    liste.quali <- colnames(res.pca$call$quali.sup$quali.sup)
    liste.quanti <- colnames(res.pca$call$X)[which(!(colnames(res.pca$call$X) %in% liste.quali))]
    hab_2 <- c(colnames(res.pca$call$X), "contrib", "cos2")
    
    if((habillage != "none") && !(habillage[1] %in% hab_2) && (habillage != "ind")) habillage[1] = colnames(res.pca$call$X)[as.numeric(habillage[1])]
    
    if(habillage[1] != "none" && length(habillage) == 2){
      if(!habillage[2] %in% hab_2) habillage[2] = colnames(res.pca$call$X)[as.numeric(habillage[2])]
      if (length(habillage) > 2) {
        warning("Habillage must be either length 1 or 2 : only 2 first arguments will be used")
        habillage = habillage[1:2]
      }
      if ((length(habillage) == 2) & !("cos2" %in% habillage) & !("contrib" %in% habillage)){
        if(!(habillage[2] %in% liste.quali)){
          if (!(habillage[1] %in% liste.quali)){
            habillage = habillage[1]
          }
          else{
            habillage = habillage[2:1]
          }
        }}
      
      if(length(habillage) == 1 && !(habillage %in% hab_2)) habillage = as.numeric(habillage)
      if((length(habillage) == 2) & (habillage[2] %in% c("contrib","cos2"))) habillage = habillage[2:1]
      if((length(habillage) == 2) & (habillage[1] %in% c("contrib","cos2")) & !(habillage[2] %in% hab_2)) habillage[2] = colnames(res.pca$call$X)[as.integer(habillage[2])]
      if(class(habillage[1]) %in% c("numeric","integer") && class(habillage[2]) %in% c("numeric","integer")) habillage = c(colnames(res.pca$call$X)[habillage[1]],colnames(res.pca$call$X)[habillage[2]])
      
      if(("cos2" %in% habillage) || ("contrib" %in% habillage)){
        if((habillage[2] %in% liste.quanti) || (habillage[1] %in% liste.quanti)) habillage = habillage[1]
      }
      if(("cos2" %in% habillage) && ("contrib" %in% habillage)) habillage = habillage[1]
      
    }
  }
  if (choix == "ind") {
    if (is.null(title)) titre <- "PCA graph of individuals"
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
	nullxlimylim <- (is.null(xlim) & is.null(ylim))
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.actif[,1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.actif[,1])
      if(!is.null(coord.illu)&is.na(test.invisible[2])) xmin <- min(xmin, coord.illu[, 1])
      if(!is.null(coord.illu)&is.na(test.invisible[2])) xmax <- max(xmax, coord.illu[, 1])
      if(!is.null(coord.quali)&is.na(test.invisible[3])) xmin <- min(xmin, coord.quali[, 1])
      if(!is.null(coord.quali)&is.na(test.invisible[3])) xmax <- max(xmax, coord.quali[, 1])
      if(!is.null(coord.ellipse)&is.na(test.invisible[3])) xmin <- min(xmin, coord.ellipse[, 2])
      if(!is.null(coord.ellipse)&is.na(test.invisible[3])) xmax <- max(xmax, coord.ellipse[, 2])
      # xlim <- c(xmin, xmax) * 1.2
      xlim <- c(xmin, xmax)
      xlim <- (xlim-mean(xlim))*1.2 + mean(xlim)
    }
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.actif[,2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.actif[,2])
      if(!is.null(coord.illu)&is.na(test.invisible[2])) ymin <- min(ymin, coord.illu[, 2])
      if(!is.null(coord.illu)&is.na(test.invisible[2])) ymax <- max(ymax, coord.illu[, 2])
      if(!is.null(coord.quali)&is.na(test.invisible[3])) ymin <- min(ymin, coord.quali[, 2])
      if(!is.null(coord.quali)&is.na(test.invisible[3])) ymax <- max(ymax, coord.quali[, 2])
      if(!is.null(coord.ellipse)&is.na(test.invisible[3])) ymin <- min(ymin, coord.ellipse[, 3])
      if(!is.null(coord.ellipse)&is.na(test.invisible[3])) ymax <- max(ymax, coord.ellipse[, 3])
      ylim <- c(ymin, ymax)
      ylim <- (ylim-mean(ylim))*1.2 + mean(ylim)
    }
    if (nullxlimylim & diff(xlim)/diff(ylim)>3) ylim <- (ylim-mean(ylim))*diff(xlim)/diff(ylim)/3 + mean(ylim)
    if (nullxlimylim & diff(xlim)/diff(ylim)<1/2) xlim <- (xlim-mean(xlim))*diff(ylim)/diff(xlim)/2 + mean(xlim)
    if(graph.type=="ggplot") nudge_y <- (ylim[2] - ylim[1])*0.03
    selection <- NULL
    if (!is.null(select)) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.pca$ind$coord)%in%select)+sum(rownames(res.pca$ind.sup$coord)%in%select)!=0) selection <- which(rownames(res.pca$ind$coord)%in%select)
        else {
          if (grepl("contrib",select[1])) selection <- (rev(order(res.pca$ind$contrib[,axes[1],drop=FALSE]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2],drop=FALSE]*res.pca$eig[axes[2],1])))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          # 		    if (grepl("contrib",select)) selection <- (rev(order(apply(res.pca$ind$contrib[,axes],1,sum))))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("dist",select[1])) selection <- (rev(order(res.pca$ind$dist)))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"dist"))),na.rm=T))]
          if (grepl("coord",select[1])) selection <- (rev(order(apply(res.pca$ind$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$ind$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select[1])) {
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
          if (grepl("dist",select[1])) selectionS <- (rev(order(res.pca$ind.sup$dist)))[1:min(nrow(res.pca$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"dist"))),na.rm=T))]
          if (grepl("coord",select[1])) selectionS <- (rev(order(apply(res.pca$ind.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$ind.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select[1])) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selectionS <- (rev(order(apply(res.pca$ind.sup$cos2[,axes,drop=FALSE],1,sum))))[1:min(nrow(res.pca$ind.sup$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selectionS <- which(apply(res.pca$ind.sup$cos2[,axes,drop=FALSE],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selectionS <- select
        }  
      }
    }
    ## PARTIE GRAPHIQUE
    if (graph.type =="ggplot") color.ind <- NULL
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new(width=min(14,8*diff(xlim)/diff(ylim)),height=8)
    if (is.null(palette)) palette = (c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
    if (habillage[1] == "none") {
      color.ind <- rep(col.ind,nrow(coord.actif))
      color.mod <- col.quali
      if (!is.null(res.pca$ind.sup)&is.na(test.invisible[2])) color.ind.sup <- rep(col.ind.sup,nrow(res.pca$ind.sup$coord))
    }
    if (habillage[1] == "ind") {
      if (is.null(coord.illu)){
	    if(length(col.hab) == nrow(coord.actif)) color.ind <- col.hab
        else color.ind <- c(1:nrow(coord.actif))
      } else{
	    if (length(col.hab)== nrow(coord.actif)+nrow(coord.illu)){
          color.ind <- col.hab[-res.pca$call$ind.sup]
		  color.ind.sup <- col.hab[res.pca$call$ind.sup]
		} else {
		  color.ind <- c(1:nrow(coord.actif))
		  color.ind.sup <- c((nrow(coord.actif)+1):(nrow(coord.actif)+nrow(coord.illu)))
		}
      }
	  color.mod <- "darkred"
    }
    liste.quali <- NULL
    if ((habillage[1] != "none")&(habillage[1] != "ind")&(habillage[1] != "cos2")&(habillage[1] != "contrib")){
      liste.quali <- colnames(res.pca$call$quali.sup$quali.sup)
      if(!(class(res.pca$call$X[,habillage[1]])[1] %in% c("numeric","double","integer"))) {
        if (is.numeric(habillage)) nom.quali <- colnames(res.pca$call$X)[habillage[1]]
        else nom.quali <- habillage[1]
        if (!(nom.quali %in% liste.quali)) stop("The variable ", habillage[1], " is not qualitative")
        n.mod <- res.pca$call$quali.sup$modalite[liste.quali == nom.quali]
        if (length(col.hab) != n.mod) {
          color.mod <- c(1:n.mod)
          color.ind <- as.numeric(as.factor(res.pca$call$X[, nom.quali]))
          color.ind.sup <- color.ind[res.pca$call$ind.sup]
          if (!is.null(res.pca$call$ind.sup)) color.ind <- color.ind[-res.pca$call$ind.sup]
        }
        else {
          color.mod <- col.hab
          color.ind <- as.factor(res.pca$call$X[, nom.quali])
          levels(color.ind) <- col.hab
          color.ind.sup <- color.ind[res.pca$call$ind.sup]
          if (!is.null(res.pca$call$ind.sup)) color.ind <- color.ind[-res.pca$call$ind.sup]
          color.ind <- as.character(color.ind)
        }
      }
      
      if(class(res.pca$call$X[,habillage[1]])[1] %in% c("numeric","double","integer")){
        if (graph.type == "classic") stop("The variable ", habillage[1], "is not qualitative")
        liste.quanti <- colnames(res.pca$call$X[which(!(colnames(res.pca$call$X) %in% colnames(res.pca$call$quali.sup$quali.sup)))])
        if (is.numeric(habillage[1])) nom.quanti <- colnames(res.pca$call$X)[habillage[1]]
        else nom.quanti <- habillage[1]
        if (!(nom.quanti %in% liste.quanti)) stop("The variable ", habillage[1], " is not quantitative")
      }}
    color.sup <- col.ind.sup
    if (graph.type == "classic" & ((habillage == "cos2") || (habillage == "contrib"))) stop("The variable is not qualitative")
    # graphe individuals factor map
    if (graph.type == "classic") {
      plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, ...)
      abline(v=0,lty=2, ...)
      abline(h=0,lty=2, ...)
      #
    }
    coo <- labe <- coll <- ipch <- fonte <- NULL
    df_ind2 <- df_ind_sup <- df_quali.sup <- NULL
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
      df_ind2 <- data.frame(labe,coord.actif,ipch,fonte)
    }
    if(graph.type == "ggplot") coll2 <- NULL
    if (!is.null(res.pca$ind.sup) & is.na(test.invisible[2])) {
      coo <- rbind(coo,res.pca$ind.sup$coord[,axes])
      if (lab.ind.sup){ labe2 <- rownames(res.pca$ind.sup$coord)
      } else  labe2 <- rep("",nrow(res.pca$ind.sup$coord))
      coll2 <- color.sup
      if (!is.null(selectionS)){
        if (is.numeric(unselect)) coll2[!((1:length(coll2))%in%selectionS)] = rgb(t(col2rgb(coll2[!((1:length(coll2))%in%selectionS)])),alpha=255*(1-unselect),maxColorValue=255) 
        else coll2[!((1:length(coll2))%in%selectionS)] <- unselect
        labe2[!((1:length(coll2))%in%selectionS)] <- ""
      }
      if (length(select)==1){
        if (grepl("contrib",select)){
          if (is.numeric(unselect)) coll2[1:length(coll2)] = rgb(t(col2rgb(coll2[1:length(coll2)])),alpha=255*(1-unselect),maxColorValue=255) 
          else coll2[1:length(coll2)] = unselect
          labe2[1:length(coll2)] <- ""
        }}
      df_ind_sup <- data.frame(labe2, res.pca$ind.sup$coord[,axes], coll2, rep(1,nrow(res.pca$ind.sup$coord)), rep(3,nrow(res.pca$ind.sup$coord)))
      coll <- c(coll,coll2)
      labe <- c(labe,labe2)
      ipch <- c(ipch,rep(1,nrow(res.pca$ind.sup$coord)))
      fonte <- c(fonte,rep(3,nrow(res.pca$ind.sup$coord)))
    }
    if (!is.null(coord.quali) & is.na(test.invisible[3])) {
      modalite <- res.pca$call$quali.sup$modalite
      if (graph.type == "ggplot") col.quali <- col.quali
      else{col.quali<-rep(col.quali, length(modalite))}
      num.li <- 0
      coo <- rbind(coo,coord.quali)
      ipch <- c(ipch,rep(22,sum(modalite)))
      if (lab.quali){ labe2 <- rownames(coord.quali)
      } else  labe2 <- rep("",sum(modalite))
      labe <- c(labe,labe2)
      fonte <- c(fonte,rep(3,sum(modalite)))
      for (q in 1:length(modalite)) {
        if ((habillage[1] != "none")&(habillage[1] != "ind")&(habillage[1] != "cos2")&(habillage[1] != "contrib")) {
          if(!(class(res.pca$call$X[,habillage[1]])[1] %in% c("numeric","double","integer"))){
            if (q == match(nom.quali, liste.quali)) coll2 <- color.mod
            else coll2 <- rep(col.quali[1],modalite[q])
          }} else coll2 <- rep(col.quali,modalite[q])
          num.li <- num.li + modalite[q]
      }
      coll <- c(coll,coll2)
      df_quali.sup <- data.frame(labe2, coord.quali, rep(22,nrow(coord.quali)), rep(3,nrow(coord.quali)))
    }
    # graphe individuals factor map, ajout des points
    if (graph.type == "classic") {
      points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, ...)
    }
    if (graph.type == "ggplot") {
      if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
      df_ind <- data.frame(labe,coo,ipch,fonte)
          gg_graph <- ggplot() +
            coord_fixed(ratio = 1) +
            xlim(xlim) + ylim(ylim) +
            geom_hline(yintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
            geom_vline(xintercept = 0,lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) +
            theme_light() +
			labs(title = titre, x = lab.x, y= lab.y)
      
      if(!is.null(select)) df_ind2[,1] <- ifelse(rownames(df_ind2) %in% rownames(df_ind2)[selection], rownames(df_ind2), "")
       transparency_ind <- col2rgb(col.ind,alpha=TRUE)[4]/255
       if (!is.null(select)) transparency_ind <- ifelse(rownames(res.pca$ind$coord) %in% rownames(res.pca$ind$coord)[selection], transparency_ind, transparency_ind*(1-unselect))
      if((!is.na(test.invisible[1])) & (habillage[1] != "none") & (is.null(legend["title"][[1]]))) legend["title"][[1]] = habillage[1]
      if (is.na(test.invisible[1])){
        if (habillage[1] == "none" | habillage[1]=="ind"){
          gg_graph <- gg_graph +
            geom_point(aes(x=df_ind2[,2], y=df_ind2[,3]), color=color.ind, shape = ggoptions_default$point.shape, size = ggoptions_default$size/3, alpha = transparency_ind)
          if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1]), size = ggoptions_default$size, color = color.ind)
          else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1]), size = ggoptions_default$size, color = color.ind, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)}
        }
        if (length(habillage) == 1 & (habillage[1] != "none" | habillage[1]!="ind")){
          if ((habillage %in% colnames(res.pca$call$X)) & !(habillage %in% liste.quali)){
            df_ind2 <- data.frame(df_ind2, (res.pca$call$X)[rownames(df_ind2),habillage])
            gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], color = df_ind2[,6]), shape = ggoptions_default$point.shape, size = ggoptions_default$size/3, alpha = transparency_ind) + 
              scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
              labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], habillage))
            if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = df_ind2[,6]), size = ggoptions_default$size, show.legend = FALSE)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = df_ind2[,6]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)}
          }
          if (habillage %in% liste.quali){
            df_ind2 <- data.frame(df_ind2, (res.pca$call$X)[rownames(df_ind2),habillage])
            gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], color = (res.pca$call$X)[rownames(df_ind2),habillage]), shape = ggoptions_default$point.shape, size = ggoptions_default$size/3, alpha = transparency_ind) + 
              scale_color_manual(values = palette[1:length(levels((res.pca$call$X)[,habillage]))]) +
              labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], habillage))
            if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size, show.legend = FALSE)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)}
          }
          if (habillage == "cos2"){
          gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], color = res.pca$ind$cos2[,axes[1]] + res.pca$ind$cos2[,axes[2]]), shape = ggoptions_default$point.shape, size = ggoptions_default$size/3, alpha = transparency_ind) +
              scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
              labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "cos2")) 
            if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = res.pca$ind$cos2[,axes[1]] + res.pca$ind$cos2[,axes[2]]), size = ggoptions_default$size)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = res.pca$ind$cos2[,axes[1]] + res.pca$ind$cos2[,axes[2]]), size = ggoptions_default$size, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)}
          }
          if (habillage == "contrib"){
          gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], color = (res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1])/(res.pca$eig[axes[1],1]+res.pca$eig[axes[2],1])), shape = ggoptions_default$point.shape, size = ggoptions_default$size/3, alpha = transparency_ind) + 
              scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
              labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "Ctr")) 
            if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1])/(res.pca$eig[axes[1],1]+res.pca$eig[axes[2],1])), size = ggoptions_default$size)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1])/(res.pca$eig[axes[1],1]+res.pca$eig[axes[2],1])), size = ggoptions_default$size, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)
            }
          }}
        if(length(habillage) == 2 & habillage[1] != "none"){
          if (!(habillage[1] %in% liste.quali)){
          gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], shape = res.pca$call$X[rownames(res.pca$ind$coord),habillage[2]], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size/3, alpha = transparency_ind) + 
              scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
              labs(color = habillage[1], shape = habillage[2]) 
            if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size, show.legend = FALSE)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)}
          }
          if (habillage[1] %in% liste.quali){
          gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], shape = res.pca$call$X[rownames(res.pca$ind$coord),habillage[2]], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size/3, alpha = transparency_ind) + 
              scale_color_manual(values = palette[1:length(levels((res.pca$call$X)[,habillage[1]]))]) +
              labs(color = habillage[1], shape = habillage[2])
            if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size, show.legend = FALSE)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$call$X)[rownames(res.pca$ind$coord),habillage[1]]), size = ggoptions_default$size, show.legend = FALSE, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)}
          }
          if (habillage[1] == "cos2"){
          gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], color = res.pca$ind$cos2[,axes[1]] + res.pca$ind$cos2[,axes[2]], shape = res.pca$call$X[rownames(res.pca$ind$coord),habillage[2]]), size = ggoptions_default$size/3, alpha = transparency_ind) +
              scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
              labs(color = habillage[1], shape = habillage[2]) 
            if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = res.pca$ind$cos2[,axes[1]] + res.pca$ind$cos2[,axes[2]]), size = ggoptions_default$size)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = res.pca$ind$cos2[,axes[1]] + res.pca$ind$cos2[,axes[2]]), size = ggoptions_default$size, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)}
          }
          if (habillage[1] == "contrib"){
          gg_graph <- gg_graph +
              geom_point(aes(x=df_ind2[,2], y=df_ind2[,3], color = (res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1])/(res.pca$eig[axes[1],1]+res.pca$eig[axes[2],1]), shape = res.pca$call$X[rownames(res.pca$ind$coord),habillage[2]]), size = ggoptions_default$size/3, alpha = transparency_ind) +
              scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti) +
              labs(color = habillage[1], shape = habillage[2])
            if (autoLab) text <- ggrepel::geom_text_repel(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1])/(res.pca$eig[axes[1],1]+res.pca$eig[axes[2],1])), size = ggoptions_default$size)
            else{text <- geom_text(aes(x=df_ind2[,2], y=df_ind2[,3], label=df_ind2[,1], color = (res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1])/(res.pca$eig[axes[1],1]+res.pca$eig[axes[2],1])), size = ggoptions_default$size, hjust = (-sign(df_ind2[,2])+1)/2, vjust = -sign(df_ind2[,3])*0.75+0.25)
            }
          }
        }
      }
      if (!is.na(test.invisible[1])){
          gg_graph <- gg_graph +
          labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], habillage[1]))
      }
      gg_graph <- gg_graph + theme
      if (is.na(test.invisible[1]) & isTRUE(lab.ind)) gg_graph <- gg_graph + text
      
      if ((!is.null(res.pca$ind.sup)) && (is.na(test.invisible[2]))){
        if(!is.null(select)) df_ind_sup[,1] <- ifelse(rownames(df_ind_sup) %in% rownames(df_ind_sup)[selectionS], rownames(df_ind_sup), "")
        if(nrow(res.pca$ind.sup$coord) > 1){
          if (isTRUE(lab.ind.sup)){
            if (habillage[1] == "none"){ gg_graph <- gg_graph + geom_point(aes(x = df_ind_sup[,2], y = df_ind_sup[,3]), size = ggoptions_default$size/3, color = color.ind.sup, shape = 1)
            if (autoLab) text_ind.sup <- ggrepel::geom_text_repel(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], label=df_ind_sup[,1]), color = color.ind.sup, size = ggoptions_default$size, fontface = "italic")
            else{text_ind.sup <- geom_text(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], label=df_ind_sup[,1]), color = color.ind.sup, size = ggoptions_default$size, fontface = "italic",hjust = (-sign(df_ind_sup[,2])+1)/2, vjust = -sign(df_ind_sup[,3])*0.75+0.25)}
            }
            else{ if (habillage[1] == "cos2"){ gg_graph <- gg_graph + geom_point(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], color = res.pca$ind.sup$cos2[,axes[1]] + res.pca$ind.sup$cos2[,axes[2]]), size = ggoptions_default$size/3, shape = 1)
            if (autoLab) text_ind.sup <- ggrepel::geom_text_repel(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], label=df_ind_sup[,1], color = res.pca$ind.sup$cos2[,axes[1]] + res.pca$ind.sup$cos2[,axes[2]]), size = ggoptions_default$size, fontface = "italic")
            else{text_ind.sup <- geom_text(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], label=df_ind_sup[,1], color = res.pca$ind.sup$cos2[,axes[1]] + res.pca$ind.sup$cos2[,axes[2]]), size = ggoptions_default$size, fontface = "italic")}
            }
              else{ if (habillage[1] == "contrib") text_ind.sup <- NULL #+ geom_point(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]], color = res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1]), size = ggoptions_default$size/3, shape = 1)
              else{gg_graph <- gg_graph + geom_point(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], color = (res.pca$call$X)[rownames(res.pca$ind.sup$coord),habillage[1]]), size = ggoptions_default$size/3, shape = 1)
              if (autoLab) text_ind.sup <- ggrepel::geom_text_repel(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], label=df_ind_sup[,1], color = (res.pca$call$X)[rownames(res.pca$ind.sup$coord),habillage[1]]), size = ggoptions_default$size, fontface = "italic", show.legend = FALSE)
              else{text_ind.sup <- geom_text(aes(x = df_ind_sup[,2], y = df_ind_sup[,3], label=df_ind_sup[,1], color = (res.pca$call$X)[rownames(res.pca$ind.sup$coord),habillage[1]]), size = ggoptions_default$size, fontface = "italic", show.legend = FALSE)
              }}}}
            gg_graph <- gg_graph + text_ind.sup
          }
          else{
            gg_graph <- gg_graph +
              geom_point(aes(x = df_ind_sup[,2], y = df_ind_sup[,3]), size = ggoptions_default$size/3, color = color.ind.sup)
          }
        }
        else{ if(dim(res.pca$ind.sup$coord)[1] == 1){
          if (is.null(select)) selectionS = 1
          if (isTRUE(lab.ind.sup)){
            if (habillage[1] == "none") gg_graph <- gg_graph + geom_point(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]]), size = ggoptions_default$size/3, color = color.ind.sup, shape = 1) + ggrepel::geom_text_repel(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]], label=ifelse(!is.null(selectionS), rownames(res.pca$ind.sup$coord), "")), color = color.ind.sup, size = ggoptions_default$size, fontface = "italic")
            else{ if (habillage[1] == "cos2") gg_graph <- gg_graph + geom_point(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]], color = res.pca$ind.sup$cos2[,axes[1]] + res.pca$ind.sup$cos2[,axes[2]]), size = ggoptions_default$size/3, shape = 1) + ggrepel::geom_text_repel(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]], label=ifelse(!is.null(selectionS), rownames(res.pca$ind.sup$coord), ""), color = res.pca$ind.sup$cos2[,axes[1]] + res.pca$ind.sup$cos2[,axes[2]]), size = ggoptions_default$size, fontface = "italic")
            else{ if (habillage[1] == "contrib") gg_graph <- gg_graph #+ geom_point(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]], color = res.pca$ind$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$ind$contrib[,axes[2]]*res.pca$eig[axes[2],1]), size = ggoptions_default$size/3, shape = 1)
            else{gg_graph <- gg_graph + geom_point(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]], color = (res.pca$call$X)[rownames(res.pca$ind.sup$coord),habillage[1]]), size = ggoptions_default$size/3, shape = 1) + ggrepel::geom_text_repel(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]], label=ifelse(!is.null(selectionS), rownames(res.pca$ind.sup$coord), ""), color = (res.pca$call$X)[rownames(res.pca$ind.sup$coord),habillage[1]]), size = ggoptions_default$size, fontface = "italic", show.legend = FALSE)
            }}}
          }
          else{
            gg_graph <- gg_graph +
              geom_point(aes(x = res.pca$ind.sup$coord[,axes[1]], y = res.pca$ind.sup$coord[,axes[2]]), size = ggoptions_default$size/3, color = color.ind.sup)
          }
        }
        }
      }
      if ((!is.null(res.pca$quali.sup)) && (is.na(test.invisible[3]))){
        if (isTRUE(lab.quali)){
          if((habillage[1] == "none") || !(habillage[1] %in% liste.quali)){
            gg_graph <- gg_graph +
              geom_point(aes(x = df_quali.sup[,2], y = df_quali.sup[,3]), size = ggoptions_default$size/2.8, color = col.quali, shape = 0)
            if (autoLab) text_quali.sup <- ggrepel::geom_text_repel(aes(x = df_quali.sup[,2], y = df_quali.sup[,3], label=df_quali.sup[,1]), color = col.quali, size = ggoptions_default$size, fontface = "italic")
            else{text_quali.sup <- geom_text(aes(x = df_quali.sup[,2], y = df_quali.sup[,3], label=df_quali.sup[,1]), color = col.quali, size = ggoptions_default$size, fontface = "italic",hjust = (-sign(df_quali.sup[,2])+1)/2, vjust = -sign(df_quali.sup[,3])*0.75+0.25)}
            gg_graph <- gg_graph + text_quali.sup
          }
          else{
            if ((habillage[1] %in% liste.quali) || (colnames(res.pca$call$X))[habillage[1]] %in% liste.quali) {
              gg_graph <- gg_graph +
                geom_point(aes(x = res.pca$quali.sup$coord[levels(res.pca$call$X[,habillage[1]]),axes[1]], y = res.pca$quali.sup$coord[levels(res.pca$call$X[,habillage[1]]),axes[2]]), size = ggoptions_default$size/2.8, color = palette[1:length(levels(res.pca$call$X[,habillage[1]]))], shape = 0)
              if (autoLab) text_quali.sup <- ggrepel::geom_text_repel(aes(x = res.pca$quali.sup$coord[levels(res.pca$call$X[,habillage[1]]),axes[1]], y = res.pca$quali.sup$coord[levels(res.pca$call$X[,habillage[1]]),axes[2]], label=levels(res.pca$call$X[,habillage[1]])), color = palette[1:length(levels(res.pca$call$X[,habillage[1]]))], size = ggoptions_default$size, fontface = "italic")
              else{text_quali.sup <- geom_text(aes(x = res.pca$quali.sup$coord[levels(res.pca$call$X[,habillage[1]]),axes[1]], y = res.pca$quali.sup$coord[levels(res.pca$call$X[,habillage[1]]),axes[2]], label=levels(res.pca$call$X[,habillage[1]])), color = palette[1:length(levels(res.pca$call$X[,habillage[1]]))], size = ggoptions_default$size, fontface = "italic",nudge_y=nudge_y)}
              gg_graph <- gg_graph + text_quali.sup
            }
            if(length(liste.quali) > 1){
              gg_graph <- gg_graph +
                geom_point(aes(x = res.pca$quali.sup$coord[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]]))),axes[1]], y = res.pca$quali.sup$coord[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]]))),axes[2]]), size = ggoptions_default$size/2.8, color = col.quali, shape = 0)
              if (autoLab) text_quali.sup <- ggrepel::geom_text_repel(aes(x = res.pca$quali.sup$coord[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]]))),axes[1]], y = res.pca$quali.sup$coord[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]]))),axes[2]], label = rownames(res.pca$quali.sup$coord)[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]])))]), color = col.quali, size = ggoptions_default$size, fontface = "italic")
              else{text_quali.sup <- geom_text(aes(x = res.pca$quali.sup$coord[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]]))),axes[1]], y = res.pca$quali.sup$coord[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]]))),axes[2]], label = rownames(res.pca$quali.sup$coord)[which(!(rownames(res.pca$quali.sup$coord) %in% levels(res.pca$call$X[,habillage[1]])))]), color = col.quali, size = ggoptions_default$size, fontface = "italic",nudge_y=nudge_y)}
              gg_graph <- gg_graph + text_quali.sup
            }
          }
        }
        else{
          gg_graph <- gg_graph +
            geom_point(aes(x = df_quali.sup[,2], y = df_quali.sup[,3]), size = ggoptions_default$size/2.8, color = col.quali, shape = 0)          
        }
      }
    }
    if (graph.type == "classic"){
      if (any(labe!="")){
        if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
        if (autoLab ==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],shadotext=shadowtext,...)
        if (autoLab ==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""],  font=fonte[labe!=""],pos=3,...)
      }}
    if (!is.null(ellipse)) {
      nbre.ellipse <- nlevels(coord.ellipse[, 1])
      for (e in 1:nbre.ellipse) {
        data.elli <- coord.ellipse[ellipse$res[, 1] == levels(coord.ellipse[, 1])[e], -1]
        if(graph.type=="classic"){
          if ((habillage[1] != "none")&(habillage[1] != "ind")) lines(x=data.elli[, 1], y = data.elli[, 2], col = color.mod[e],...)
          else lines(x=data.elli[, 1], y = data.elli[, 2], col = col.quali,...)}
        else{
          if(graph.type=="ggplot"){
            if (habillage[1] != "none"){
              gg_graph <- gg_graph + geom_path(aes_string(x=data.elli[,1],y=data.elli[,2]), color = color.mod[e])
            }
            else {
              gg_graph <- gg_graph + geom_path(aes_string(x=data.elli[,1],y=data.elli[,2]), color = col.quali)
            }
          }
        }
      }
    }
    
    
    
    #    if ((habillage != "none")&(habillage != "ind")) legend("topleft",legend= levels(res.pca$call$X[,habillage]),text.col= color.mod,cex=par("cex")*0.8)
    if ((habillage[1] != "none") & (habillage[1] != "ind") & (habillage[1] != "cos2") & (habillage[1] != "contrib") & (graph.type == "classic")) {
      L <- list(x="topleft", legend = levels(res.pca$call$X[, habillage[1]]), text.col = color.mod, cex = par("cex") * 0.8)
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
    if (is.null(title)) titre <- "PCA graph of variables"
    else titre <- title
    selection <- selectionS <- NULL
    if (!is.null(select)) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.pca$var$coord)%in%select)+sum(rownames(res.pca$quanti.sup$coord)%in%select)!=0) selection <- which(rownames(res.pca$var$coord)%in%select)
        else {
          if (grepl("contrib",select[1])) selection <- (rev(order(res.pca$var$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$var$contrib[,axes[2]]*res.pca$eig[axes[2],1])))[1:min(nrow(res.pca$var$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("coord",select[1])) selection <- (rev(order(apply(res.pca$var$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$var$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select[1])) {
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
          if (grepl("contrib",select[1])) selectionS <- NULL
          if (grepl("coord",select[1])) selectionS <- (rev(order(apply(res.pca$quanti.sup$coord[,axes]^2,1,sum))))[1:min(nrow(res.pca$quanti.sup$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select[1])) {
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
    
    # cercle variables factor map
    if (graph.type == "classic") {
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
    }
    
    #
    coll <- coo <- labe <- posi <- NULL
    if (!is.null(coord.var[ which(apply(res.pca$var$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),])&is.na(test.invisible[1])&(nrow(coord.var)>0)){
      coord.var <- coord.var[ which(apply(res.pca$var$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),,drop=FALSE]
      coo <- coord.var
      if (length(col.var)==1) coll <- c(coll,rep(col.var,nrow(coord.var)))
	  else coll <- col.var
      if (!is.null(col.hab)) coll <- col.hab[which(colnames(res.pca$call$X)%in%rownames(res.pca$var$coord))]
      if (lab.var){ labe <- c(labe,rownames(coord.var))
      } else  labe <- c(labe,rep("",nrow(coord.var)))
      if (!is.null(selection)){
        if (is.numeric(unselect)) coll[!((1:length(coll))%in%selection)] = rgb(t(col2rgb(coll[!((1:length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255) 
        else coll[!((1:length(coll))%in%selection)] = unselect
        labe[!((1:length(coll))%in%selection)] <- ""
      }
      if(graph.type == "ggplot"){
        df_var <- df_quanti.sup <- NULL
        df_var <- data.frame(labe,coord.var,coll)
	  } else {
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
	}
    #
    if (!is.null(coord.quanti)){
      if (!is.null(coord.quanti[ which(apply(res.pca$quanti.sup$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),])& is.na(test.invisible[2]) & (nrow(coord.quanti)>0)) {
        coord.quanti <- coord.quanti[ which(apply(res.pca$quanti.sup$cos2[, axes,drop=FALSE],1,sum, na.rm = TRUE) >= lim.cos2.var),,drop=FALSE]
        coo <- rbind(coo,coord.quanti)
        if (length(col.quanti.sup)==1) col.quanti.sup<-rep(col.quanti.sup, nrow(coord.quanti))
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
        if (graph.type == "ggplot") df_quanti.sup <- data.frame(labe2,coord.quanti,coll2)
        
        #
        if (graph.type == "classic"){
          for (q in 1:nrow(coord.quanti)) {
            arrows(0, 0, coord.quanti[q, 1], coord.quanti[q, 2], length = 0.1, angle = 15, code = 2, lty = 2, col=coll2[q])
            #
            
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
          }}
        labe <- c(labe,labe2)
        coll <- c(coll,coll2)
      }  
    }
    if (graph.type == "classic"){
      if (any(labe!="")){
        if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
        if (autoLab==FALSE) text(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], pos = posi[labe!=""], col = coll[labe!=""],...)
        if (autoLab==TRUE) autoLab(coo[labe!="", 1], y = coo[labe!="", 2], labels = labe[labe!=""], col = coll[labe!=""], shadotext=shadowtext,...)
      }
	} else {
      if((!is.na(test.invisible[1])) & (habillage[1] != "none") & (is.null(legend["title"][[1]]))){
        legend["title"][[1]] = habillage[1]}
      if (autoLab=="auto") autoLab = (length(which(labe!=""))<50)
      if (length(habillage) != 1){
        warning("Habillage must be length 1")
        habillage <- habillage[1]
      }
      if (!(habillage[1] %in% c("contrib","cos2","none"))){
        warning("Habillage must be in c('contrib','cos2','none')")
        habillage <- "none"
      }
      circle <- annotate("path",
                         x=0+1*cos(seq(0,2*pi,length.out=100)),
                         y=0+1*sin(seq(0,2*pi,length.out=100)),
                         lty = ggoptions_default$circle.lty,
                         lwd = ggoptions_default$circle.lwd,
                         color = ggoptions_default$circle.color)
      transparency_var <- ifelse(rownames(res.pca$var$coord) %in% labe, 1, 1-unselect)
          gg_graph <- ggplot() + 
            coord_fixed(ratio = 1) + 
            geom_line(aes(x=x, y=y), data=data.frame(x=-1:1,y=0),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
            geom_line(aes(x=x, y=y), data=data.frame(x=0,y=-1:1),lty=ggoptions_default$line.lty, lwd = ggoptions_default$line.lwd, color=ggoptions_default$line.color) + 
            theme_light()
			
      if (is.na(test.invisible[1])){
        if (((habillage[1] != "contrib") & (habillage[1] != "cos2"))) {
          gg_graph <- gg_graph + 
            aes(x=df_var[,2], y=df_var[,3]) +
            geom_segment(aes(x=0,y=0,xend=df_var[,2], yend=df_var[,3]),arrow=arrow(length=unit(0.2,"cm")),alpha = transparency_var, lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd, color=col.var)
          if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1]), size = ggoptions_default$size, color = col.var)
          else{text <- geom_text(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1]), size = ggoptions_default$size, color = col.var, hjust = (-sign(df_var[,2])+1)/2, vjust = -sign(df_var[,3])*0.75+0.25)}
        }
        if (habillage[1] == "cos2" || habillage[1] == "contrib"){
		  if (habillage[1] == "cos2") df_var[,4] <- res.pca$var$cos2[,axes[1]] + res.pca$var$cos2[,axes[2]]
		  if (habillage[1] == "contrib") df_var[,4] <- (res.pca$var$contrib[,axes[1]]*res.pca$eig[axes[1],1]+res.pca$var$contrib[,axes[2]]*res.pca$eig[axes[2],1])/(res.pca$eig[axes[1],1]+res.pca$eig[axes[2],1])
          gg_graph <- gg_graph + 
            aes(x=df_var[,2], y=df_var[,3],color = df_var[,4]) +
            geom_segment(aes(x=0,y=0,xend=df_var[,2], yend=df_var[,3],col = df_var[,4]),arrow=arrow(length=unit(0.2,"cm")), alpha = transparency_var, lty = ggoptions_default$segment.lty, lwd = ggoptions_default$segment.lwd) + 
            scale_color_gradient(low=ggoptions_default$low.col.quanti, high=ggoptions_default$high.col.quanti)
          if (habillage[1] == "cos2") gg_graph <- gg_graph + labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "cos2"))
          if (habillage[1] == "contrib") gg_graph <- gg_graph + labs(color = ifelse(legend["title"] %in% legend, legend["title"][[1]], "Ctr"))
          if(autoLab) text <- ggrepel::geom_text_repel(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1], color = df_var[,4]), size = ggoptions_default$size)
          else{text <- geom_text(aes(x=df_var[,2], y=df_var[,3],label=df_var[,1], color = df_var[,4]), size = ggoptions_default$size, hjust = (-sign(df_var[,2])+1)/2, vjust = -sign(df_var[,3])*0.75+0.25)}
        }
      }
      if (!is.na(test.invisible[1])){
          gg_graph <- gg_graph + 
          labs(color = legend["title"][[1]])
      }
      if ((!is.null(res.pca$quanti.sup))&(is.na(test.invisible[2]))){
        transparency_quanti <- ifelse(rownames(res.pca$quanti.sup$coord) %in% labe, 1, 1-unselect)
        
        if (isTRUE(lab.quanti)){
          if(habillage[1] == "contrib") text_quanti.sup <- NULL
          if (habillage[1] == "none"){ gg_graph <- gg_graph + geom_segment(aes(x=0,y=0,xend=df_quanti.sup[,2], yend=df_quanti.sup[,3]),arrow=arrow(length=unit(0.2,"cm")),lty = 2, color = col.quanti.sup,alpha = transparency_quanti)
          if (autoLab) text_quanti.sup <- ggrepel::geom_text_repel(aes(x = df_quanti.sup[,2], y = df_quanti.sup[,3], label=df_quanti.sup[,1]), color = col.quanti.sup, size = ggoptions_default$size,alpha = transparency_quanti)
          else{text_quanti.sup <- geom_text(aes(x = df_quanti.sup[,2], y = df_quanti.sup[,3], label=df_quanti.sup[,1]), color = col.quanti.sup, size = ggoptions_default$size,hjust = (-sign(df_quanti.sup[,2])+1)/2, vjust = -sign(df_quanti.sup[,3])*0.75+0.25,alpha = transparency_quanti)}
          }
          if (habillage[1] == "cos2"){gg_graph <- gg_graph + geom_segment(aes(x=0,y=0,xend=df_quanti.sup[,2], yend=df_quanti.sup[,3], color = res.pca$quanti.sup$cos2[,axes[1]] + res.pca$quanti.sup$cos2[,axes[2]]),arrow=arrow(length=unit(0.2,"cm")),lty = 2,alpha = transparency_quanti)
          if (autoLab) text_quanti.sup <- ggrepel::geom_text_repel(aes(x = df_quanti.sup[,2], y = df_quanti.sup[,3], label=df_quanti.sup[,1], color = res.pca$quanti.sup$cos2[,axes[1]] + res.pca$quanti.sup$cos2[,axes[2]]), size = ggoptions_default$size,alpha = transparency_quanti)
          else{text_quanti.sup <- geom_text(aes(x = df_quanti.sup[,2], y = df_quanti.sup[,3], label=df_quanti.sup[,1], color = res.pca$quanti.sup$cos2[,axes[1]] + res.pca$quanti.sup$cos2[,axes[2]]), size = ggoptions_default$size,hjust = (-sign(df_quanti.sup[,2])+1)/2, vjust = -sign(df_quanti.sup[,3])*0.75+0.25,alpha = transparency_quanti)}
          }
          
          gg_graph <- gg_graph + text_quanti.sup
        } else{
          gg_graph <- gg_graph + 
            geom_segment(aes(x=0,y=0,xend=df_quanti.sup[,2], yend=df_quanti.sup[,3]),arrow=arrow(length=unit(0.2,"cm")),lty = 2, color = col.quanti.sup)
        }
      }
      gg_graph <- gg_graph + theme + circle + labs(title = titre, x = lab.x, y= lab.y) 
      if (is.na(test.invisible[1]) & (isTRUE(lab.var))) gg_graph <- gg_graph + text 
    }
  }
  if(graph.type == "ggplot") return(gg_graph)
}