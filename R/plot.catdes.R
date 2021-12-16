plot.catdes <- function(x,show= "all",output=c("figure","dt"),level=0.01,sort=NULL,col.upper="indianred2",col.lower="royalblue1",numchar=10,barplot=FALSE,cex.names=1,...){
  #Check the arguments
  if( !attr(x,"class")[1]=="catdes") {stop("'x' must be of type catdes")}
  if (show!="all" && show!="quanti" && show!="quali"&& show!="quanti.var"&& show!="test.chi2"){stop("Invalid value of show")}
  output <- match.arg(output[1],c("figure","dt"))
output <- tolower(output[1])
if (output=="dt") {
  mini <- 1
  if (show=="quanti" || show=="all"){
    if (is.null(x$quanti) & (show=="quanti")) stop("No quantitative variables")
    else {
      mini <- min(x$quanti.var[,2])
      lvl <- length(x$quanti)
      rows <- rownames(x$quanti[[1]])
      for (i in 2:lvl) rows <- c(rows,rownames(x$quanti[[i]]))
	  rows <- unique(rows)
	  tabvtest <- matrix(0,nrow=length(rows),ncol=lvl)
	  tabpvalue <- matrix(Inf,nrow=length(rows),ncol=lvl)
	  rownames(tabvtest) <- rownames(tabpvalue) <- sort(rows)
	  colnames(tabvtest) <- colnames(tabpvalue) <- names(x$quanti)
	  for (i in 1:lvl){
	    tabvtest[rownames(x$quanti[[i]]),i] <- x$quanti[[i]][,"v.test", drop = FALSE]
	    tabpvalue[rownames(x$quanti[[i]]),i] <- x$quanti[[i]][,"p.value", drop = FALSE]
	  }
	  tableau_vtest <- NULL
      if (level >= min(tabpvalue)) tableau_vtest <- signif(t(as.matrix(tabvtest[apply(tabpvalue,1,min) <= level,])),3)
    }
  }
  
  if (show=="quali" || show=="all"){
    if (is.null(x$category) & (show=="quali")) stop("No qualitative variables")
    else {
      lvl <- length(x$category)
      rows <- rownames(x$category[[1]])
      for (i in 2:lvl) rows <- c(rows,rownames(x$category[[i]]))
	  rows <- unique(rows)
	  tabvtest <- matrix(0,nrow=length(rows),ncol=lvl)
	  tabpvalue <- matrix(Inf,nrow=length(rows),ncol=lvl)
	  rownames(tabvtest) <- rownames(tabpvalue) <- sort(rows)
	  colnames(tabvtest) <- colnames(tabpvalue) <- names(x$category)
	  for (i in 1:lvl){
	    tabvtest[rownames(x$category[[i]]),i] <- x$category[[i]][,"v.test", drop = FALSE]
	    tabpvalue[rownames(x$category[[i]]),i] <- x$category[[i]][,"p.value", drop = FALSE]
	  }
      mini <- min(mini,min(tabpvalue))
      colnames(tabpvalue) <- colnames(tabvtest) <- names(x$category)
      tableau_quali <- NULL
	  if (level >= min(tabpvalue)) tableau_quali <- signif(tabvtest[apply(tabpvalue,1,min) <= level,,drop = FALSE],3)
    }
  }
    
  if (show=="quanti"){
    if (is.null(tableau_vtest)) stop(paste("The p-value should be greater than",signif(mini,3),"."))
        lvl <- length(x$category)
        quant <- seq(-max(abs(tableau_vtest), na.rm = T), max(abs(tableau_vtest), na.rm = T),length.out = 100)
        color <- grDevices::colorRampPalette(c(col.lower,"white",col.upper))(length(quant)+1)     
        a <- DT::formatStyle(
          DT::datatable(t(as.matrix(tableau_vtest)),
                        extensions = c('Buttons','FixedColumns','FixedHeader'),
                        options = list( pageLength = ncol(tableau_vtest),
                            dom = 'Bfrtip', buttons = c('csv'), fixedColumns = TRUE, fixedHeader = TRUE)
          ),
          rownames(tableau_vtest),
          backgroundColor = DT::styleInterval(quant, color)
        )
        return(a)
  }
  
  if (show=="quali"){
        if (is.null(tableau_quali)) stop(paste("The p-value should be greater than",signif(mini,3),"."))
        quant <- seq(-max(abs(tableau_quali),na.rm = TRUE), max(abs(tableau_quali), na.rm = TRUE), length.out = 100)
        # quant <- seq(min(tableau_quali,na.rm = TRUE), max(tableau_quali, na.rm = TRUE), length.out = 100)
        color <- grDevices::colorRampPalette(c(col.lower,"white",col.upper))(length(quant)+1)
        a <- DT::formatStyle(
          DT::datatable(tableau_quali, extensions = c('Buttons','FixedColumns','FixedHeader'),
            options = list(pageLength = nrow(tableau_quali), dom = 'Bfrtip',
              buttons = c('csv'), fixedColumns = TRUE, fixedHeader = TRUE)
          ),
          colnames(tableau_quali), backgroundColor = DT::styleInterval(quant, color)
        )  
        return(a)
      }
  
  if (show=="all"){
    tab <- NULL
    if(!is.null(tableau_quali)) tab <- rbind(tab,tableau_quali)
    if(!is.null(tableau_vtest)) tab <- rbind(tab,t(tableau_vtest))
    if (is.null(tab)) stop(paste("The p-value should be greater than",signif(mini,3),"."))
    quant <- seq(-max(abs(tab), na.rm = T), max(abs(tab), na.rm = T), length.out = 100)
    color <- grDevices::colorRampPalette(c(col.lower,"white",col.upper))(length(quant)+1)
    a <- DT::formatStyle(
      DT::datatable(tab,extensions = c('Buttons','FixedColumns','FixedHeader'),
        options = list(pageLength = nrow(tab),dom = 'Bfrtip',
          buttons = c('csv'),fixedColumns = TRUE,fixedHeader = TRUE)
      ), colnames(tab),backgroundColor = DT::styleInterval(quant, color)
    )
    return(a)
  }
  
  if (show=="quanti.var"){
      if(level <= min(x$quanti.var[,"P-value"])) stop(paste("The p-value should be greater than",signif(min(x$quanti.var[,"P-value"]),3)))
      tableau_link_quanti <- as.data.frame(x$quanti.var[ x$quanti.var[,"P-value"] <= level,c("Eta2","P-value"), drop = FALSE])
      quant <- seq(min(tableau_link_quanti[,1]), max(tableau_link_quanti[,1]), length.out = 100)
      color <- grDevices::colorRampPalette(c(col.lower,"white",col.upper))(length(quant)+1)
      a <- DT::formatStyle(
        DT::datatable(signif(tableau_link_quanti,3),
                      extensions = c('Buttons','FixedColumns','FixedHeader'),
                      options = list(pageLength = nrow(tableau_link_quanti),
                        dom = 'Bfrtip',buttons = c('csv'),fixedColumns = TRUE,fixedHeader = TRUE)
        ),
        columns = colnames(tableau_link_quanti),
        valueColumns = 'P-value',
        backgroundColor = DT::styleInterval(quant, color)
      )
    return(a)
  }
  
  if (show=="test.chi2"){
    if(is.null(x$test.chi2)) stop("No qualitative variables")
    if(level <= min(x$test.chi2[,"p.value"])) stop(paste("The p-value should be greater than",signif(min(x$test.chi2[,"p.value"]),3)))
    tableau_link_chisquare <- (x$test.chi2[x$test.chi2[,"p.value"] <= level,"p.value", drop = FALSE])
    quant <- seq(min(tableau_link_chisquare[,"p.value"]), max(tableau_link_chisquare[,"p.value"]), length.out = 100)
    color <- grDevices::colorRampPalette(c(col.upper,"white",col.lower))(length(quant)+1)
    a <- DT::formatStyle(
      DT::datatable(
        signif(tableau_link_chisquare,3),
        extensions = c('Buttons','FixedColumns','FixedHeader'),
        options = list(pageLength = nrow(tableau_link_chisquare),
                       dom = 'Bfrtip', buttons = c('csv'), fixedColumns = TRUE, fixedHeader = TRUE)
      ),
      columns = colnames(tableau_link_chisquare),
      valueColumns = "p.value",
      backgroundColor = DT::styleInterval(quant, color)
    )
    return(a)
  }
} else {
if (!barplot){
  if(!is.null(x$quanti)) rows <- names(x$quanti)  
  else  rows <- names(x$category)   
  if(is.null(rows)){stop("Invalid value of x")}
  nb_cluster <- length(rows)
  #If sort is a string, then we convert it to an integer
  if (!is.null(sort)){
    if(is.character(sort)){
        sort <- which(rows==sort)
        if (length(sort)==0) stop("Invalid value of sort")
    } else if (!is.double(sort) || sort!=round(sort) || sort>nb_cluster || sort<1){stop("Invalid value of sort")}
  }
  if(!col.upper %in% colours() || !col.lower %in% colours() ) {stop("Invalid colour name. Please use colours() to see the colours available.")}
  if(level<0){stop("The level must be positive")}
  
  #Setting the colors
  col.upper.rgb <- t(col2rgb(col.upper)/255)
  col.lower.rgb <- t(col2rgb(col.lower)/255)
  
  #Names of the clusters
  #The table that will contain all the p-values
  pval <- c()
  
  ##Columns of the table obtained with the union of the results in the catdes
  #Quantitative
  if(is.null(sort)){
    if (show=="quanti" || show=="all"){
      quanti <- c()
      for (q in x$quanti){
        quanti <- union(quanti,rownames(q))
        pval <- c(pval,q[,'p.value'])
      }
    }
    #Categorical
    if (show=="quali" || show=="all"){
      quali <- c()
      for (q in x$category){
        quali <- union(quali,rownames(q))
        pval <- c(pval,q[,'p.value'])
      }
    }
    #All columns
    if (show=="all"){
      columns <- c(quanti,quali)
    }
    else if (show=="quali"){
      columns <- quali
    }
    else if (show=="quanti"){
      columns <- quanti
    }
  } else{
    quanti_all <- c()
    pval <- c() #all the p-values
    for (q in x$quanti){
      quanti_all <- union(quanti_all,rownames(q))
      pval <- c(pval,q[,'p.value'])
    }
    #Categorical
    quali_all <- c()
    for (q in x$category){
      quali_all <- union(quali_all,rownames(q))
      pval <- c(pval,q[,'p.value'])
    }
    k <- sort
    if ((show=="quanti" || show=="all") && !(is.null(x$quanti))){
    pvals_k_quanti <- as.data.frame(x$quanti[[k]][,"p.value"])
    colnames(pvals_k_quanti)<-"pvals"
    names <- rownames(pvals_k_quanti)
    rownames(pvals_k_quanti) <- NULL
    pvals_k_quanti <- cbind(names,pvals_k_quanti)
    quanti_all <- as.data.frame(cbind(quanti_all,rep(0,length(quanti_all))))
    colnames(quanti_all) <- c("names","pvals")
     quanti_all <- merge(x=quanti_all,y=pvals_k_quanti,by="names",all.x=TRUE)[,c("names","pvals.y")]
    colnames(quanti_all) <- c("names","pvals")
    }
    if ((show=="quali" || show=="all") && !(is.null(x$category))){
    pvals_k_quali <- as.data.frame(x$category[[k]][,"p.value"])
    colnames(pvals_k_quali)<-"pvals"
    names <- rownames(pvals_k_quali)
    rownames(pvals_k_quali) <- NULL
    pvals_k_quali <- cbind(names,pvals_k_quali)
    quali_all <- as.data.frame(cbind(quali_all,rep(0,length(quali_all))))
    colnames(quali_all) <- c("names","pvals")
     quali_all <- merge(x=quali_all,y=pvals_k_quali,by="names",all.x=TRUE)[,c("names","pvals.y")]
    colnames(quali_all) <- c("names","pvals")
    }
    if (show=="all") columns <- rbind(quali_all,quanti_all)
    if (show=="quali") columns <- rbind(quali_all)
    if (show=="quanti") columns <- rbind(quanti_all)
    columns <- columns[order(columns$pvals),]
    columns <- columns$names
  }
  
  #Create the grid
  dim1 <- length(rows)
  dim2 <- length(columns)
  plot.new()
  for (i in 1:dim1){ 
    ybottom <- 1 - i * (1/(dim1 + 1))
    xright <- 1/(dim2 + 1)
    ytop <- 1 - (i + 1) * (1/(dim1 + 1))
    xleft <- 0
      center <- c(mean(c(xleft,xright)),mean(c(ytop,ybottom)))
      text(center[1],center[2],rows[i],cex=0.5*cex.names,font=2)
      for(j in 1:(dim2)){
        xleft <- j * (1/(dim2 + 1))
        ybottom <- 1 - i * (1/(dim1 + 1))
        xright <- (j + 1) * (1/(dim2 + 1))
        ytop <- 1 - (i + 1) * (1/(dim1 + 1))
        rect(xleft, ybottom = ybottom, xright, ytop)
    }
  }
  for (j in 1:(dim2)){ 
    xleft <- j * (1/(dim2 + 1))
    ybottom <- 1
    xright <- (j + 1) * (1/(dim2 + 1))
    ytop <- 1 - (1/(dim1 + 1))
    center <- c(mean(c(xleft,xright)),mean(c(ytop,ybottom)))
    text(center[1],1 - (1/(dim1 + 1)),columns[j],cex=0.4*cex.names,srt=45,adj=c(0,0))
  }
  #For the gradients of color
  max <- level 
  min <- min(pval)
  range <- max-min
  
  #Completing the grid
  #For each cluster
  for (i in 1:dim1){
    quanti <- x$quanti
    quali <- x$category
    #Quantitative
    if (show=="quanti" || show=="all"){
      for (name_row in rownames(quanti[[i]])){
        j <- which(columns==name_row)
        p <- quanti[[i]][name_row,"p.value"]
        if (p<level){
          col <- pmin(1,pmax(0,0.9*(p-min)/(min-max)+1))
          xleft <- j * (1/(dim2 + 1))
          ybottom <- 1 - i * (1/(dim1 + 1))
          xright <- (j + 1) * (1/(dim2 + 1))
          ytop <- 1 - (i + 1) * (1/(dim1 + 1))
          if(quanti[[i]][name_row,"v.test"]>=0){
            rect(xleft, ybottom = ybottom, xright, ytop, col = rgb(col.upper.rgb,alpha=col), border = NULL)
          } else {
            rect(xleft, ybottom = ybottom, xright, ytop, col = rgb(col.lower.rgb,alpha=col), border = NULL)
          }
        }
      }
    }
    
    #Qualitative
    if (show=="quali" || show=="all"){
      for (name_row in rownames(quali[[i]])){
        j <- which(columns==name_row)
        p <- quali[[i]][name_row,"p.value"]
        if (p<level){
          col <- pmin(1,pmax(0,0.9*(p-min)/(min-max)+1))
          xleft <- j * (1/(dim2 + 1))
          ybottom <- 1 - i * (1/(dim1 + 1))
          xright <- (j + 1) * (1/(dim2 + 1))
          ytop <- 1 - (i + 1) * (1/(dim1 + 1))
          if(quali[[i]][name_row,"v.test"]>=0){
            rect(xleft, ybottom = ybottom, xright, ytop, col = rgb(col.upper.rgb,alpha=col), border = NULL)
          } else {
            rect(xleft, ybottom = ybottom, xright, ytop, col = rgb(col.lower.rgb,alpha=col), border = NULL)
          }
        }
      }
    }
  }
} else {    ### begin program for barplot=TRUE
  lengthX <- max(length(x$quanti),length(x$category))		# measure the length of x
  long <- rep(0,lengthX)
                                        #print(x$category)
  list.catdes <- list(long)
  minimum <- 0
  maximum <- 0
  count <- 0
  for (i in 1:lengthX){
    if (!is.null(x$quanti[[i]])){
      quanti <- as.data.frame(x$quanti[[i]])
      quanti.catdes <- as.vector(quanti[,1])
      names(quanti.catdes) <- rownames(quanti)
    } else quanti.catdes <- NULL
    if (!is.null(x$category[[i]])){
      category <- as.data.frame(x$category[[i]])
      category.catdes <- as.vector(category[,5])
      names(category.catdes) <- rownames(category)
    } else category.catdes <- NULL
                                        #print(category.catdes)
    if (show=="all") catdes.aux <- c(quanti.catdes,category.catdes)
    if (show=="quanti") catdes.aux <- quanti.catdes					# different options
    if (show=="quali") catdes.aux <- category.catdes
    if (!is.null(catdes.aux)) {
      count <- count+1								#count is a counter of the catdes clusters which are non null.   
      long[i] <- length(catdes.aux)
      minimum <- min(catdes.aux,minimum)			# find the longest catdes clusters and the smallest.
      maximum <- max(catdes.aux,maximum)
    } else long[i] <- 0
    list.catdes[[i]] <- catdes.aux								# list the catdes clusters
  }
  if(count!=0){
    if (count<=4){ 
      numc <- count									# design of the graphic window
      numr <- 1
    } else{
      numc <- 4
      numr <- count%/%4+1
    }
    plot.new()
    par(las = 3)
    par(mfrow = c(numr, numc))
    for(i in 1:lengthX){
      catdes.aux <- list.catdes[[i]]
      if(!is.null(catdes.aux)){
        catdes.aux <- sort(catdes.aux,decreasing=FALSE)					#plot the catdes for every cluster in the graphic window
        coul <- rep(col.upper,length(catdes.aux))
        coul[catdes.aux<0] <- col.lower
		if (is.null(x$category)) titre <- names(x$quanti)[i]
		else titre <- names(x$category)[i]
        barplot(catdes.aux, width =c(1,1), col = coul, border = "black", 
                ylim = c(minimum-1,maximum+1),xlim=c(0,max(long)+1), 
                main = titre, cex.names = cex.names, ylab="v.test", 
                names.arg = substr(names(catdes.aux), 1, numchar))
	
      }
    }
  }    
  par(las = 0)
}
}
}
