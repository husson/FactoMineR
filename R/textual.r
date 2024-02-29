textual <- function (tab, num.text, contingence.by=1:ncol(tab), maj.in.min = TRUE, sep.word=NULL) {


cont.textuel <- function(exp, maj.in.min = TRUE, accent = TRUE, sep.word=NULL){
  mots <- list()
  expression <- list()
  if (is.null(sep.word)) sep.word <- "; (),?./:'!=+\n;{}-"
  new.sep.word <- substr(sep.word,1,1)
  for (j in 1:(nchar(sep.word)-1)) new.sep.word <- paste(new.sep.word,substr(sep.word,1,1),sep="")
  sep1 <- substr(new.sep.word,1,1)
  for (i in 1:length(exp)){
    expression[[i]] <- chartr(sep.word,new.sep.word,exp[[i]])
    if (maj.in.min) expression[[i]] <- chartr("A-Z","a-z",expression[[i]])
    stopnow <- FALSE
    aux.length <- -1
    while (nchar(expression[[i]]) != aux.length){
      aux.length <- nchar(expression[[i]])
      expression[[i]] <- gsub(paste(sep1,sep1,sep=""), sep1, expression[[i]]) 
    }
    if (substr(expression[[i]],1,1) == sep1) expression[[i]] <- substr(expression[[i]],2,nchar(expression[[i]]))
    expression[[i]] <- strsplit(expression[[i]],sep1)
  }
  mots.totaux <- as.factor(unlist(expression))
  for (i in 1:length(expression)) mots[[i]] <- c(levels(mots.totaux),expression[[i]][[1]])
  nbmots <- length(levels(mots.totaux))
  table <- as.data.frame(summary(mots.totaux,maxsum=nbmots))
  row.names(table) <-  levels(mots.totaux)
  for (i in 1:length(expression)) table <- cbind(table,summary(as.factor(mots[[i]]),maxsum=nbmots)-1)
  table <- cbind.data.frame(table, apply(matrix(as.integer(table[,-1]>0),nrow=length(levels(mots.totaux))),1,sum))
  colnames(table)[1] <- "words"
  if (!is.null(names(exp))) colnames(table)[2:(length(exp)+1)] <- names(exp)
  if (is.null(names(exp))) colnames(table)[2:(length(exp)+1)] <- paste("exp",1:length(exp),sep=".")
  colnames(table)[ncol(table)] <- "nb.list"
  row.names(table) <-  levels(mots.totaux)
  res <- list(nb.words = table[rev(order(table[,1])),c(1,ncol(table))], contingence.table = table[,-c(1,ncol(table))])
  return(res)
}

  tab <- as.data.frame(tab)
  if (is.null(rownames(tab))) rownames(tab) <- 1:nrow(tab)
  comp <- as.list(tab[,num.text])
  names(comp) <- rownames(tab)
##  res.cont = cont.textuel(comp, maj.in.min = maj.in.min, accent = accent, sep.word=sep.word)
  res.cont <- cont.textuel(comp, maj.in.min = maj.in.min, sep.word=sep.word)
  aux <- t(res.cont$contingence.table)
  don <- cbind.data.frame(tab[,-num.text],aux)
  for (j in 1:length(contingence.by)){
    if (length(contingence.by[[j]])==1) {
      if (contingence.by[[j]]==num.text) don.mean <- apply(don[,ncol(tab):ncol(don)],2,function(x,fac) tapply(x,fac,sum),fac=as.factor(rownames(tab)))
      else don.mean <- apply(don[,ncol(tab):ncol(don)],2,function(x,fac) tapply(x,fac,sum),fac=tab[,contingence.by[[j]]])
      if (j==1) don.comp <- don.mean
      else {
        colnames(don.mean) <- colnames(don.comp)
        don.comp <- rbind.data.frame(don.comp,don.mean)
      }
    }
    else {
      don.mean <- apply(don[,ncol(tab):ncol(don)],2,function(x,fac1,fac2) tapply(x,paste(fac1,fac2,sep="."),sum),fac1=tab[,contingence.by[[j]][1]],fac2=tab[,contingence.by[[j]][2]])
      if (j==1) don.comp <- don.mean
      else {
        colnames(don.mean) <- colnames(don.comp)
        don.comp <- rbind.data.frame(don.comp,don.mean)
      }
    }
  }
  res <- list(cont.table = don.comp, nb.words = res.cont$nb.words)
  return (res)
}
