summary.CA <- function(object,nb.dec=3,nbelements=10,ncp=3,align.names=TRUE,file="",...){

  print2 <- function(mat,file=""){
	if (file=="") print(mat, quote = FALSE, right = TRUE)
	else {
	  mat <- cbind(format(rownames(mat)),mat)
	  mat <- rbind(colnames(mat),mat)
      mat2 <- cbind(format(mat[,1],justify="right"),format(mat[,2],justify="right"))
	  for (k in 3:ncol(mat)) mat2 <- cbind(mat2,format(mat[,k],justify="right"))
	  mat2 <- cbind(mat2,"\n")
	  for (i in 1:nrow(mat2)) cat(mat2[i,],file=file,append=TRUE)
	}
  }

print3 <- function(obj,file="",ncp,width.row=0,nbelements=nbelements){
  list.obj <- match.arg(names(obj),c("dist","inertia","coord","cos2","contrib","v.test","vtest"),several.ok=TRUE)
  nb.col <- sum(c("coord","cos2","contrib","v.test")%in%list.obj)
  nbelements <- min(nbelements,nrow(obj$coord))
  mat <- matrix(NA,nbelements,sum(c("dist","inertia")%in%list.obj)+nb.col*ncp)
  colnames(mat) <- paste("v",1:ncol(mat))
  rownames(mat) <- format(rownames(obj$coord)[1:nbelements,drop=FALSE],width=width.row)
  indice <- 1
  if (sum(c("dist","inertia")%in%list.obj)) {
    if ("dist"%in%list.obj) {
	  if (!is.null(obj$dist)) mat[,indice] <- obj$dist[1:nbelements]
	  colnames(mat)[indice] <- "Dist"
	}
    if ("inertia"%in%list.obj) {
	  if (!is.null(obj$inertia)) mat[,indice] <- obj$inertia[1:nbelements]*1000
	  colnames(mat)[indice] <- "Iner*1000"
	}
	indice <- indice + 1
  }
  if ("coord"%in%list.obj){
    if (!is.null(obj$coord)) mat[,indice+nb.col*(0:(ncp-1))] <- obj$coord[1:nbelements,1:ncp,drop=FALSE]
	colnames(mat)[indice+nb.col*(0:(ncp-1))] <- paste("Dim.",1:ncp,sep="")
    indice <- indice +1
  }
  if ("contrib"%in%list.obj){
    if (!is.null(obj$contrib)) mat[,indice+nb.col*(0:(ncp-1))] <- obj$contrib[1:nbelements,1:ncp,drop=FALSE]
	colnames(mat)[indice+nb.col*(0:(ncp-1))] <- "ctr"
    indice <- indice +1
  }
  if ("cos2"%in%list.obj){
    if (!is.null(obj$cos2)) mat[,indice+nb.col*(0:(ncp-1))] <- obj$cos2[1:nbelements,1:ncp,drop=FALSE]
	colnames(mat)[indice+nb.col*(0:(ncp-1))] <- "cos2"
    indice <- indice +1
  }
  if ("v.test"%in%list.obj){
    if (!is.null(obj$v.test)) mat[,indice+nb.col*(0:(ncp-1))] <- obj$v.test[1:nbelements,1:ncp,drop=FALSE]
	colnames(mat)[indice+nb.col*(0:(ncp-1))] <- "v.test"
    indice <- indice +1
  }
  mat <- format(round(mat,nb.dec))
  if (sum(c("dist","inertia")%in%list.obj)) mat2 <- cbind("|", mat[1:nbelements,1,drop=FALSE],"|")
  else mat2 <- "|"
  for (k in 1:ncp) mat2 <- cbind(mat2, mat[,(1+sum(c("dist","inertia")%in%list.obj)+nb.col*(k-1)):(sum(c("dist","inertia")%in%list.obj)+nb.col*k),drop=FALSE],"|")
  colnames(mat2)[1] <- ""
  print2(as.matrix(mat2),file=file)
}

  res <- object
  if (!inherits(res, "CA")) stop("non convenient object")
  cat(paste("\nCall:\n"),file=file)
  cat(paste(deparse(res$call$call),"\n"),file=file,append=TRUE)
  cat("\n",file=file,append=TRUE)

  IT <- sum(res$eig[, 1] )* sum(res$call$X) 
  df <- (nrow(res$call$X) - 1) * (ncol(res$call$X) - 1)
  pc <- pchisq(IT, df = df,lower.tail = FALSE)
  cat("The chi square of independence between the two variables is equal to", IT, 
      "(p-value = ", pc, ").\n",file=file,append=TRUE)

  cat("\nEigenvalues\n",file=file,append=TRUE)
  eige <- format(t(round(res$eig[,1:3],nb.dec)),justify="right")
  rownames(eige) <- c("Variance","% of var.","Cumulative % of var.")
  colnames(eige) <- paste("Dim",1:ncol(eige),sep=".")
  print2(eige,file=file)

  width.row <- 0
  if (align.names==TRUE){
    aux <- match.arg(names(res),c("ind","ind.sup","freq","freq.sup","var","quanti.var","quanti.sup","quali.var","quali.sup","quanti.sup","quali.sup","group","row","row.sup","col","col.sup"),several.ok = TRUE)
    width.row = max(nchar(rownames(res[aux[1]][[1]]$coord)))
	for (k in 1:length(aux)) width.row = max(width.row,nchar(rownames(res[aux[k]][[1]]$coord)[1:min(nrow(res[aux[k]][[1]]$coord),nbelements)]))
  }
  ncp <- min(res$call$ncp,ncp)
  
   cat("\nRows",file=file,append=TRUE)
   if (nrow(res$row$coord) > nbelements) cat(paste(" (the ",nbelements," first)",sep=""),file=file,append=TRUE)
   cat("\n",file=file,append=TRUE)
  print3(res$row,file=file,ncp=ncp,width.row=width.row,nbelements=nbelements)
  
   cat("\nColumns",file=file,append=TRUE)
   if (nrow(res$col$coord) > nbelements) cat(paste(" (the ",nbelements," first)",sep=""),file=file,append=TRUE)
   cat("\n",file=file,append=TRUE)
  print3(res$col,file=file,ncp=ncp,width.row=width.row,nbelements=nbelements)
  
 if (!is.null(res$row.sup)){
   cat("\nSupplementary row",file=file,append=TRUE)
   if (nrow(res$row.sup$coord)>1) cat("s",file=file,append=TRUE)
   if (nrow(res$row.sup$coord) > nbelements) cat(paste(" (the ",nbelements," first)",sep=""),file=file,append=TRUE)
   cat("\n",file=file,append=TRUE)
  print3(res$row.sup,file=file,ncp=ncp,width.row=width.row,nbelements=nbelements)
 }

  if (!is.null(res$col.sup)){
   cat("\nSupplementary column",file=file,append=TRUE)
   if (nrow(res$col.sup$coord)>1) cat("s",file=file,append=TRUE)
   if (nrow(res$col.sup$coord) > nbelements) cat(paste(" (the ",nbelements," first)",sep=""),file=file,append=TRUE)
   cat("\n",file=file,append=TRUE)
  print3(res$col.sup,file=file,ncp=ncp,width.row=width.row,nbelements=nbelements)
 }

  if (!is.null(res$quanti.sup)){
  cat("\nSupplementary continuous variable",file=file,append=TRUE)
  if (nrow(res$quanti.sup$coord)>1) cat("s",file=file,append=TRUE)
  cat("\n",file=file,append=TRUE)
  print3(res$quanti.sup,file=file,ncp=ncp,width.row=width.row,nbelements=nbelements)
 }
  if (!is.null(res$quali.sup)){
  cat("\nSupplementary categorical variable",file=file,append=TRUE)
  if (nrow(res$quali.sup$coord)>1) cat("s",file=file,append=TRUE)
  cat("\n",file=file,append=TRUE)
  print3(res$quali.sup,file=file,ncp=ncp,width.row=width.row,nbelements=nbelements)
 }
}