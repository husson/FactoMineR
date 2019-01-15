RegBest = function(y,x, int = TRUE, wt=NULL, na.action = na.omit,method=c("r2","Cp", "adjr2"), nbest=1){
  if (!is.numeric(y)) stop("The variable  y must be continuous !!!")
  for (i in 1:ncol(x)){
    if (!is.numeric(x[,i])) stop("All the variables must be continuous !!!")
  }
  if (is.null(wt)) wt = rep(1,nrow(x))
  if (is.null(colnames(x))) colnames(x) = paste("v",1:ncol(x),sep="")
  colnames(x) = chartr(" ",".",colnames(x))
  method <- method[1]
  aa = leaps::leaps(x=x, y=y, wt=wt, int=int, method=method, nbest=nbest, names=colnames(x))
  result = vector(mode = "list", length = nrow(aa$which))
  best.p = 1
  mat = matrix(NA,nrow(aa$which),2)
  colnames(mat) <- c("R2","Pvalue")
  rownames(mat) <- paste("Model with",1:nrow(aa$which),"variables")
  rownames(mat)[1] <- paste("Model with",1,"variable")
  for (i in 1:nrow(aa$which)){
    don = cbind.data.frame(y,x[,aa$which[i,]])
    if (i==1) colnames(don) = c("y",colnames(x)[which.max(as.integer(aa$which[i,]))])
    if (int) formul = paste("y~",colnames(don)[2],sep="")
    else formul = paste("y~ -1+",colnames(don)[2],sep="")
    if (ncol(don)>2) for (j in 3:ncol(don)) formul = paste(formul,colnames(don)[j],sep="+")
    resu = summary(lm(as.formula(as.character(formul)),data=don))

   resu$pvalue =  pf(resu$fstatistic[1],resu$fstatistic[2],resu$fstatistic[3],lower.tail=F)
   if (method=="r2"){
    if (resu$pvalue<best.p) {
      best.p = resu$pvalue
      best.i = i
    }
   }
   mat[i,1] <- resu$r.squared
   mat[i,2] <- resu$pvalue
   result[[i]] = resu
  } 
  if (method=="Cp") best.i=which.min(aa$Cp)
  if (method=="adjr2") best.i=which.max(aa$adjr2)
  resultat = list()
  resultat$all = result
  resultat$summary = mat
  resultat$best = result[[best.i]]
  return(resultat)
}
