descfreq <- function(donnee,by.quali=NULL,proba = 0.05){

  lab.sauv <- lab <- colnames(donnee)
  for (i in 1:length(lab)) lab[i]=gsub(" ",".",lab[i])
  if (!is.null(by.quali)) {
    donnee <- as.data.frame(matrix(unlist(by(donnee,by.quali,apply,2,sum)),ncol=ncol(donnee),byrow=TRUE))
	rownames(donnee) <- levels(by.quali)
  }
  colnames(donnee) = lab

  old.warn = options("warn")
  options(warn = -1)
  marge.li = apply(donnee,1,sum)
  nom = tri = structure(vector(mode = "list", length = nrow(donnee)), names = rownames(donnee))
  marge.col = apply(donnee,2,sum)
  for (j in 1:nrow(donnee)) {
   for (k in 1:ncol(donnee)) {
    aux2 = donnee[j,k]/marge.col[k]
    aux3 = marge.li[j]/sum(marge.li)
    if (aux2 > aux3) aux4 = phyper(donnee[j,k]-1,marge.col[k],sum(marge.col)-marge.col[k],marge.li[j],lower.tail=FALSE)*2
    else aux4 = phyper(donnee[j,k],marge.col[k],sum(marge.col)-marge.col[k],marge.li[j])*2
	if (aux4>1) aux4 <- 2-aux4 ##sinon on peut avoir proba > a 1 
    if (aux4<proba) {
      aux5 = (1-2*as.integer(aux2>aux3))*qnorm(aux4/2)
      aux1 = donnee[j,k]/marge.li[j]
      tri[[j]] = rbind(tri[[j]],c(aux1*100,sum(marge.col[k])/sum(donnee)*100,donnee[j,k],marge.col[k],aux4,aux5))
      nom[[j]] = rbind(nom[[j]],c(colnames(donnee)[k],colnames(donnee)))
    }
   }
  }
  for (j in 1:nrow(donnee)){
    if (!is.null(tri[[j]])){
      oo = rev(order(tri[[j]][,6]))
      tri[[j]] = tri[[j]][oo,]
      nom[[j]] = nom[[j]][oo,]
      if (nrow(matrix(tri[[j]],ncol=6))>1) rownames(tri[[j]]) = nom[[j]][,1]
      else {
        tri[[j]] = matrix(tri[[j]],ncol=6)
        rownames(tri[[j]]) = nom[[j]][1]
      }
      colnames(tri[[j]]) =  c("Intern %","glob %","Intern freq","Glob freq ","p.value","v.test")
    }
  }
  res = tri

  options(old.warn)
class(res) <- c("descfreq", "list ")
  return(res)
}
