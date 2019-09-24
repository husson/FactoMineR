condes <- function (donnee, num.var, weights=NULL, proba = 0.05)
{

cor.calc <- function(y,x,w=NULL){
  if (is.null(w)) w=rep(1,length(x))
  Z <- cbind(x,y)
  missing <- apply(is.na(Z),1,any)
  Z <- Z[!missing,]
  w <- w[!missing]
  n=sum(w)
  if (n<3) n <- sum(w)*length(x)  ### au cas ou les poids somment a 1, on multiplie par n
  r=cov.wt(Z,wt=w,method="ML",cor=TRUE)$cor[1,2]
  return( list(r=r,proba=pt(sqrt(n-2)*sqrt(r^2/(1-r^2)),n-2,lower.tail=FALSE)*2))
}

test.aov.w <- function(y,x,w=NULL){
  if (is.null(w)) w=rep(1,length(x))
  res.aov <- aov(y ~ x, weights=w, na.action = na.exclude)
  res <- summary(res.aov)[[1]]
  ddlR <- sum(w[!apply(is.na(cbind.data.frame(x,y)),1,any)])-nlevels(x)
  tabF <- c(res[1, 2]/(res[1, 2]+res[2,2]), pf((res[1,3])/(res[2,2]/(ddlR)), res[1, 1], ddlR, lower.tail = FALSE))

  Estimate <- summary.lm(res.aov)$coef[-1, 1,drop=FALSE]
  Estimate <- c(Estimate, -sum(Estimate))

  tabX <- tab.disjonctif(x)
  aux <- apply(tabX,2,cor.calc,y,w=w)
  aux <- matrix(as.numeric(sapply(aux,unlist)),byrow=T,ncol=2)
  p.value <- aux[,2]
  resT <- cbind(Estimate,p.value)
  return(list(tabF = tabF, resT = resT))  
}
    donnee <- as.data.frame(donnee)
	is.quali <- which(!unlist(lapply(donnee,is.numeric)))
    donnee[,is.quali] <- lapply(donnee[,is.quali,drop=FALSE],as.factor)
    donnee <- droplevels(donnee)
	lab.sauv <- lab <- colnames(donnee)
    quali = NULL
	if (is.null(weights)) weights <- rep(1,nrow(donnee))
	if (sum(weights)<3) weights <- weights*nrow(donnee)
    for (i in 1:length(lab)) {
        if (is.factor(donnee[, i])) {
            if (any(is.na(donnee[, i]))) {
                levels(donnee[, i]) <- c(levels(donnee[, i]),"NA")
                donnee[, i][is.na(donnee[, i])] <- "NA"
            }
            if (levels(donnee[, i])[1] == "") levels(donnee[, i])[1] = "NA"
            if (i != num.var) {
			  levels(donnee[, i])=paste(colnames(donnee)[i],levels(donnee[, i]),sep="=")
			  quali = c(quali, i)
			}
        }
    }
    quanti = (1:ncol(donnee))[-c(quali, num.var)]
    if (length(quanti) == 0) quanti = NULL
    colnames(donnee) = lab
    result <- list()
    if (!is.null(quanti)) {
        if (length(quanti)>1){
		  tab.quanti=apply(donnee[,quanti],2,cor.calc,donnee[,num.var],w=weights)
          aux = matrix(as.numeric(sapply(tab.quanti,unlist)),byrow=TRUE,ncol=2)
		} else aux <- matrix(unlist(cor.calc(donnee[, quanti], donnee[, num.var],w=weights)),ncol=2)
        rownames(aux) = colnames(donnee)[quanti]
        resQ = NULL
        if (NROW(aux) > 1) aux <- aux[rev(order(aux[, 1])), ]
        resQ <- aux[aux[, 2] <= proba, , drop = FALSE]
        colnames(resQ) = c("correlation", "p.value")
		if (nrow(resQ)==0) resQ=NULL
        result$quanti <- resQ
    }
    if (!is.null(quali)) {
        old.contr = options()$contrasts
        options(contrasts = c("contr.sum", "contr.sum"))
        tabF = matrix(NA, length(quali), 2)
        tabT = matrix(NA, 1, 2)
        indice.tabT = 0
        for (v in 1:length(quali)) {
            resaov <- test.aov.w(donnee[, num.var], donnee[, quali[v]], w=weights)
            tabF[v,] <- resaov$tabF
			resT <- resaov$resT
            rownames(resT) = levels(donnee[, quali[v]])
            tabT = rbind(tabT, resT)
        }
        rownames(tabF) = colnames(donnee)[quali]
        colnames(tabF) = c("R2","p.value")
        tabT = tabT[-1, ]
        resF = resT = NULL
        if (sum(tabF[,2] <= proba) > 0)  resF <- tabF[tabF[,2] <= proba,,drop=FALSE]
        if (!is.null(resF)) resF <- resF[order(resF[,2]),,drop=FALSE]
        tabT <- tabT[rev(order(sign(tabT[, 1])/tabT[, 2])), ]
        if (sum(tabT[, 2] <= proba) >= 1) resT <- tabT[tabT[, 2] <= proba, ,drop=FALSE]
        result$quali = resF
        result$category = resT
        options(contrasts = old.contr)
    }
	if (is.null(quali) & is.null(quanti)) print(paste("The value of proba is too small. Choose a proba greater than",proba))
	result$call <- list(num.var=num.var, proba=proba, weights=weights, X=donnee)
	class(result) <- c("condes", "list ")

    return(result)
}
