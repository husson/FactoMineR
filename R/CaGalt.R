CaGalt<-function(Y,X,type="s",conf.ellip=FALSE,nb.ellip=100,level.ventil=0,sx=NULL,graph=TRUE,axes=c(1,2)){
    if (is.null(attributes(X)$row.names)) rownames(X) <- 1:nrow(X)
    if (is.null(attributes(X)$names)) colnames(X) <- colnames(X, do.NULL = FALSE,prefix="V")
    X <- as.data.frame(X)
    is.quali <- which(!unlist(lapply(X,is.numeric)))
    X[,is.quali] <- lapply(X[,is.quali,drop=FALSE],as.factor)
	X.initial<-X
	P<-as.matrix(Y/sum(Y))
	PI.<-apply(P,1,sum)
	P.J<-apply(P,2,sum)
	mean.p <- function(V,poids) res<-sum(V*poids,na.rm=TRUE)/sum(poids[!is.na(V)])
	sd.p <- function(V,poids) res<-sqrt(sum(V^2*poids,na.rm=TRUE)/sum(poids[!is.na(V)]))
	if (!type%in%c("c","s","n")) stop("not convenient type definition")
	if (is.null(sx)){
		if (type!="n") ncp<-min(nrow(X)-1,ncol(X))
		else ncp<-min(nrow(X)-1,ncol(tab.disjonctif(X))-ncol(X))
	}else{
		if (type!="n") ncp<-min(sx,nrow(X)-1,ncol(X))
		else ncp<-min(sx,nrow(X)-1,ncol(tab.disjonctif(X))-ncol(X))
	}
	if(type!="n"){
		diag.X<-PCA(X,scale.unit=type=="s",ncp=ncp,row.w=PI.,graph=F)
		X<-as.matrix(sweep(X,2,apply(X,2,mean.p,PI.),"-"))
		if (type=="s") X<-sweep(X,2,apply(X,2,sd.p,PI.),"/")
	}else{
		diag.X<-MCA(X,row.w=PI.,ncp=ncp,level.ventil=level.ventil,graph=F)
		if(ncol(X)>1) X<-sweep(tab.disjonctif(X),2,apply(tab.disjonctif(X),2,mean.p,PI.),"-")
		else X<-tab.disjonctif(X)
	}
	phi.stand<-diag.X$svd$U		
	L<-sweep(crossprod(P,phi.stand),1,P.J,"/")
	T<-crossprod(P,X)
	C<-crossprod(sweep(X,1,PI.^(1/2),"*"),sweep(X,1,PI.^(1/2),"*"))
	W<-sweep(crossprod(t(T),ginv(C)),1,P.J,"/")
	colnames(W)<-colnames(T)
	diag.L<-PCA(cbind(L,W),quanti.sup=(ncp+1):(ncp+ncol(T)),scale.unit=FALSE,ncp=ncp,row.w=P.J,graph=F)
	coord.ind<-sweep(crossprod(t(P),diag.L$svd$U),1,PI.,"/")
	cos2.ind<-sweep(coord.ind^2,1,apply(coord.ind^2,1,sum),"/")
	res.ind<-list(coord=coord.ind,cos2=cos2.ind)
	res.freq<-list(coord=diag.L$ind$coord,cos2=diag.L$ind$cos2,contrib=diag.L$ind$contrib)
	res<-list(eig=diag.L$eig,ind=res.ind,freq=res.freq)
	if(type=="n") res$quali.var<-list(coord=diag.L$quanti.sup$coord,cos2=diag.L$quanti.sup$cos2)
	else res$quanti.var<-diag.L$quanti.sup
	PJ<-vector(mode="list",length=nb.ellip)
	for (n in 1:nb.ellip){
		samp<-sample(1:nrow(Y),replace=TRUE)
		while(sum(apply(Y[samp,],2,sum)>0)!=ncol(Y)) samp<-sample(1:nrow(Y),replace=TRUE)
		Y.samp<-Y[samp,]
		P.samp<-as.matrix(Y.samp/sum(Y.samp))
		PI.samp<-apply(P.samp,1,sum)
		P.J.samp<-apply(P.samp,2,sum)
		PJ[[n]]<-P.J.samp
		if(type=="n"){
			X.samp<-sweep(tab.disjonctif(X.initial[samp,]),2,apply(tab.disjonctif(X.initial[samp,]),2,mean.p,PI.samp),"-")
		}else{
			X.samp<-as.matrix(sweep(X.initial[samp,],2,apply(X.initial[samp,],2,mean.p,PI.samp),"-"))
			if (type=="s") X.samp<-sweep(X.samp,2,apply(X.samp,2,sd.p,PI.samp),"/")
		}
		phi.stand.samp<-phi.stand[samp,]
		T.samp<-crossprod(P.samp,X.samp) 
		C.samp<-crossprod(sweep(X.samp,1,PI.samp^(1/2),"*"),sweep(X.samp,1,PI.samp^(1/2),"*"))
		if(n==1){
			L.samp<-sweep(crossprod(P.samp,phi.stand.samp),1,P.J.samp,"/")
			W.samp<-sweep(crossprod(t(T.samp),ginv(C.samp)),1,P.J.samp,"/")
			W.stand.samp<-sweep(sweep(crossprod(t(T.samp),ginv(C.samp)),1,P.J.samp,"/"),2,apply(sweep(crossprod(t(T.samp),ginv(C.samp)),1,P.J.samp,"/"),2,sd.p,P.J.samp),"/")
		}else{
			L.samp<-rbind(L.samp,sweep(crossprod(P.samp,phi.stand.samp),1,P.J.samp,"/"))
			W.samp<-cbind(W.samp,sweep(crossprod(t(T.samp),ginv(C.samp)),1,P.J.samp,"/"))
			W.stand.samp<-rbind(W.stand.samp,sweep(sweep(crossprod(t(T.samp),ginv(C.samp)),1,P.J.samp,"/"),2,apply(sweep(crossprod(t(T.samp),ginv(C.samp)),1,P.J.samp,"/"),2,sd.p,P.J.samp),"/"))
		}
	}
	rownames(L.samp)<-paste(rep(rownames(L),nb.ellip),rep(1:nb.ellip,each=nrow(L)),sep="")
	freq.ellip.coord<-as.data.frame(PCA(rbind(L,L.samp),ncp=ncp,ind.sup=(ncol(Y)+1):((nb.ellip+1)*ncol(Y)),row.w=P.J,scale.unit=FALSE,graph=FALSE)$ind.sup$coord)
	if(type!="n"){
		for (n in 1:nb.ellip){
		 	aux<-as.matrix(freq.ellip.coord[(((n-1)*ncol(Y))+1):(n*ncol(Y)),1:ncp])
			aux.cent<-sweep(aux,2,apply(aux,2,mean.p,PJ[[n]]),"-")
			aux.stand<-as.matrix(sweep(aux.cent,2,apply(aux.cent,2,sd.p,PJ[[n]]),"/"))
			if (n==1) var.ellip.coord<-as.data.frame(crossprod(W.stand.samp[(((n-1)*ncol(Y))+1):(n*ncol(Y)),],sweep(aux.stand,1,PJ[[n]],"*")))
			else var.ellip.coord<-rbind(var.ellip.coord,as.data.frame(crossprod(W.stand.samp[(((n-1)*ncol(Y))+1):(n*ncol(Y)),],sweep(aux.stand,1,PJ[[n]],"*"))))
		}
	}else{
		colnames(W.samp)<-paste(rep(colnames(W),nb.ellip),rep(1:nb.ellip,each=ncol(W)),sep="")
		var.ellip.coord<-as.data.frame(PCA(cbind(L,W.samp),quanti.sup=(ncp+1):(ncp+ncol(W.samp)),scale.unit=F,ncp=ncp,row.w=P.J,graph=F)$quanti.sup$coord)
	}
	freq.ellip.coord$FREQ<-rep(rownames(L),nb.ellip)
	var.ellip.coord$VAR<-rep(colnames(X),nb.ellip)
	res$ellip<-list(freq=freq.ellip.coord,var=var.ellip.coord)
	class(res)<-c("CaGalt","list")		
	if (graph){	
		plot.CaGalt(res,choix="ind",axes=axes)
		if(nrow(res$freq$coord)<50){
			plot.CaGalt(res,choix="freq",axes=axes,conf.ellip=conf.ellip,new.plot=TRUE)
		}else{
			plot.CaGalt(res,choix="freq",axes=axes,conf.ellip=conf.ellip,new.plot=TRUE,select = "contrib 49")
			warning("The first 50 frequencies that have the highest contribution on the 2 dimensions of your plot are drawn.")
		}
		if (type!="n") plot.CaGalt(res,choix="quanti.var",axes=axes,conf.ellip=conf.ellip,new.plot=TRUE)
		else plot.CaGalt(res,choix="quali.var",axes=axes,conf.ellip=conf.ellip,new.plot=TRUE)
	}
	return(res)
}