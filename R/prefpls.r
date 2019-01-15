"prefpls" <- function(donnee,var1=1,var2=2,firstvar=3,lastvar=ncol(donnee),levels=c(0.2,0.4,0.6,0.7,0.8,0.9,1),asp=1,nbchar=max(nchar(colnames(donnee))),title=NULL,choix="var"){

aux <- as.data.frame(matrix(nrow=lastvar-firstvar+1,ncol=7))
for (i in firstvar:lastvar){
  a <- summary(lm(donnee[,i]~donnee[,var1]+donnee[,var2], na.action=na.omit))$coef[2]
  b <- summary(lm(donnee[,i]~donnee[,var1]+donnee[,var2], na.action=na.omit))$coef[3]
  r2 <- summary(lm(donnee[,i]~donnee[,var1]+donnee[,var2],na.action=na.omit))$r.squared
  cor1 <- cor(donnee[,i],donnee[,var1],use="pairwise.complete.obs")
  cor2 <-  cor(donnee[,i],donnee[,var2],use="pairwise.complete.obs")
  axe2 <- sqrt(r2/(1+a^2/b^2))*(abs(b)/b)
  axe1 <- a*axe2/b
  aux [i,1] <-  colnames(donnee)[i]
  aux [i,2] <- axe1
  aux [i,3] <- axe2
  aux [i,4] <- cor1*sqrt(r2)/sqrt(cor1^2+cor2^2)
  aux [i,5] <-  cor2*sqrt(r2)/sqrt(cor1^2+cor2^2)
  aux [i,6] <- cor1
  aux [i,7] <- cor2
}

if (choix=="ind") {
  dev.new()
  if (is.null(title)) title2 = paste("Biplot for",colnames(donnee)[var1],"and",colnames(donnee)[var2])
  else title2 = title
  plot(donnee[,var1],donnee[,var2],xlab=colnames(donnee)[var1],ylab=colnames(donnee)[var2],pch=20,main=title2,asp=asp)
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  text(donnee[,var1],donnee[,var2],rownames(donnee), pos = 4, offset = 0.2)
}
if (choix=="var"){
  dev.new()
  alph <- acos(cor(donnee[,var1],donnee[,var2]))/pi*180
  zz <- function(x,y,alpha)
  ifelse((y<cos(alpha/180*pi+acos(x)))|(y>cos(alpha/180*pi-acos(x))),NA,
  (x^2+y^2)/sqrt((x+y*cos(alpha/180*pi))^2+y^2*sin(alpha/180*pi)^2))
  x <- y<- seq(-1,1,0.01)
  res <- outer(x,y,zz,alpha=alph)
  if (is.null(title)) title = paste("Prefmap-PLS graph between",colnames(donnee)[var1],"and",colnames(donnee)[var2])
  image(x,y,res,zlim=c(0,1),asp=1,xlab=colnames(donnee)[var1],ylab=colnames(donnee)[var2],col = rev(terrain.colors(100))[1:65],main=title,sub=paste("Correlation between",colnames(donnee)[var1],"and",colnames(donnee)[var2],":",signif(cor(donnee[,var1],donnee[,var2]),4)))
  lines(x,cos(alph/180*pi-acos(x)))
  lines(x,cos(alph/180*pi+acos(x)))
  lines(x,x*0,lty=2)
  lines(0*x,x,lty=2)
  contour(x, y, res,levels=levels,add=TRUE,labex=0)
  abscisse <- aux[,6]
  ordonnee <- aux[,7]
  for(i in firstvar:lastvar) {
    points(aux[i,6],aux[i,7],pch=20)
    text(aux[i,6],aux[i,7],substr(aux[i,1],1,nbchar), pos = 4, offset = 0.2,cex=0.8)
  }
}
}
