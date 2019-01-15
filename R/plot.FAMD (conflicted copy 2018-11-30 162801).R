plot.FAMD <- function (x, choix = c("ind","var","quanti","quali"), axes = c(1, 2), 
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE, col.hab = NULL, 
    invisible = NULL, lim.cos2.var = 0., xlim = NULL,
    ylim = NULL, title = NULL, palette=NULL, autoLab = c("auto","yes","no"), new.plot = FALSE, 
    select = NULL, unselect = 0.7, shadowtext=FALSE, legend = list(bty = "y", x = "topleft"), ...) {

autoLab <- match.arg(autoLab,c("auto","yes","no"))
choix <- match.arg(choix,c("ind","var","quanti","quali"))

if (choix=="quanti") {
  class(x) <- c("PCA", "list")
	x$call$scale.unit <- TRUE
	choix="var"
	if (lab.var==TRUE) label="all"
	else label="none"
	col.var <- "black"
	col.quanti.sup <- "blue"
	x$var <- x$quanti.var
	x$quanti.sup <- x$quanti.var.sup
	if (is.null(title)) title <- "Graph of the quantitative variables"

  plot.PCA(x, axes = axes, choix = "var", label=label,
    habillage = habillage, col.hab = col.hab, col.var = col.var, 
    col.quanti.sup = col.quanti.sup, invisible = invisible, lim.cos2.var = lim.cos2.var, 
    xlim = xlim, ylim = ylim, title = title, palette=palette, new.plot = new.plot,
    select = select,unselect = unselect,autoLab = autoLab,shadowtext = shadowtext,...)
} else {
  class(x) <- c("MFA", "list")
  if (choix=="var"){
    choix="group"
  }
  x$group$coord <- x$var$coord
  x$group$cos2 <- x$var$cos2
  x$group$contrib <- x$var$contrib
  if (!is.null(x$quali.sup)){
    x$quali.var.sup$coord <- x$quali.sup$coord
    x$quali.var.sup$cos2 <- x$quali.sup$cos2
  }
  if (is.null(col.hab)){
    col.hab <- c(rep("darkblue",sum(x$call$nature.var=="quanti")),
                 rep("red",sum(x$call$nature.var=="quali")),
                 rep("blue",sum(x$call$nature.var=="quanti.sup")),
                 rep("darkgreen",sum(x$call$nature.var=="quali.sup")))
  }
  x$call$group <- rep(1,length(x$call$type))
  if (!is.null(x$var$coord.sup)){
    x$group$coord.sup <- x$var$coord.sup
    x$group$cos2.sup <- x$var$cos2.sup
  }
  x$separate.analyses=vector(mode = "list", length = ncol(x$call$X))
  for (i in 1:ncol(x$call$X)) x$separate.analyses[[i]]$call$X <- x$call$X[,i,drop=FALSE]
  if (choix=="quali") {
    choix <- "ind"
    invisible <- c(invisible, "ind","ind.sup")
    if (is.null(title)) title <- "Graph of the categories"
  }
  if ((choix=="group")&is.null(title)) title <- "Graph of the variables"
  plot.MFA (x, axes = axes, choix = choix, lab.var = lab.var,
            lab.ind = lab.ind, lab.par = FALSE, habillage = habillage,
            col.hab = col.hab, invisible = invisible, lim.cos2.var = lim.cos2.var, 
            xlim = xlim, ylim = ylim, title = title, palette=palette, new.plot=new.plot,
            select=select,unselect=unselect,autoLab=autoLab,shadowtext=shadowtext,
            legend=legend,...)
} 
}
