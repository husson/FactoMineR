plot.FAMD <- function (x, choix = c("ind","var","quanti","quali"), axes = c(1, 2), 
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE, col.hab = NULL, 
    invisible = NULL, lim.cos2.var = 0., xlim = NULL,
    ylim = NULL, title = NULL, palette=NULL, autoLab = c("auto","yes","no"), new.plot = FALSE, 
    select = NULL, unselect = 0.7, shadowtext=FALSE, legend = list(bty = "y", x = "topleft"), graph.type = c("ggplot","classic"), ggoptions = NULL, ...) {

autoLab <- match.arg(autoLab,c("auto","yes","no"))
choix <- match.arg(choix,c("ind","var","quanti","quali"))
graph.type <- match.arg(graph.type[1],c("ggplot","classic"))
old.palette <- palette()
if (is.null(palette)) palette <- c("black", "red", "green3", "blue", "magenta", "darkgoldenrod","darkgray", "orange", "cyan", "violet", "lightpink", "lavender", "yellow", "darkgreen","turquoise", "lightgrey", "lightblue", "darkkhaki","darkmagenta","lightgreen", "darkolivegreen", "lightcyan", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen","darkslateblue", "darkslategray", "darkslategrey","darkturquoise", "darkviolet", "lightgray", "lightsalmon","lightyellow", "maroon")
palette(palette)   # that is necessary

if (choix=="ind" | choix=="quali") {
  class(x) <- c("MFA", "list")
  if (!is.null(x$quali.sup)){
    x$quali.var.sup$coord <- x$quali.sup$coord
    x$quali.var.sup$cos2 <- x$quali.sup$cos2
  }
  if (choix=="quali") {
    invisible <- c(invisible, "ind","ind.sup")
    if (is.null(title)) title <- "Graph of the categories"
  }
  x$call$nature.group <- x$call$nature.var
  x$call$group.mod <- sapply(x$call$X,nlevels)
  x$call$group.mod[x$call$group.mod==0] <- 1
  x$call$group <- rep(1,length(x$call$type))
  x$separate.analyses=vector(mode = "list", length = ncol(x$call$X))
  for (i in 1:ncol(x$call$X)) x$separate.analyses[[i]]$call$X <- x$call$X[,i,drop=FALSE]
  gg_graph <- plot.MFA (x, axes = axes, choix = "ind", lab.var = lab.var,
            lab.ind = lab.ind, lab.par = FALSE, habillage = habillage,
            col.hab = col.hab, invisible = invisible, lim.cos2.var = lim.cos2.var, 
            xlim = xlim, ylim = ylim, title = title, palette=palette, new.plot=new.plot,
            select=select,unselect=unselect,autoLab=autoLab,shadowtext=shadowtext,
            legend=legend, graph.type = graph.type, ggoptions = ggoptions, ...)

} 
if (choix=="var") {
  class(x) <- c("MFA", "list")
  x$group$coord <- x$var$coord
  x$group$cos2 <- x$var$cos2
  x$group$contrib <- x$var$contrib
  if (!is.null(x$var$coord.sup)){
    x$group$coord.sup <- x$var$coord.sup
    x$group$cos2.sup <- x$var$cos2.sup
  }
  if (is.null(col.hab)){
    col.hab <- c(rep("black",sum(x$call$nature.var=="quanti")),
       rep("red",sum(x$call$nature.var=="quali")),
       rep("blue",sum(x$call$nature.var=="quanti.sup")),
       rep("darkred",sum(x$call$nature.var=="quali.sup")))
  }
  if (is.null(title)) title <- "Graph of the variables"
  gg_graph <- plot.MFA (x, axes = axes, choix = "group", lab.var = lab.var,
            lab.ind = lab.ind, lab.par = FALSE, habillage = habillage,
            col.hab = col.hab, invisible = invisible, lim.cos2.var = lim.cos2.var, 
            xlim = xlim, ylim = ylim, title = title, palette=palette, new.plot=new.plot,
            select=select,unselect=unselect,autoLab=autoLab,shadowtext=shadowtext,
            legend=legend, graph.type = graph.type, ggoptions = ggoptions, ...)
}
if (choix=="quanti") {
  class(x) <- c("PCA", "list")
	x$call$scale.unit <- TRUE
	if (lab.var==TRUE) label="all"
	else label="none"
	col.var <- "black"
	col.quanti.sup <- "blue"
	x$var <- x$quanti.var
	x$quanti.sup <- x$quanti.sup
	if (is.null(title)) title <- "Graph of the quantitative variables"

  gg_graph <- plot.PCA(x, axes = axes, choix = "var", label=label,
    habillage = habillage, col.hab = col.hab, col.var = col.var, 
    col.quanti.sup = col.quanti.sup, invisible = invisible, lim.cos2.var = lim.cos2.var, 
    xlim = xlim, ylim = ylim, title = title, palette=palette, new.plot = new.plot,
    select = select,unselect = unselect,autoLab = autoLab,shadowtext = shadowtext, graph.type = graph.type, ggoptions = ggoptions, ...)
} 
  palette(old.palette)
  if (graph.type == "ggplot") return(gg_graph)
}
