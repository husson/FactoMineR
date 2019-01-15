coord.ellipse <- function (coord.simul, centre = NULL, axes = c(1, 2), level.conf = 0.95, npoint = 100, bary = FALSE){

    ellipse <- function(x, scale = c(1, 1), centre = c(0, 0), level = 0.95, t = sqrt(qchisq(level, 2)), which = c(1, 2), npoints = 100) {
      names <- c("x", "y")
      if (is.matrix(x)) {
        xind <- which[1]
        yind <- which[2]
        r <- x[xind, yind]
        if (missing(scale)) {
            scale <- sqrt(c(x[xind, xind], x[yind, yind]))
            if (scale[1] > 0) r <- r/scale[1]
            if (scale[2] > 0) r <- r/scale[2]
        }
        if (!is.null(dimnames(x)[[1]])) names <- dimnames(x)[[1]][c(xind, yind)]
      }
      else r <- x
      r <- min(max(r, -1), 1)
      d <- acos(r)
      a <- seq(0, 2 * pi, len = npoints)
      matrix(c(t * scale[1] * cos(a + d/2) + centre[1], t * scale[2] * cos(a - d/2) + centre[2]), npoints, 2, dimnames = list(NULL, names))
    }
    #### End function ellipse
    
    nbre.fact <- nlevels(coord.simul[, 1])
    res <- NULL
    label <- NULL
    lev <- levels(coord.simul[, 1])
    for (f in 1:nbre.fact) {
#      x <- coord.simul[which(coord.simul[, 1] == unique(coord.simul[, 1])[f]), axes[1] + 1]
#      y <- coord.simul[which(coord.simul[, 1] == unique(coord.simul[, 1])[f]), axes[2] + 1]
      x <- coord.simul[which(coord.simul[, 1] == lev[f]), axes[1] + 1]
      y <- coord.simul[which(coord.simul[, 1] == lev[f]), axes[2] + 1]
      if (is.null(centre))  center <- c(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE))
      else {
        if (ncol(coord.simul) != ncol(centre)) stop("ncol de centre incorrect")
        if (!all.equal(lev, levels(centre[, 1]))) stop("Levels of centre are not corrects")
#        center <- as.numeric(centre[which(centre[, 1] == unique(centre[, 1])[f]), c(axes[1] + 1, axes[2] + 1)])
        center <- as.numeric(centre[which(centre[, 1] == levels(centre[, 1])[f]), c(axes[1] + 1, axes[2] + 1)])
      }
      tab <- data.frame(x = x, y = y)
      if (nrow(tab)>1) mat.cov <- cov(tab)
	  else mat.cov <- matrix(0,4,ncol=2)
      if (bary) mat.cov = mat.cov/nrow(tab)
      elli.tmp <- ellipse::ellipse(mat.cov, centre = center, level = level.conf, npoints = npoint)
      res <- rbind(res, elli.tmp)
#      label <- c(label, rep(lev[f], npoint))
    }
    label <- factor(rep(lev,each=npoint),levels=lev)
    result <- data.frame(facteur = label, res)
    colnames(result)[1]="facteur"
    colnames(result) <- colnames(coord.simul)[c(1, axes + 1)]
    return(list(res = result, call = npoint))
}
