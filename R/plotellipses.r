plotellipses <- function (model, keepvar = "all", axes = c(1, 2), means = TRUE, 
                              level = 0.95, magnify = 2, cex = 1, pch = 20, pch.means = 15, 
                              type = c("g", "p"), keepnames = TRUE, namescat = NULL, xlim = NULL, 
                              ylim = NULL, lwd = 1, label = "all",
							  autoLab = c("auto","yes", "no"), graph.type=c("ggplot","classic"), ...) 
{
# lit les options
  graph.type <- match.arg(graph.type[1],c("ggplot","classic"))
  p3p <- list(...)
  
  monpanel <- function(x, y, level, means, nommod, magnify = magnify, 
                       pchmeans = pchmeans, ...) {
    lattice::panel.xyplot(x, y, ...)
    panel.superpose2(x, y, level = level, means = means, 
                     nommod = nommod, pchmeans = pchmeans, magnify = magnify, 
                     panel.groups = "monpanel.ellipse", ...)
  }
  monpanel.ellipse <- function(x, y, col.line, lty, lwd, subscripts, 
                               pch, cex, font, font.family, col, col.symbol, fill, alpha, 
                               type, group.number, level, means, nommod, magnify, pchmeans, 
                               ...) {
    if (length(x) > 1) {
      matrice <- matrix(c(x, y), ncol = 2)
      cdg <- colMeans(matrice)
      if (means) {
        variance <- var(matrice)/length(x)
      }
      else {
        variance <- var(matrice)
      }
      coord.ellipse <- ellipse::ellipse(variance, centre = cdg, 
                                        level = level)
      lattice::llines(coord.ellipse[, 1], coord.ellipse[, 
                                                        2], col = col.line, lty = lty, lwd = lwd)
    }
    else cdg <- c(x, y)
    lattice::ltext(cdg[1], cdg[2], unique(nommod), cex = cex * 
                     magnify, col = col.line, pos = 3, offset = 0.4 * 
                     cex * magnify)
    lattice::lpoints(cdg[1], cdg[2], pch = pchmeans[group.number], 
                     col = col.line, cex = cex * magnify)
  }
  panel.superpose2 <- function(x, y = NULL, subscripts, groups, 
                               panel.groups = "panel.xyplot", col = NA, col.line = superpose.line$col, 
                               col.symbol = superpose.symbol$col, pch = superpose.symbol$pch, 
                               cex = superpose.symbol$cex, fill = superpose.symbol$fill, 
                               font = superpose.symbol$font, fontface = superpose.symbol$fontface, 
                               fontfamily = superpose.symbol$fontfamily, lty = superpose.line$lty, 
                               lwd = superpose.line$lwd, alpha = superpose.symbol$alpha, 
                               type = "p", nommod = nommod, ..., distribute.type = FALSE) {
    if (distribute.type) {
      type <- as.list(type)
    }
    else {
      type <- unique(type)
      wg <- match("g", type, nomatch = NA)
      if (!is.na(wg)) {
        lattice::panel.grid(h = -1, v = -1)
        type <- type[-wg]
      }
      type <- list(type)
    }
    x <- as.numeric(x)
    if (!is.null(y)) 
      y <- as.numeric(y)
    if (length(x) > 0) {
      if (!missing(col)) {
        if (missing(col.line)) 
          col.line <- col
        if (missing(col.symbol)) 
          col.symbol <- col
      }
      superpose.symbol <- lattice::trellis.par.get("superpose.symbol")
      superpose.line <- lattice::trellis.par.get("superpose.line")
      vals <- if (is.factor(groups)) 
        levels(groups)
      else sort(unique(groups))
      nvals <- length(vals)
      col <- rep(col, length = nvals)
      col.line <- rep(col.line, length = nvals)
      col.symbol <- rep(col.symbol, length = nvals)
      pch <- rep(pch, length = nvals)
      fill <- rep(fill, length = nvals)
      lty <- rep(lty, length = nvals)
      lwd <- rep(lwd, length = nvals)
      alpha <- rep(alpha, length = nvals)
      cex <- rep(cex, length = nvals)
      font <- rep(font, length = nvals)
      if (!is.null(fontface)) fontface <- rep(fontface, length = nvals)
      if (!is.null(fontfamily)) fontfamily <- rep(fontfamily, length = nvals)
      type <- rep(type, length = nvals)
      panel.groups <- if (is.function(panel.groups)) 
        panel.groups
      else if (is.character(panel.groups)) 
        get(panel.groups)
      else eval(panel.groups)
      subg <- groups[subscripts]
      ok <- !is.na(subg)
      for (i in seq_along(vals)) {
        id <- ok & (subg == vals[i])
        if (any(id)) {
          args <- list(x = x[id], subscripts = subscripts[id], 
                       pch = pch[i], cex = cex[i], font = font[i], 
                       fontface = fontface[i], fontfamily = fontfamily[i], 
                       col = col[i], col.line = col.line[i], col.symbol = col.symbol[i], 
                       fill = fill[i], lty = lty[i], lwd = lwd[i], 
                       alpha = alpha[i], type = type[[i]], group.number = i, 
                       nommod = (nommod[subscripts])[id], ...)
          if (!is.null(y)) args$y <- y[id]
          do.call(panel.groups, args)
        }
      }
    }
  }
  autoLab <- match.arg(autoLab, c("auto", "yes", "no"))
  if (autoLab == "yes")  autoLab = TRUE
  if (autoLab == "no")  autoLab = FALSE
  nomtot <- names(model$call$X)
  nbevartot <- ncol(model$call$X)
  eliminer <- NULL
  if (class(model)[1] == "PCA") {
    if (all(names(model$call) != "quali.sup")) 
      return(NULL)
    else {
      nomtot <- names(model$call$X)[model$call$quali.sup$numero]
      eliminer <- (1:nbevartot)[-model$call$quali.sup$numero]
      if (is.character(keepvar)) {
        if (length(keepvar) == 1) {
          possibilites <- c("all", "quali", "quali.sup")
          choix <- match(keepvar, possibilites)
          if (is.na(choix)) {
            if (!any(keepvar == nomtot))  return(NULL)
            else eliminer <- unique(c(eliminer, (1:nbevartot)[!(keepvar ==  nomtot)]))
          }
          else {
            if (choix == 2) 
              return(NULL)
          }
        }
        else eliminer <- unique(c(eliminer, (1:nbevartot)[!(nomtot %in% keepvar)]))
      }
      if (is.numeric(keepvar)) eliminer <- unique(c(eliminer, (1:nbevartot)[-keepvar]))
      if (is.logical(keepvar)) eliminer <- unique(c(eliminer, (1:nbevartot)[!keepvar]))
    }
    nomtot <- names(model$call$X)
    if (all((1:nbevartot) %in% eliminer)) 
      return(NULL)
  }
  if (class(model)[1] == "MCA") {
    if (any(names(model$call) == "quanti.sup")) 
      eliminer <- model$call$quanti.sup
    if (is.character(keepvar)) {
      if (length(keepvar) == 1) {
        possibilites <- c("all", "quali", "quali.sup")
        choix <- match(keepvar, possibilites)
        if (is.na(choix)) {
          if (!any(keepvar == nomtot)) 
            return(NULL)
          else eliminer <- unique(c(eliminer, (1:nbevartot)[!(keepvar == nomtot)]))
        }
        else {
          if (choix == 2) eliminer <- c(eliminer, model$call$quali.sup)
          if (choix == 3) eliminer <- (1:nbevartot)[-model$call$quali.sup]
          if (choix == 1) {
            if (all(!(names(model$call) %in% c("quali", "quali.sup")))) return(NULL)
          }
        }
      }
      else eliminer <- unique(c(eliminer, (1:nbevartot)[!(nomtot %in% keepvar)]))
    }
    if (is.numeric(keepvar)) eliminer <- unique(c(eliminer, (1:nbevartot)[-keepvar]))
    if (is.logical(keepvar)) eliminer <- unique(c(eliminer, (1:nbevartot)[!keepvar]))
  }
  if ((class(model)[1] == "MFA") || (class(model)[1] == "FAMD")) {
    eliminer <- which(unlist(lapply(model$call$X, is.numeric)))
    if (is.character(keepvar)) {
      if (length(keepvar) == 1) {
        possibilites <- c("all", "quali", "quali.sup")
        choix <- match(keepvar, possibilites)
        if (is.na(choix)) {
          if (!any(keepvar == nomtot)) {
            return(NULL)
          } else eliminer <- unique(c(eliminer, (1:nbevartot)[!(keepvar == nomtot)]))
        } else {
          if (choix == 2) eliminer <- c(eliminer, which(model$call$nature.var == "quali.sup"))
          if (choix == 3) eliminer <- c(eliminer, which(model$call$nature.var == "quali"))
          if (choix == 1) {
            if (all(!(names(model$call) %in% c("quali", "quali.sup")))) 
              return(NULL)
          }
        }
      } else eliminer <- unique(c(eliminer, (1:nbevartot)[!(nomtot %in% keepvar)]))
    }
    if (is.numeric(keepvar)) eliminer <- unique(c(eliminer, (1:nbevartot)[-keepvar]))
    if (is.logical(keepvar)) eliminer <- unique(c(eliminer, (1:nbevartot)[!keepvar]))
  }
  if (!is.null(eliminer)) 
    nomvargardees <- nomtot[-eliminer] else nomvargardees <- nomtot
  if (!is.logical(keepnames)) {
    if (is.numeric(keepnames)) {
      nomvartrimmees <- nomtot[-unique(c(eliminer, keepnames))]
    } else nomvartrimmees <- nomvargardees[!(nomvargardees %in% keepnames)]
  } else {
    if (length(keepnames) == 1) {
      if (keepnames) {
        nomvartrimmees <- NULL
      } else nomvartrimmees <- nomvargardees
    }
    else {
      if (length(keepnames) == length(nomtot)) {
        nomvartrimmees <- nomtot[(!keepnames) & ((1:nbevartot) != eliminer)]
      } else return(NULL)
    }
  }
  if (is.null(model$call$ind.sup)) {
    if (!is.null(eliminer)) {
      donnees <- model$call$X[, -eliminer, drop = FALSE]
    } else donnees <- model$call$X
  } else {
    if (!is.null(eliminer)) {
      donnees <- model$call$X[-model$call$ind.sup, -eliminer, drop = FALSE]
    } else donnees <- model$call$X[-model$call$ind.sup, , drop = FALSE]
  }
  nbevar <- ncol(donnees)
  if (nbevar == 1) {
    if (keepvar == "all" || keepvar == "quali.sup") {
      var <- model$call$quali.sup$numero
    } else {
      if (is.numeric(keepvar)) {
        var <- keepvar
      } else var <- which(keepvar == colnames(model$call$X))
    }
    if (!is.null(model$call$ind.sup)) {
      aux <- cbind.data.frame(model$call$X[-model$call$ind.sup, 
                                           var, drop = FALSE], model$ind$coord[, 1:max(axes)])
    } else {
      aux <- cbind.data.frame(model$call$X[, var, drop = FALSE], 
                                 model$ind$coord[, 1:max(axes)])
    }
    # if (class(model)[1] == "PCA") {
    #   res.pca <- PCA(aux, ncp = max(axes), quali.sup = 1, scale.unit = FALSE, 
    #                  graph = FALSE, axes = 1:max(axes))
    #   res.pca$eig[axes, ] = model$eig[axes, ]
    #   coord.ell <- coord.ellipse(aux, bary = means, 
    #                              level.conf = level, axes = axes)
    #   if (means == TRUE) {
    #     L <- list(x=model, habillage = var, ellipse = coord.ell, 
    #              cex = cex, label = label, axes = axes, xlim = xlim, 
    #              ylim = ylim, title = paste("Confidence ellipses around the categories of", 
    #                                         colnames(model$call$X)[var]), autoLab = autoLab,graph.type=graph.type)
    #   } else {
    #     L <- list(x=model, habillage = var, ellipse = coord.ell, 
    #                 cex = cex, label = label, axes = axes, xlim = xlim, 
    #                 ylim = ylim, title = paste("Concentration ellipses for the categories of", 
    #                                            colnames(model$call$X)[var]), autoLab = autoLab,graph.type=graph.type)
    #   }
    #   L <- modifyList(L, p3p)
    #   if (graph.type=="ggplot") return(do.call(plot.PCA, L))
    #   else do.call(plot.PCA, L)
    # }
    # if (class(model)[1] == "MCA") {
    #   res.pca <- PCA(aux, ncp = max(axes), quali.sup = 1, scale.unit = FALSE, 
    #                  graph = FALSE, axes = 1:max(axes))
    #   res.pca$eig[axes, ] = model$eig[axes, ]
    #   coord.ell <- coord.ellipse(aux, bary = means, level.conf = level
    #                              , axes = axes)
    #   if (means == TRUE) {
    #     L <- list(x=res.pca, habillage = 1, ellipse = coord.ell, 
    #              cex = cex, label = label, axes = axes, xlim = xlim, 
    #              ylim = ylim, title = paste("Confidence ellipses around the categories of", 
    #                                         colnames(model$call$X)[var]), autoLab = autoLab,graph.type=graph.type)
    #   } else {
    #     L <- list(x=res.pca, habillage = 1, ellipse = coord.ell, 
    #                 cex = cex, label = label, axes = axes, xlim = xlim, 
    #                 ylim = ylim, title = paste("Concentration ellipses for the categories of", 
    #                                            colnames(model$call$X)[var]), autoLab = autoLab,graph.type=graph.type)
    #   }
    #   L <- modifyList(L, p3p)
    #   if (graph.type=="ggplot") return(do.call(plot.PCA, L))
    #   else do.call(plot.PCA, L)
    # }
    if ((class(model)[1] == "PCA")||(class(model)[1] == "MCA")||(class(model)[1] == "MFA") || (class(model)[1] == 
                                       "FAMD")) {
      res.pca <- PCA(aux, ncp = max(axes), quali.sup = 1, scale.unit = FALSE, 
                     graph = FALSE, axes = 1:max(axes))
      res.pca$eig[axes, ] = model$eig[axes, ]
      coord.ell <- coord.ellipse(aux, bary = means, level.conf = level, 
                                 axes = axes)
        L <- list(x=res.pca, habillage = 1, ellipse = coord.ell, 
                  cex = cex, label = label, axes = axes, xlim = xlim, 
                  ylim = ylim, title = paste(if (means == TRUE){"Confidence ellipses around the categories of"} else {"Concentration ellipses for the categories of"}, 
                                             colnames(model$call$X)[var]), autoLab = autoLab, graph.type=graph.type)
      # if (means == TRUE) {
        # L <- list(x=res.pca, habillage = 1, ellipse = coord.ell, 
                  # cex = cex, label = label, axes = axes, xlim = xlim, 
                  # ylim = ylim, title = paste("Confidence ellipses around the categories of", 
                                             # colnames(model$call$X)[var]), autoLab = autoLab, graph.type=graph.type)
      # } else {
        # L <- list(x=res.pca, habillage = 1, ellipse = coord.ell, 
                    # cex = cex, label = label, axes = axes, xlim = xlim, 
                    # ylim = ylim, title = paste("Concentration ellipses for the categories of", 
                                               # colnames(model$call$X)[var]), autoLab = autoLab, graph.type=graph.type)
      # }
      L <- modifyList(L, p3p)
      if (graph.type=="ggplot") return(do.call(plot.PCA, L))
      else do.call(plot.PCA, L)
    }
  } else {
    don <- apply(model$ind$coord[, axes], 2, FUN = function(x, 
            k) rep(x, k), k = nbevar)
    nindiv <- nrow(donnees)
    rownames(don) <- NULL
    colnames(don) <- c("x", "y")
    nomvar <- rep(nomvargardees, each = nindiv)
    modalite2 <- as.vector(apply(data.matrix(donnees), 2, 
                                 unlist))
    if (is.null(namescat)) {
      nommod <- as.vector(apply(donnees, 2, unlist))
    } else nommod <- namescat
    if (!is.null(nomvartrimmees) & (is.null(namescat))) {
      kept <- (1:nbevar)[nomvargardees %in% nomvartrimmees]
      selecti <- as.vector(mapply(seq, (kept - 1) * nindiv + 1, (kept) * nindiv))
      nommod[selecti] <- substr(nommod[selecti], nchar(nomvar[selecti]) + 2, nchar(nommod[selecti]))
    }
    modalite = factor(modalite2)
    don <- cbind.data.frame(don, var = nomvar, modalite = factor(modalite2))
    if (length(pch.means) < max(modalite2)) 
      pch.means <- rep(pch.means, length = max(modalite2))
    if (is.null(xlim) & is.null(ylim)) {
      lattice::xyplot(y ~ x | var, data = don, groups = modalite, 
                      panel = monpanel, level = level, means = means, 
                      magnify = magnify, cex = cex * 0.5, pch = pch, 
                      pchmeans = pch.means, nommod = nommod, type = type, 
                      xlab = paste("Dim ", axes[1], " (", round(model$eig[axes[1], 
                        2], 2), "%)", sep = ""), ylab = paste("Dim ", 
                        axes[2], " (", round(model$eig[axes[2], 2], 
                        2), "%)", sep = ""))
    } else {
      if (is.null(xlim)) {
        xlim <- ylim
      } else {
        if (is.null(ylim)) 
          ylim <- xlim
      }
      lattice::xyplot(y ~ x | var, data = don, groups = modalite, 
                      panel = monpanel, level = level, means = means, 
                      magnify = magnify, cex = cex * 0.5, pch = pch, 
                      pchmeans = pch.means, nommod = nommod, type = type, 
                      xlab = paste("Dim ", axes[1], " (", round(model$eig[axes[1], 
                         2], 2), "%)", sep = ""), ylab = paste("Dim ", 
                         axes[2], " (", round(model$eig[axes[2], 2], 
                         2), "%)", sep = ""), xlim = xlim, ylim = ylim)
    }
  }
}