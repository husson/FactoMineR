AovSum <- function (formula, data, na.action = na.omit, ...) 
{
  warnings("You should use the LinearModel function instead of the AovSum function that is obsolete")
  old.contr = options()$contrasts
  on.exit(options(contrasts = old.contr))
  if (missing(data)) {
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", 
                 "na.action", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(stats::model.frame)
    data <- eval(mf, parent.frame())
  }
  data <- as.data.frame(data)
  options(contrasts = c("contr.sum", "contr.sum"))
  don = data
  arg <- list(...)
  arg <- c(arg, list(formula = formula, data = don, na.action = na.action))
  modele <- do.call(aov, arg)
  test.F = car::Anova(modele, type = "III")[-1, ]
  test.F = test.F[c(1, 2, 2, 3, 4)]
  test.F[3] = test.F[1]/test.F[2]
  colnames(test.F)[1] = "SS"
  colnames(test.F)[2] = "df"
  colnames(test.F)[3] = "MS"
  test.T = summary.lm(modele)$coef
  cov.mat = vcov(modele)
  facteurs = rownames(attr(modele$terms, "factors"))[-1]
  interact = NULL
  if (length(colnames(attr(modele$terms, "factors"))) > length(facteurs)) 
    interact = colnames(attr(modele$terms, "factors"))[-(1:length(facteurs))]
  niveau = list()
  for (i in 1:length(facteurs)) {
    if (is.factor(don[, facteurs[i]])) 
      niveau[[i]] = paste(facteurs[i], levels(don[, facteurs[i]]), 
                          sep = " - ")
    else niveau[[i]] = facteurs[i]
  }
  res = test.T[c(1, 1), ]
  iinit = 2
  for (i in 1:length(facteurs)) {
    old.rownames = rownames(res)
    if (is.factor(don[, facteurs[i]])) {
      indices = iinit:(iinit + nlevels(don[, facteurs[i]]) - 
                         2)
      dern.mod = c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                                                              indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
                                                                                                                    indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
                                                                                                                                                                                 indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 
                     2)
      res = rbind(res, test.T[indices, ], dern.mod)
      rownames(res) = c(old.rownames, niveau[[i]])
      iinit = iinit + nlevels(don[, facteurs[i]]) - 1
    }
    else {
      indices = iinit
      res = rbind(res, test.T[indices, ])
      rownames(res) = c(old.rownames, niveau[[i]])
      iinit = iinit + 1
    }
  }
  res = res[-1, ]
  if (!is.null(interact)) {
    for (k in 1:length(interact)) {
      fact.int = rownames(attr(modele$terms, "factors"))[which(attr(modele$terms, 
                                                                    "factors")[, interact[k]] == 1)]
      old.rownames = rownames(res)
      fact1 = fact.int[1]
      fact2 = fact.int[2]
      iinit0 = iinit
      if ((is.factor(don[, fact1])) & (is.factor(don[, 
                                                     fact2]))) {
        for (l in 1:(nlevels(don[, fact2]) - 1)) {
          indices = iinit:(iinit + (nlevels(don[, fact1]) - 
                                      2))
          dern.mod = c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                                                                  indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
                                                                                                                        indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
                                                                                                                                                                                     indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 
                         2)
          res = rbind(res, test.T[indices, ], dern.mod)
          iinit = iinit + (nlevels(don[, fact1]) - 1)
        }
        iinit = iinit0
        for (l in 1:(nlevels(don[, fact1]) - 1)) {
          indices = iinit + (nlevels(don[, fact1]) - 
                               1) * (0:(nlevels(don[, fact2]) - 2))
          dern.mod = c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                                                                  indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
                                                                                                                        indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
                                                                                                                                                                                     indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 
                         2)
          res = rbind(res, dern.mod)
          iinit = iinit + 1
        }
        indices = iinit0:(iinit0 + (nlevels(don[, fact1]) - 
                                      1) * (nlevels(don[, fact2]) - 1) - 1)
        dern.mod = c(sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                                                               indices])), sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
                                                                                                                    indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
                                                                                                                                                                                 indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 
                       2)
        res = rbind(res, dern.mod)
        iinit = iinit0 + (nlevels(don[, fact1]) - 1) * 
          (nlevels(don[, fact2]) - 1)
        nom = old.rownames
        aa = paste(fact2, levels(don[, fact2]), sep = " - ")
        for (i in 1:length(aa)) nom = c(nom, paste(paste(fact1, 
                                                         levels(don[, fact1]), sep = " - "), aa[i], 
                                                   sep = " : "))
      }
      if ((is.factor(don[, fact1])) & (!is.factor(don[, 
                                                      fact2]))) {
        indices = iinit:(iinit + (nlevels(don[, fact1]) - 
                                    2))
        dern.mod = c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                                                                indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
                                                                                                                      indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
                                                                                                                                                                                   indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 
                       2)
        res = rbind(res, test.T[indices, ], dern.mod)
        iinit = iinit + (nlevels(don[, fact1]) - 1)
        nom = c(old.rownames, paste(paste(fact1, levels(don[, 
                                                            fact1]), sep = " - "), fact2, sep = " : "))
      }
      if ((!is.factor(don[, fact1])) & (is.factor(don[, 
                                                      fact2]))) {
        indices = iinit:(iinit + (nlevels(don[, fact2]) - 
                                    2))
        dern.mod = c(-sum(test.T[indices, 1]), sqrt(sum(cov.mat[indices, 
                                                                indices])), -sum(test.T[indices, 1])/sqrt(sum(cov.mat[indices, 
                                                                                                                      indices])), pt(abs(sum(test.T[indices, 1]))/sqrt(sum(cov.mat[indices, 
                                                                                                                                                                                   indices])), test.F[nrow(test.F), 2], lower.tail = FALSE) * 
                       2)
        res = rbind(res, test.T[indices, ], dern.mod)
        iinit = iinit + (nlevels(don[, fact2]) - 1)
        nom = c(old.rownames, paste(paste(fact2, levels(don[, 
                                                            fact2]), sep = " - "), fact1, sep = " : "))
      }
      if ((!is.factor(don[, fact1])) & (!is.factor(don[, 
                                                       fact2]))) {
        indices = iinit
        res = rbind(res, test.T[indices, ])
        iinit = iinit + 1
        nom = c(old.rownames, paste(fact1, fact2, sep = " : "))
      }
      rownames(res) = nom
    }
  }
  result = list(Ftest = test.F, Ttest = res)
  class(result) <- "AovSum"
  options(contrasts = old.contr)
  return(result)
}
