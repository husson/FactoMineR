condes <- function(donnee, num.var, weights = NULL, proba = 0.05) {

  cor.calc <- function(y, x, w = NULL) {
    if (is.null(w)) w <- rep(1, length(x))
    missing <- rowSums(is.na(cbind(x, y))) > 0
    x <- x[!missing]; y <- y[!missing]; w <- w[!missing]
    n <- sum(w)
    if (n < 3) n <- n * length(x)
    r <- cov.wt(cbind(x, y), wt = w, method = "ML", cor = TRUE)$cor[1, 2]
    list(r = r, proba = pt(sqrt(n - 2) * sqrt(r^2 / (1 - r^2)), n - 2,
                    lower.tail = FALSE) * 2)
  }

test.aov.w <- function(y, x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))

  missing <- is.na(y) | is.na(x)
  y <- y[!missing]; x <- x[!missing]; w <- w[!missing]
  x <- droplevels(x)
  W     <- sum(w)
  nlevs <- nlevels(x)
  mu        <- sum(w * y) / W
  w_by_grp  <- tapply(w,     x, sum)
  mu_by_grp <- tapply(w * y, x, sum) / w_by_grp
  SCE <- sum(w_by_grp * (mu_by_grp - mu)^2)
  SCT <- sum(w * (y - mu)^2)
  SCR <- SCT - SCE
  ddl_E  <- nlevs - 1
  ddl_R  <- length(y) - nlevs
  Fstat  <- (SCE / ddl_E) / (SCR / ddl_R)
  R2     <- SCE / SCT
  pval_F <- pf(Fstat, ddl_E, ddl_R, lower.tail = FALSE)
  tabF   <- c(R2, pval_F)

  # Estimate au sens contr.sum
  mu_contrsum  <- mean(mu_by_grp)
  Estimate_k1  <- as.numeric(mu_by_grp[-nlevs] - mu_contrsum)
  Estimate     <- c(Estimate_k1, -sum(Estimate_k1))

  pvals <- vapply(levels(x), function(lv) {
    cor.calc(y, as.numeric(x == lv), w = w)$proba
  }, numeric(1))

  resT <- cbind(Estimate = Estimate, p.value = pvals)
  rownames(resT) <- levels(x)

  list(tabF = tabF, resT = resT)
}

  donnee   <- as.data.frame(donnee)
  is.quali <- which(!vapply(donnee, is.numeric, logical(1)))
  donnee[, is.quali] <- lapply(donnee[, is.quali, drop = FALSE], as.factor)
  donnee   <- droplevels(donnee)
  lab      <- colnames(donnee)

  if (is.null(weights)) weights <- rep(1, nrow(donnee))
  if (sum(weights) < 3) weights <- weights * nrow(donnee)

  quali <- NULL
  for (i in is.quali) {
    col <- donnee[, i]
    if (anyNA(col)) {
      levels(col) <- c(levels(col), "NA")
      col[is.na(col)] <- "NA"
    }
    if (levels(col)[1] == "") levels(col)[1] <- "NA"
    if (i != num.var) {
      levels(col) <- paste(colnames(donnee)[i], levels(col), sep = "=")
      quali <- c(quali, i)
    }
    donnee[, i] <- col
  }

  quanti <- setdiff(seq_len(ncol(donnee)), c(quali, num.var))
  if (length(quanti) == 0) quanti <- NULL
  colnames(donnee) <- lab
  result <- list()

  if (!is.null(quanti)) {
    if (length(quanti) > 1) {
      tab.quanti <- lapply(donnee[, quanti, drop = FALSE],
                           cor.calc, donnee[, num.var], w = weights)
      aux <- do.call(rbind, lapply(tab.quanti, function(x) c(x$r, x$proba)))
    } else {
      cc  <- cor.calc(donnee[, quanti], donnee[, num.var], w = weights)
      aux <- matrix(c(cc$r, cc$proba), nrow = 1)
    }
    n_obs <- colSums(!is.na(donnee[, quanti, drop = FALSE]) &
                     !is.na(donnee[, num.var]))
    aux   <- data.frame(aux, n = n_obs)
    rownames(aux) <- colnames(donnee)[quanti]
    colnames(aux) <- c("correlation", "p.value", "n")

    if (nrow(aux) > 1) aux <- aux[order(-aux[, 1]), ]
    resQ <- aux[!is.na(aux$p.value) & aux$p.value <= proba & aux$n > 2,
                , drop = FALSE]
    if (nrow(resQ) == 0) resQ <- NULL
    result$quanti <- resQ
  }

  if (!is.null(quali)) {
    old.contr <- options()$contrasts
    options(contrasts = c("contr.sum", "contr.sum"))
    on.exit(options(contrasts = old.contr), add = TRUE)

    nq    <- length(quali)
    tabF  <- matrix(NA, nq, 2, dimnames = list(colnames(donnee)[quali],
                                               c("R2", "p.value")))
    tabT_list <- vector("list", nq)

    for (v in seq_len(nq)) {
      resaov <- test.aov.w(donnee[, num.var], donnee[, quali[v]], w = weights)
      tabF[v, ]       <- resaov$tabF
      resT            <- resaov$resT
      rownames(resT)  <- levels(donnee[, quali[v]])
      tabT_list[[v]]  <- resT
    }

    tabT <- do.call(rbind, tabT_list)   # plus rapide que rbind iteratif

    resF <- resT <- NULL
    keep_F <- !is.na(tabF[, 2]) & tabF[, 2] <= proba
    if (any(keep_F)) resF <- tabF[keep_F, , drop = FALSE][order(tabF[keep_F, 2]), ]

    oo   <- order(sign(tabT[, 1]) / tabT[, 2], decreasing = TRUE)
    tabT <- tabT[oo, ]
    keep_T <- !is.na(tabT[, 2]) & tabT[, 2] <= proba
    if (any(keep_T)) resT <- tabT[keep_T, , drop = FALSE]

    result$quali    <- resF
    result$category <- resT
  }

  if (is.null(quali) && is.null(quanti))
    message("The value of proba is too small. Choose a proba greater than ", proba)

  result$call <- list(num.var = num.var, proba = proba, weights = weights, X = donnee)
  class(result) <- c("condes", "list")
  result
}