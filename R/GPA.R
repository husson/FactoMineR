GPA<-function (df, tolerance = 10^-10, nbiteration = 200, scale = TRUE,
    group, name.group = NULL, graph = TRUE, axes = c(1, 2))
{
    ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
        if (length(dim(X)) > 2 || !(is.numeric(X) || is.complex(X)))
            stop("'X' must be a numeric or complex matrix")
        if (!is.matrix(X))
            X <- as.matrix(X)
        Xsvd <- svd(X)
        if (is.complex(X))
            Xsvd$u <- Conj(Xsvd$u)
        Positive <- Xsvd$d > max(tol * Xsvd$d[1], 0)
        if (all(Positive))
            Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
        else if (!any(Positive))
            array(0, dim(X)[2:1])
        else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) *
            t(Xsvd$u[, Positive, drop = FALSE]))
    }
    similarite <- function(X, Y) {
        if (dim(X)[[1]] != dim(Y)[[1]])
            stop("not the same dimension for X and Y")
        Y <- scale(Y, scale = FALSE)
        X <- scale(X, scale = FALSE)
        y <- Y %*% procrustesbis(Y, X)$H
        similari <- sum(diag(t(X) %*% y))/(sum(diag(t(X) %*%
            X)) * sum(diag(t(y) %*% y)))^0.5
        return(similari)
    }
    crit.procGPAcvmqte <- function(x) {
        if (!inherits(x, "GPAc"))
            stop("Object of type 'GPAc' expected")
        contribindiv <- matrix(0, dim(x$Xfin)[[1]], 3)
        contriconfig <- matrix(0, dim(x$Xfin)[[3]], 3)
        contridim <- matrix(0, dim(x$consensus)[[2]], 3)
        nomligne <- row.names(x$depart)
        nomconfig <- (x$name.group)
        Maii <- 0 * (x$M[, , 1] %*% x$consensus %*% t(x$M[, ,
            1] %*% x$consensus))
        for (i in 1:dim(x$M)[[3]]) {
            Maii <- Maii + x$M[, , i] %*% x$consensus %*% t(x$M[,
                , i] %*% x$consensus)
        }
        aii <- diag(Maii)
        Meii <- x$Xfin[, , 1] - x$consensus
        Mbii <- 0 * (x$Cj[, , 1] %*% Meii %*% t(x$Cj[, , 1] %*%
            Meii))
        for (i in 1:dim(x$M)[[3]]) {
            Meii <- x$Xfin[, , i] - x$consensus
            Mbii <- Mbii + (x$Cj[, , i] %*% Meii %*% t(x$Cj[,
                , i] %*% Meii))
        }
        bii <- diag(Mbii)
        Mdii <- 0 * x$Xfin[, , 1] %*% t(x$Xfin[, , 1])
        for (i in 1:dim(x$M)[[3]]) {
            Mdii <- Mdii + x$Xfin[, , i] %*% t(x$Xfin[, , i])
        }
        dii <- diag(Mdii)
        for (i in 1:dim(x$Xfin)[[1]]) {
            contribindiv[i, 1] <- aii[i]
            contribindiv[i, 2] <- bii[i]
            contribindiv[i, 3] <- dii[i]
        }
        contribindiv <- rbind(contribindiv, colSums(contribindiv))
        rownames(contribindiv) <- c(nomligne, "sum")
        colnames(contribindiv) <- c("SSfit", "SSresidual", "SStotal")
        contriconfig <- matrix(0, dim(x$Xfin)[[3]], 3)
        contridim <- matrix(0, dim(x$consensus)[[2]], 3)
        for (i in 1:dim(x$M)[[3]]) {
            Meii <- x$Xfin[, , i] - x$consensus
            contriconfig[i, 1] <- x$poids[i] * sum(diag(t(x$Z) %*%
                x$Cj[, , i] %*% x$Xdeb[, , i] %*% x$R[, , i]))
            contriconfig[i, 2] <- sum(diag(t(Meii) %*% x$Cj[,
                , i] %*% Meii))
            contriconfig[i, 3] <- x$poids[i]^2 * (sum(diag(t(x$Xdeb[,
                , i]) %*% x$Cj[, , i] %*% x$Xdeb[, , i]))) -
                sum(diag(t(x$consensus) %*% x$Cj[, , i] %*% Meii))
        }
        contriconfig <- rbind(contriconfig, colSums(contriconfig))
        rownames(contriconfig) <- c(nomconfig, "sum")
        colnames(contriconfig) <- c("SSfit", "SSresidual", "SStotal")
        Mfii <- 0 * (t(x$Xfin[, , 1] - x$consensus) %*% x$Cj[,
            , 1] %*% (x$Xfin[, , 1] - x$consensus))
        Mgii <- 0 * (t(x$poids[1] * x$Cj[, , 1] %*% x$Xdeb[,
            , 1] %*% x$R[, , 1] %*% x$K) %*% (x$poids[1] * x$Cj[,
            , 1] %*% x$Xdeb[, , 1] %*% x$R[, , 1] %*% x$K))
        for (i in 1:dim(x$M)[[3]]) {
            Mfii <- Mfii + t(x$Xfin[, , i] - x$consensus) %*%
                x$Cj[, , i] %*% (x$Xfin[, , i] - x$consensus)
            Mgii <- Mgii + t(x$poids[i] * x$Cj[, , i] %*% x$Xdeb[,
                , i] %*% x$R[, , i] %*% x$K) %*% (x$poids[i] *
                x$Cj[, , i] %*% x$Xdeb[, , i] %*% x$R[, , i] %*%
                  x$K)
        }
        for (i in 1:dim(x$consensus)[[2]]) {
            contridim[i, 1] <- diag(x$gama)[i]
            contridim[i, 2] <- diag(Mfii)[i]
            contridim[i, 3] <- diag(Mgii)[i]
        }
        contridim <- rbind(contridim, colSums(contridim))
        rownames(contridim) <- c(c(1:dim(x$consensus)[[2]]),
            "sum")
        colnames(contridim) <- c("SSfit", "SSresidual", "SStotal")
        contribution <- list()
        contribution$objet <- contribindiv
        contribution$config <- contriconfig
        contribution$dim <- contridim
        return(contribution)
    }
    crit.procGPAcsansvm <- function(x) {
        if (!inherits(x, "GPAc"))
            stop("Object of type 'GPAc' expected")
        contribindiv <- matrix(0, dim(x$Xfin)[[1]], 3)
        contribconfig <- matrix(0, dim(x$Xfin)[[3]], 3)
        contridim <- matrix(0, dim(x$consensus)[[2]], 3)
        general <- matrix(0, 7, 6)
        U <- matrix(1, dim(x$Xfin)[[1]], 1)
        nbj <- dim(x$Xfin)[[3]]
        nbligne <- dim(x$Xdeb)[[1]]
        nbcol <- dim(x$Xfin)[[2]]
        nomligne <- row.names(x$depart)
        nomconfig <- (x$name.group)
        contribconfigdim <- matrix(0, dim(x$Xfin)[[3]], dim(x$Xfin)[[2]] *
            2)
        s <- NULL
        s1 <- NULL
        s3 <- NULL
        for (k in 1:dim(x$Xfin)[[2]]) {
            s <- NULL
            s1 <- NULL
            for (i in 1:dim(x$Xfin)[[3]]) {
                s <- rbind(s, colSums(x$Xfin[, , i]^2, na.rm = F,
                  dims = 1))
                s1 <- rbind(s1, colSums((x$Xfin[, , i] - x$consensus)^2,
                  na.rm = F, dims = 1))
            }
            contribconfigdim <- cbind(s1, s)
        }
        contribconfigdim <- rbind(contribconfigdim, colSums(contribconfigdim))
        nomfit <- NULL
        nomfit <- c(paste("SSresidual", 1:dim(x$Xfin)[[2]], ""),
            paste("SStotal", 1:dim(x$Xfin)[[2]], sep = ""))
        colnames(contribconfigdim) <- nomfit
        row.names(contribconfigdim) <- c(nomconfig, "sum")
        s <- NULL
        s1 <- NULL
        s3 <- NULL
        for (i in 1:dim(x$Xfin)[[3]]) {
            s <- rbind(s, colSums(x$Xfin[, , i]^2, na.rm = F,
                dims = 1))
            s1 <- rbind(s1, colSums((x$Xfin[, , i] - x$consensus)^2,
                na.rm = F, dims = 1))
            s3 <- rbind(s1, colSums((x$consensus)^2, na.rm = F,
                dims = 1))
        }
        contribconfig[, 3] <- rowSums(s, na.rm = F, dims = 1)
        contribconfig[, 2] <- rowSums(s1, na.rm = F, dims = 1)
        contribconfig[, 1] <- 0
        contribconfig <- rbind(contribconfig, colSums(contribconfig))
        colnames(contribconfig) <- c("SSfit", "SSresidual", "SStotal")
        row.names(contribconfig) <- c(nomconfig, "sum")
        s <- 0 * rowSums(x$Xfin[, , 1]^2, na.rm = F, dims = 1)
        s1 <- 0 * rowSums((x$Xfin[, , i] - x$consensus)^2, na.rm = F,
            dims = 1)
        tt <- NULL
        for (i in 1:dim(x$Xfin)[[3]]) {
            s <- s + rowSums(x$Xfin[, , i]^2, na.rm = F, dims = 1)
            s1 <- s1 + rowSums((x$Xfin[, , i] - x$consensus)^2,
                na.rm = F, dims = 1)
            tt <- cbind(tt, rowSums((x$Xfin[, , i] - x$consensus)^2,
                na.rm = F, dims = 1))
        }
        contribindiv[, 3] <- s
        contribindiv[, 2] <- s1
        contribindiv[, 1] <- nbj * rowSums((x$consensus)^2, na.rm = F,
            dims = 1)
        contibis <- NULL
        contibis2 <- NULL
        for (i in 1:dim(x$Xfin)[[3]]) {
            contibis <- cbind(contibis, rowSums((x$Xfin[, , i] -
                x$consensus)^2, na.rm = F, dims = 1))
            contibis2 <- contibis/contribindiv[, 2] * 100
            contibis2 <- contibis2 * nbj/100
        }
        contribindiv <- rbind(contribindiv, colSums(contribindiv))
        colnames(contribindiv) <- c("SSfit", "SSresidual", "SStotal")
        row.names(contribindiv) <- c(nomligne, "sum")
        contibis <- cbind(contibis, contibis2)
        contibis <- (rbind(contibis, colSums(contibis)))
        colnames(contibis) <- c(paste("SSresidual", "ratio",
            nomconfig), paste("SSresidual", "raw", nomconfig))
        row.names(contibis) <- c(nomligne, "sum")
        s <- 0 * (x$Xfin[, , 1]^2)
        s1 <- 0 * (x$Xfin[, , i] - x$consensus)^2
        tt <- NULL
        for (i in 1:dim(x$Xfin)[[3]]) {
            s <- s + (x$Xfin[, , i]^2)
            s1 <- s1 + (x$Xfin[, , i] - x$consensus)^2
        }
        contribindivdim <- cbind(nbj * (x$consensus)^2, s1, s)
        colnames(contribindivdim) <- c(paste("SSfit", 1:dim(x$consensus)[[2]],
            sep = ""), paste("SSresidual", 1:dim(x$consensus)[[2]],
            sep = ""), paste("SStotal", 1:dim(x$consensus)[[2]]))
        s <- NULL
        s1 <- NULL
        s3 <- NULL
        for (i in 1:dim(x$Xfin)[[3]]) {
            s <- rbind(s, colSums(x$Xfin[, , i]^2, na.rm = F))
            s1 <- rbind(s1, colSums((x$Xfin[, , i] - x$consensus)^2,
                na.rm = F, dims = 1))
            s3 <- rbind(s1, colSums((x$consensus)^2, na.rm = F,
                dims = 1))
        }
        s3 <- colSums((x$consensus)^2, na.rm = F, dims = 1)
        contridim <- cbind(nbj * (s3), (colSums(s1)), (colSums(s)))
        contridim <- rbind(contridim, colSums(contridim))
        colnames(contridim) <- c("Consensus", "residus", "Total")
        row.names(contridim) <- c(paste("dim", 1:dim(x$Xfin)[[2]]),
            "Total")
        contribution <- list()
        contribution$objet <- contribindiv/nbj * 100
        contribution$contribindivdim <- contribindivdim/nbj *
            100
        contribution$contibis <- contibis/nbj * 100
        contribution$config <- contribconfig/nbj * 100
        contribution$contribconfigdim <- contribconfigdim/nbj *
            100
        contribution$dimension <- contridim/nbj * 100
        return(contribution)
    }
    placevm <- function(mat) {
        if (!is.matrix(mat))
            stop("A matrix please !!")
        vect <- NULL
        matri <- NULL
        i <- 1
        while (i <= dim(mat)[[1]]) {
            if (any(is.na(mat[i, ]))) {
                matri <- matri
                vect <- c(vect, i)
                i <- i + 1
            }
            else {
                matri <- rbind(matri, mat[i, ])
                i <- i + 1
            }
        }
        sol <- list()
        sol$matri <- matri
        sol$vect <- vect
        return(sol)
    }
    procrustesbis <- function(X1, X2) {
        if (!is.matrix(X1))
            stop("A matrix please !!" )
        if (!is.matrix(X2))
            stop("A matrix please !!")
        if (dim(X2)[[2]] != dim(X1)[[2]])
            stop("On souhaite ici avoir un tableau de type matrice ayant le meme nombre de colonnes  ")
        nbcolonne <- dim(X1)[[2]]
        X1c <- scale(X1, scale = FALSE)
        X2c <- scale(X2, scale = FALSE)
        Aj <- t(X1c) %*% X2c
        lola <- eigen(t(Aj) %*% Aj)
        Qj <- as.matrix(eigen(t(Aj) %*% Aj)$vectors)
        phij <- diag(lola$values, length(lola$values), length(lola$values))
        opp1 <- abs(lola$values)^0.5
        H <- diag(1, nbcolonne)
        rang <- sum((opp1/opp1[1]) > 10^-7)
        if (is.na(rang)) {
            H <- H
            rho <- sum(diag(X1c %*% H %*% t(X2c)))/sum(diag(X1c %*%
                t(X1)))
        }
        else {
            if (rang == length(lola$values)) {
                Pj <- Aj %*% Qj %*% as.matrix(diag(diag(phij)^(-0.5),
                  length(lola$values), length(lola$values)))
                H <- Pj %*% t(Qj)
            }
            else {
                nbracinepos <- rang
                Pjstar <- Aj %*% Qj[, 1:nbracinepos] %*% as.matrix(diag((diag(phij)[1:nbracinepos])^(-0.5),
                  length(lola$values[1:nbracinepos]), length(lola$values[1:nbracinepos])))
                Pbar <- matrix(0, nbcolonne, (length(lola$values) -
                  nbracinepos))
                Pjstar1 <- Pjstar
                for (k in 1:(length(lola$values) - nbracinepos)) {
                  yinit <- rnorm(nbcolonne)
                  yinit.lm <- lm(yinit ~ 0 + Pjstar1)
                  vectorth <- yinit - fitted.values(yinit.lm)
                  Pbar[, k] <- vectorth/(t(vectorth) %*% vectorth)^0.5
                  if (sum(sign(Pbar[, k]/Qj[, (nbracinepos +
                    k)]), na.rm = TRUE) < 0) {
                    Pjstar1 <- cbind(Pjstar1, -Pbar[, k])
                  }
                  else {
                    Pjstar1 <- cbind(Pjstar1, Pbar[, k])
                  }
                }
                H <- Pjstar1 %*% t(as.matrix(lola$vectors))
            }
            rho <- sum(diag(X1c %*% H %*% t(X2c)))/sum(diag(X1c %*%
                t(X1)))
        }
        result <- list()
        result$H <- H
        result$rho <- rho
        return(result)
    }
    algogpa <- function(X, tolerance = 10^-7, nbiteration = 200,
        scale = TRUE, df, name.group) {
        Xm <- NULL
        v <- NULL
        M <- NULL
        C <- NULL
        U <- NULL
        Ip <- NULL
        vdiag <- NULL
        mat1 <- NULL
        Cc <- NULL
        invgC <- NULL
        Xm1 <- NULL
        nbj <- NULL
        nbjuge <- NULL
        pds <- NULL
        nbjuge <- dim(X)[[3]]
        p <- dim(X)[[1]]
        nbcolonne <- dim(X)[[2]]
        M <- array(0, c(p, p, nbjuge))
        Cj <- array(0, c(p, p, nbjuge))
        U <- matrix(1, p, 1)
        Ip <- diag(rep(1, p), p, p)
        Xm <- X
        v <- rep(1, p)
        VMQTE <- FALSE
        for (j in 1:nbjuge) {
            vdiag <- v
            if (length(placevm(Xm[, , j])$vect) == 0) {
                M[, , j] <- Ip
            }
            else {
                vdiag[placevm(Xm[, , j])$vect] <- 0
                M[, , j] <- diag(vdiag, length(vdiag), length(vdiag))
                VMQTE <- TRUE
            }
        }
        vdiag <- NULL
        mat1 <- U %*% t(U)
        for (j in 1:nbjuge) {
            Cj[, , j] <- M[, , j] %*% (Ip - mat1 %*% M[, , j]/as.numeric(t(U) %*%
                M[, , j] %*% U))
        }
        Cc <- Cj[, , 1]
        for (j in 2:nbjuge) {
            Cc <- Cc + Cj[, , j]
        }
        invgC <- ginv(Cc)
        Xm1 <- Xm
        for (j in 1:nbjuge) {
            for (i in 1:dim(Xm)[[1]]) {
                Xm1[i, , j] <- replace(Xm1[i, , j], is.na(Xm1[i,
                  , j]), 999999)
            }
        }
        lambda2 <- 0
        for (j in 1:nbjuge) {
            lambda2 <- lambda2 + sum(diag(t(Xm1[, , j]) %*% Cj[,
                , j] %*% Xm1[, , j]))
        }
        lambda <- (nbjuge/(lambda2))^0.5
        Xnorm <- Xm1 * lambda
        diagW <- NULL
        for (i in 1:nbjuge) {
            diagW <- c(diagW, 1/sum(diag(t(Xnorm[, , i]) %*%
                Cj[, , i] %*% Xnorm[, , i]))^0.5)
        }
        W12 <- diag(diagW, nbjuge, nbjuge)
        pds <- rep(1, nbjuge)
        R <- array(0, c(nbcolonne, nbcolonne, nbjuge))
        Im <- diag(rep(1, nbcolonne), nbcolonne, nbcolonne)
        for (i in 1:nbjuge) {
            R[, , i] <- Im
        }
        Aj <- array(0, c(nbcolonne, nbcolonne, nbjuge))
        for (j in 1:nbjuge) {
            sommetemp <- pds[1] * Cj[, , 1] %*% Xnorm[, , 1] %*%
                R[, , 1]
            for (i in 2:nbjuge) {
                sommetemp <- sommetemp + pds[i] * Cj[, , i] %*%
                  Xnorm[, , i] %*% R[, , i]
            }
            R[, , j] <- procrustesbis(Cj[, , j] %*% Xnorm[, ,
                j], invgC %*% (sommetemp - pds[j] * Cj[, , j] %*%
                Xnorm[, , j] %*% R[, , j]))$H
        }
        sommetemp2 <- NULL
        sommetemp2 <- pds[1] * Cj[, , 1] %*% Xnorm[, , 1] %*%
            R[, , 1]
        for (i in 2:nbjuge) {
            sommetemp2 <- sommetemp2 + pds[i] * Cj[, , i] %*%
                Xnorm[, , i] %*% R[, , i]
        }
        matidd <- t(sommetemp2) %*% invgC %*% sommetemp2
        lossf <- nbjuge - sum(diag(t(sommetemp2) %*% invgC %*%
            sommetemp2))
        lossf2 <- lossf
        if (scale) {
            matY <- matrix(0, nbjuge, nbjuge)
            B <- array(0, c(p, nbcolonne, nbjuge))
            for (k in 1:nbjuge) {
                B[, , k] <- Cj[, , k] %*% Xnorm[, , k] %*% R[,
                  , k]
            }
            for (k in 1:nbjuge) {
                for (l in 1:nbjuge) {
                  matY[k, l] <- sum(diag(t(B[, , k]) %*% invgC %*%
                    B[, , l]))
                }
            }
            eigzou <- eigen(W12 %*% matY %*% W12)
            verifsigne <- sum(eigzou$vectors[, 1] < 0)
            tailleeig <- dim(eigzou$vectors)[[1]]
            if (verifsigne == tailleeig) {
                vecteurpropre <- eigzou$vectors[, 1] * (-1)
            }
            else {
                vecteurpropre <- eigzou$vectors[, 1]
            }
            verifsigne <- NULL
            tailleeig <- NULL
            pds <- (nbjuge)^0.5 * W12 %*% as.matrix(vecteurpropre)
            sommetemp2 <- pds[1] * Cj[, , 1] %*% Xnorm[, , 1] %*%
                R[, , 1]
            for (i in 2:nbjuge) {
                sommetemp2 <- sommetemp2 + pds[i] * Cj[, , i] %*%
                  Xnorm[, , i] %*% R[, , i]
            }
            lossf2 <- (nbjuge - sum(diag(t(sommetemp2) %*% invgC %*%
                sommetemp2)))
        }
        tol <- lossf2
        itorth <- lossf
        itpoids <- lossf2
        lossf <- lossf2
        compteur <- 0

        while (tol > tolerance && compteur < nbiteration) {
            for (j in 1:nbjuge) {
                sommetemp <- pds[1] * Cj[, , 1] %*% Xnorm[, ,
                  1] %*% R[, , 1]
                for (i in 2:nbjuge) {
                  sommetemp <- sommetemp + pds[i] * Cj[, , i] %*%
                    Xnorm[, , i] %*% R[, , i]
                                }
                R[, , j] <- procrustesbis(Cj[, , j] %*% Xnorm[,
                  , j], invgC %*% (sommetemp - pds[j] * Cj[,
                  , j] %*% Xnorm[, , j] %*% R[, , j]))$H
                              }
            sommetemp2 <- NULL
            sommetemp2 <- pds[1] * Cj[, , 1] %*% Xnorm[, , 1] %*%
                R[, , 1]
            for (i in 2:nbjuge) {
                sommetemp2 <- sommetemp2 + pds[i] * Cj[, , i] %*%
                  Xnorm[, , i] %*% R[, , i]
                              }
            matidd <- t(sommetemp2) %*% invgC %*% sommetemp2
            lossf2ortho <- nbjuge - sum(diag(t(sommetemp2) %*%
                invgC %*% sommetemp2))
            lossf2 <- lossf2ortho
            lossfpoids <- 0
            if (scale) {
                matY <- matrix(0, nbjuge, nbjuge)
                B <- array(0, c(p, nbcolonne, nbjuge))
                for (k in 1:nbjuge) {
                  B[, , k] <- Cj[, , k] %*% Xnorm[, , k] %*%
                    R[, , k]
                }
                for (k in 1:nbjuge) {
                  for (l in 1:nbjuge) {
                    matY[k, l] <- sum(diag(t(B[, , k]) %*% invgC %*%
                      B[, , l]))
                  }
                }
                eigzou <- eigen(W12 %*% matY %*% W12)
                if (sum(eigzou$vectors[, 1] < 0) == dim(eigzou$vectors)[[1]]) {
                  vecteurpropre <- -eigzou$vectors[, 1]
                }
                else {
                  vecteurpropre <- eigzou$vectors[, 1]
                }
                pds <- (nbjuge)^0.5 * W12 %*% as.matrix(vecteurpropre)
                sommetemp2 <- pds[1] * Cj[, , 1] %*% Xnorm[,
                  , 1] %*% R[, , 1]
                for (i in 2:nbjuge) {
                  sommetemp2 <- sommetemp2 + pds[i] * Cj[, ,
                    i] %*% Xnorm[, , i] %*% R[, , i]
                }
                lossfpoids <- (nbjuge - sum(diag(t(sommetemp2) %*%
                  invgC %*% sommetemp2)))
                lossf2 <- (nbjuge - sum(diag(t(sommetemp2) %*%
                  invgC %*% sommetemp2)))
            }

            tol <- (lossf - lossf2)
            lossf <- lossf2
            itorth <- c(itorth, lossf2ortho)
            itpoids <- c(itpoids, lossfpoids)
            compteur <- compteur + 1
            }
        sommetemp2 <- NULL
        sommetemp2 <- pds[1] * Cj[, , 1] %*% Xnorm[, , 1] %*%
            R[, , 1]
        for (i in 2:nbjuge) {
            sommetemp2 <- sommetemp2 + pds[i] * Cj[, , i] %*%
                Xnorm[, , i] %*% R[, , i]
                            }
        pp <- invgC %*% sommetemp2
        translation <- matrix(0, nbcolonne, nbjuge)
        for (i in 1:nbjuge) {
            translation[, i] <- (t(pds[i] * Xnorm[, , i] - pp %*%
                t(R[, , i])) %*% M[, , i] %*% U)/as.numeric(pds[i] *
                t(U) %*% M[, , i] %*% U)
                             }
        ppeig <- eigen(t(pp) %*% Cc %*% pp)
        Xfin <- Xnorm
        for (k in 1:nbjuge) {
            Xfin[, , k] <- pds[k] * M[, , k] %*% (Xnorm[, , k] -
                U %*% t(translation[, k])) %*% R[, , k] %*% as.matrix(ppeig$vectors)
                            }

        it <- cbind(itorth, itpoids)
        colnames(it) <- c("rotation step", "scaling step")
        consensus <- pp %*% as.matrix(ppeig$vectors)
        cte <- 1
       
        while (cte <= dim(consensus)[[2]]) {
            if (all(abs(consensus[, cte]) < sqrt(.Machine$double.eps))) {
                fina <- cte - 1
                cte = dim(consensus)[[2]] + 1
            }
            else {
                fina <- cte
                cte <- cte + 1
            }

        }

        row.names(consensus) <- row.names(df)
       colnames(consensus,do.NULL = FALSE, prefix = "dim")
      
      
        
        colnames(Xfin)<-paste("dim",1:dim(Xfin)[[2]],sep=".")
        row.names(Xfin) <- row.names(df)
                  
        result <- list()
        class(result) <- c("GPAc", "list")
        result$depart <- df
        result$name.group <- name.group
        result$M <- M
        result$Cj <- Cj
        result$consensus <- consensus[, 1:fina]
        result$Z <- pp
        result$Xfin <- Xfin[, 1:fina, ]
        result$Xdeb <- Xnorm
        result$poids <- pds
        result$translation <- translation
        result$it <- it
        result$Rj <- R
        result$K <- as.matrix(ppeig$vectors)
        result$gama <- as.matrix(diag(ppeig$values))
        result$VMQTE <- VMQTE
        return(result)
    }
    crit <- function(Ys, MATRICEFIN) {
        s1 <- 0
        nbj <- dim(MATRICEFIN)[[3]]
        for (i in 1:nbj) {
            ai <- colSums((MATRICEFIN[, , i] - Ys)^2)
            s1 <- s1 + sum(ai)
        }
        return(s1)
    }
    f1ter <- function(tab, P = 5, scal, df, name.group, vm, tol) {
        nbj <- dim(tab)[[3]]
        permutation <- NULL
        for (i in 1:P) {
            permutation <- rbind(permutation, sample(nbj))
        }
        listresult <- list()
        length(listresult) <- 3 * P
        critfin <- NULL
        for (i in 1:P) {
            permutcolonne <- NULL
            for (colonne in 1:nbj) {
                permutcolonne <- rbind(permutcolonne, sample(dim(tab)[[2]]))
            }
            listesigne <- list()
            length(listesigne) <- nbj
            compteur <- 1
            while (compteur <= nbj) {
                topn2 <- sample(seq(0, dim(tab)[[2]], 1), 1)
                if (topn2 == 0)
                  compteur <- compteur + 1
                if (topn2 != 0) {
                  listesigne[[compteur]] <- sample(dim(tab)[[2]],
                    topn2)
                  compteur <- compteur + 1
                }
            }
            organisation <- sample(1:3)
            if (organisation[[1]] == 1) {
                tempo <- tab[, , permutation[i, ]]
                if (organisation[[2]] == 2) {
                  tempo2 <- permutcolonneX(tempo, permutcolonne)
                  tempo3 <- changesignecolonneX(tempo2, listesigne)
                }
                else {
                  tempo2 <- changesignecolonneX(tempo, listesigne)
                  tempo3 <- permutcolonneX(tempo2, permutcolonne)
                }
            }
            else {
                if (organisation[[1]] == 2) {
                  tempo <- permutcolonneX(tab, permutcolonne)
                  if (organisation[[2]] == 1) {
                    tempo2 <- tempo[, , permutation[i, ]]
                    tempo3 <- changesignecolonneX(tempo2, listesigne)
                  }
                  else {
                    tempo2 <- changesignecolonneX(tempo, listesigne)
                    tempo3 <- tempo2[, , permutation[i, ]]
                  }
                }
                else {
                  tempo <- changesignecolonneX(tab, listesigne)
                  if (organisation[[2]] == 1) {
                    tempo2 <- tempo[, , permutation[i, ]]
                    tempo3 <- permutcolonneX(tempo2, permutcolonne)
                  }
                  else {
                    tempo2 <- permutcolonneX(tempo, permutcolonne)
                    tempo3 <- tempo2[, , permutation[i, ]]
                  }
                }
            }
            listresult[[i]] <- algogpa(tempo3, scale = scal,
                df = df, name.group = name.group, tolerance = tol)
             
            if (vm) {
                tableau <- crit.procGPAcvmqte(gpafin)$objet
                crit1 <- tableau[dim(tableau)[[1]], 2] * 100/tableau[dim(tableau)[[1]],
                  3]
            }
            else {
                crit1 <- crit(listresult[[i]]$consensus, listresult[[i]]$Xfin)
            }
            critfin <- c(critfin, crit1)
        }
        ord1 <- order(critfin)
        gpafin <- listresult[[ord1[1]]]
        return(list(gpafin = gpafin, permutab = permutation[ord1[[1]],
            ]))
    }
    permutcolonneX <- function(X, permutcolonne) {
        Xfin <- X
        for (i in 1:dim(X)[[3]]) {
            Xfin[, , i] <- as.matrix(X[, , i][, permutcolonne[i,
                ]])
        }
        return(Xfin)
    }
    changesignecolonneX <- function(X, chgesigne) {
        Xfin <- X
        for (i in 1:dim(X)[[3]]) {
            if (is.null(chgesigne[[i]]))
                Xfin[, , i] <- X[, , i]
            else {
                Xfin[, chgesigne[[i]], i] <- -X[, chgesigne[[i]],
                  i]
            }
        }
        return(Xfin)
    }
    calibre <- function(X) {
        if (!is.array(X))
            stop("On souhaite ici avoir un tableau de type array  ")
        nbj <- dim(X)[[3]]
        dimlist <- NULL
        mattravlist <- list()
        for (i in 1:nbj) {
            if (length(placevm(X[, , i])$vect) == 0) {
                res.pca <- PCA(as.matrix(X[, , i]), scale.unit = FALSE,
                  graph = FALSE, ncp = min(dim(X[, , i])[[1]],
                    dim(X[, , i])[[2]]))
                mattravlist[[i]] <- as.matrix(res.pca$ind$coord)
                dimlist <- c(dimlist, dim(mattravlist[[i]])[[2]])
            }
            else {
                res.pca <- PCA(as.matrix(X[-(placevm(X[, , i])$vect),
                  , i]), scale.unit = FALSE, graph = FALSE, ncp = min(dim(X[,
                  , i])[[1]], dim(X[, , i])[[2]]))
                temp <- matrix(0, dim(X)[[1]], dim(as.matrix(res.pca$ind$coord))[[2]])
                temp[-placevm(X[, , i])$vect, ] <- as.matrix(res.pca$ind$coord)
                mattravlist[[i]] <- temp
                dimlist <- c(dimlist, dim(mattravlist[[i]])[[2]])
            }
        }
        mattrav <- array(0, c(dim(X)[[1]], max(dimlist), nbj))
        for (i in 1:nbj) {
            mattrav[, 1:dimlist[[i]], i] <- mattravlist[[i]]
        }
        tabfin <- mattrav
        for (i in 1:nbj) {
            if (length(placevm(X[, , i])$vect) == 0) {
                mattrav[, , i] <- tabfin[, , i]
            }
            else {
                tabfin[placevm(X[, , i])$vect, , i] <- NA
                mattrav[, , i] <- tabfin[, , i]
            }
        }
        return(mattrav)
    }
    if (is.null(name.group))
        name.group <- paste("group", c(1:length(group)), sep = ".")
    if (!is.data.frame(df))
        stop("df is not a data.frame")
    blo <- group
    nbjuge <- length(blo)
    X <- array(0, c(nrow(df), max(blo), nbjuge))

    dimnames(X) <- list(rownames(df), 1:max(blo), name.group)

    indice <- 0
    for (i in 1:nbjuge) {
        X[, 1:blo[i], i] <- as.matrix(df[, (indice + 1):(indice +
            blo[i])])
        indice <- indice + blo[i]
    }
    Xdd <- X

    X1 <- calibre(X)
     
    gpafin <- algogpa(X1, df = df, name.group = name.group)

    gpares <- f1ter(X1, P = 5, scal = scale, tol = tolerance,
        df = df, name.group = name.group, vm = gpafin$VMQTE)
    odd <- order(gpares$permutab)
    x <- gpares$gpafin
    gpafin <- x
    gpafin$translation <- x$translation[, odd]
    gpafin$M <- (x$M)[, , odd]
    gpafin$Cj <- (x$Cj)[, , odd]
    gpafin$Xfin <- x$Xfin[, , odd]
    gpafin$Xdeb <- x$Xdeb[, , odd]
    gpafin$Rj <- x$Rj[, , odd]
    gpafin$poids <- as.matrix(x$poids[odd])
    Xdep <- calibre(X)
    row.names(Xdep) <- row.names(df)
    colnames(Xdep) <- c(paste("dim", 1:dim(Xdep)[[2]]))
    x <- gpafin
    RVs <- matrix(-1, nbjuge, nbjuge)
    RV <- matrix(-1, nbjuge, nbjuge)
    sim <- matrix(-1, nbjuge, nbjuge)
    if (x$VMQTE) {
        vmplacelist <- list()
        length(vmplacelist) <- nbjuge
        for (theta in 1:nbjuge) {
            if (length(placevm(Xdd[, , theta])$vect) != 0) {
                vmplacelist[[theta]] <- placevm(Xdd[, , theta])$vect
            }
        }
        for (i in 1:nbjuge) {
            for (j in i:nbjuge) {
                if (length(c(vmplacelist[[i]], vmplacelist[[j]])) !=
                  0) {
                  Xi <- Xdd[, , i][-c(vmplacelist[[i]], vmplacelist[[j]]),
                    ]
                  Xj <- Xdd[, , j][-c(vmplacelist[[i]], vmplacelist[[j]]),
                    ]
                  if (is.null(dim(Xi))) {
                    Xi <- t(as.matrix(Xdd[, , i][-c(vmplacelist[[i]],
                      vmplacelist[[j]]), ]))
                    Xj <- t(as.matrix(Xdd[, , j][-c(vmplacelist[[i]],
                      vmplacelist[[j]]), ]))
                  }
                }
                if (length(c(vmplacelist[[i]], vmplacelist[[j]])) ==
                  0) {
                  Xi <- Xdd[, , i]
                  Xj <- Xdd[, , j]
                }
                RVs[i, j] <- RVs[j, i] <- coeffRV(Xi, Xj)$rvstd
                RV[i, j] <- RV[j, i] <- coeffRV(Xi, Xj)$rv
                sim[i, j] <- sim[j, i] <- similarite(Xi, Xj)
            }
        }
    }
    else {
        for (i in 1:nbjuge) {
            for (j in i:nbjuge) {
                Xi <- Xdd[, , i]
                Xj <- Xdd[, , j]
                RVs[i, j] <- RVs[j, i] <- coeffRV(Xi, Xj)$rvstd
                RV[i, j] <- RV[j, i] <- coeffRV(Xi, Xj)$rv
                sim[i, j] <- sim[j, i] <- similarite(Xi, Xj)
            }
        }
    }
    row.names(RV) <- colnames(RV) <- name.group
    row.names(RVs) <- colnames(RVs) <- name.group
    row.names(sim) <- colnames(sim) <- name.group
    listcor <- list()
    namelist <- NULL
    for (i in 1:dim(x$Xfin)[[3]]) {
        namelist <- c(namelist, paste("cor", name.group[[i]]))
        listcor[[i]] <- cor(scale(Xdd[, 1:blo[[i]], i], scale = FALSE),
            x$consensus, use = "pairwise.complete.obs")
    }
    listdimblo <- NULL
    for (i in 1:length(blo)) {
        listdimblo <- c(listdimblo, (max(blo) - blo[[i]]))
    }
    if (max(round(listdimblo, 6)) <= 10^-5) {
        averagecor <- 0 * listcor[[1]]
        for (i in 1:dim(x$Xfin)[[3]]) {
            averagecor <- averagecor + listcor[[i]]
        }
        averagecor <- averagecor/dim(x$Xfin)[[3]]
        listcor[[(dim(x$Xfin)[[3]] + 1)]] <- averagecor
        namelist <- c(namelist, "averagecor")
        names(listcor) <- namelist
    }
    names(listcor) <- namelist
    Xfin <- x$Xfin
    if (x$VMQTE) {
        for (i in 1:dim(x$Xfin)[[3]]) {
            Xfin[vmplacelist[[i]], , i] <- NA
        }
    }
    resultat <- list()
    class(resultat) <- c("GPA", "list")
    resultat$RV <- RV
    resultat$RVs <- RVs
    resultat$simi <- sim
    resultat$scaling <- x$poids
    resultat$dep <- Xdep
    resultat$consensus <- x$consensus
    resultat$Xfin <- Xfin
    resultat$correlations <- listcor
    if (x$VMQTE)
        resultat$PANOVA <- crit.procGPAcvmqte(x)
    else resultat$PANOVA <- crit.procGPAcsansvm(x)
    if (graph)
        plot.GPA(resultat, axes = axes)
    return(resultat)
}
