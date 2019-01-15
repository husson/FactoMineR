simule <- function (data, nb.simul = 500){

    if (!inherits(data, "data.frame")) stop("data is not a data frame")
    if (!inherits(data[, 1], "factor")) stop("data[,1] is not a factor")
    nbre.fact <- nlevels(data[, 1])
    res.moy <- res.moy.sim <- as.data.frame(matrix(0, nbre.fact, ncol(data)))
    res.sim <- as.data.frame(matrix(0, nbre.fact * nb.simul, ncol(data)))
    colnames(res.moy) <- colnames(res.sim) <- colnames(res.moy.sim) <- colnames(data)
    for (f in 1:nbre.fact) {
      table <- data[which(data[, 1] == levels(data[, 1])[f]) ,]
      nb.aux <- length(which(data[, 1] == levels(data[, 1])[f]))
      table <- na.omit(table)
      tmp <- NULL
      for (j in 1:nb.simul) tmp <- rbind(tmp,apply(table[sample(1:nb.aux,nb.aux,replace=TRUE),-1], 2,mean))
      res.moy[f, 1] <- res.moy.sim[f, 1] <- as.character(levels(data[, 1])[f])
      res.moy[f, -1] <- apply(table[,-1],2,mean)
      res.sim[((f - 1) * nb.simul + 1):(f * nb.simul), 1] <- as.character(levels(data[, 1])[f])
      res.sim[((f - 1) * nb.simul + 1):(f * nb.simul), -1] <- tmp
      res.moy.sim[f, -1] <- apply(tmp, 2, mean)
    }
    res.moy[, 1] <- factor(res.moy[, 1])
    res.sim[, 1] <- factor(res.sim[, 1])
    res.moy.sim[, 1] <- factor(res.moy.sim[, 1])
    resultats <- list(mean = res.moy, simul = res.sim, simul.mean = res.moy.sim)
    class(resultats) <- c("sim", "list")
    return(resultats)
}
