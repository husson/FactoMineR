summary.CaGalt<-function (object, nb.dec = 3, nbelements = 10, nbind = nbelements, ncp = 3, align.names = TRUE, file = "", ...) {
	print2 <- function(mat, file = "") {
		if (file == "") print(mat, quote = FALSE, right = TRUE)
		else {
			mat <- cbind(format(rownames(mat)), mat)
            	mat <- rbind(colnames(mat), mat)
            	mat2 <- cbind(format(mat[, 1], justify = "right"), format(mat[, 2], justify = "right"))
            	for (k in 3:ncol(mat)) mat2 <- cbind(mat2, format(mat[, k], justify = "right"))
            	mat2 <- cbind(mat2, "\n")
            	for (i in 1:nrow(mat2)) cat(mat2[i, ], file = file, append = TRUE)
		}
	}
	print3 <- function(obj, file = "", ncp, width.row = 0, nbelements = nbelements) {
		list.obj <- match.arg(names(obj), c("coord", "cos2", "contrib", "cor"), several.ok = TRUE)
        	nb.col <- sum(c("coord", "cos2", "contrib", "cor") %in% list.obj)
        	nbelements <- min(nbelements, nrow(obj$coord))
        	mat <- matrix(NA, nbelements, nb.col * ncp)
		if("contrib" %in% list.obj){
			obj$coord<-obj$coord[(rev(order(obj$contrib[, 1] + obj$contrib[, 2]))),]
			obj$cos2<-obj$cos2[(rev(order(obj$contrib[, 1] + obj$contrib[, 2]))),]
			obj$contrib<-obj$contrib[(rev(order(obj$contrib[, 1] + obj$contrib[, 2]))),]
        	}
		colnames(mat) <- paste("v", 1:ncol(mat))
        	rownames(mat) <- format(rownames(obj$coord)[1:nbelements, drop = FALSE], width = width.row)
        	indice <- 1
		if ("coord" %in% list.obj) {
            	mat[, indice + nb.col * (0:(ncp - 1))] <- obj$coord[1:nbelements, 1:ncp, drop = FALSE]
            	colnames(mat)[indice + nb.col * (0:(ncp - 1))] <- paste("Dim.", 1:ncp, sep = "")
            	indice <- indice + 1
        	}
		if ("cor" %in% list.obj) {
            	mat[, indice + nb.col * (0:(ncp - 1))] <- obj$cor[1:nbelements, 1:ncp, drop = FALSE]
            	colnames(mat)[indice + nb.col * (0:(ncp - 1))] <- paste("cor", 1:ncp, sep = "")
            	indice <- indice + 1
        	}
        	if ("contrib" %in% list.obj) {
            	mat[, indice + nb.col * (0:(ncp - 1))] <- obj$contrib[1:nbelements, 1:ncp, drop = FALSE]
            	colnames(mat)[indice + nb.col * (0:(ncp - 1))] <- "ctr"
            	indice <- indice + 1
        	}
        	if ("cos2" %in% list.obj) {
            	mat[, indice + nb.col * (0:(ncp - 1))] <- obj$cos2[1:nbelements, 1:ncp, drop = FALSE]
            	colnames(mat)[indice + nb.col * (0:(ncp - 1))] <- "cos2"
            	indice <- indice + 1
        	}
            mat <- format(round(mat, nb.dec))
     		mat2 <- "|"
        	for (k in 1:ncp) mat2 <- cbind(mat2, mat[, (1 + nb.col * (k - 1)):(nb.col * k), drop = FALSE], "|")
        	colnames(mat2)[1] <- ""
        	print2(as.matrix(mat2), file = file)
    	}
    	res <- object
    	if (!inherits(res, "CaGalt")) stop("non convenient object")
    	cat("\nEigenvalues\n", file = file, append = TRUE)
    	eige <- format(t(round(res$eig[, 1:3], nb.dec)), justify = "right")
    	rownames(eige) <- c("Variance", "% of var.", "Cumulative % of var.")
    	colnames(eige) <- paste("Dim", 1:ncol(eige), sep = ".")
    	print2(eige, file = file)
    	ncp <- min(res$call$ncp, ncp)
    	width.row <- 0
    	aux <- match.arg(names(res), c("ind", "freq", "quanti.var", "quali.var"), several.ok = TRUE)
    	if (align.names == TRUE) {
		width.row = max(nchar(rownames(res[aux[1]][[1]]$coord)))
        	for (k in 1:length(aux)) width.row = max(width.row, nchar(rownames(res[aux[k]][[1]]$coord)[1:min(nrow(res[aux[k]][[1]]$coord), nbelements)]))
    	}
    	if (nbind > 0) {
		cat("\nIndividuals", file = file, append = TRUE)
        	if (nrow(res$ind$coord) > nbind) cat(paste(" (the ", nbind, " first individuals)", sep = ""), file = file, append = TRUE)
        	cat("\n", file = file, append = TRUE)
        	print3(res$ind, file = file, ncp = ncp, width.row = width.row, nbelements = nbind)
	}
	if (nrow(res$freq$coord) > 1) cat("\nFrequencies", file = file, append = TRUE)
	else cat("\nFrequency", file = file, append = TRUE)
	if (nrow(res$freq$coord) > nbelements) cat(paste(" (the ", nbelements, " first most contributed frequencies on the first principal plane)", sep = ""), file = file, append = TRUE)
 	cat("\n", file = file, append = TRUE)
	print3(res$freq, file = file, ncp = ncp, width.row = width.row, nbelements = nbelements)
    	if (!is.null(res$quanti.var)) {
		cat("\nQuantitative variables", file = file, append = TRUE)
        	if (nrow(res$quanti.var$coord) > nbelements) cat(paste(" (the ", nbelements, " first)", sep = ""), file = file, append = TRUE)
        	cat("\n", file = file, append = TRUE)
        	print3(res$quanti.var, file = file, ncp = ncp, width.row = width.row, nbelements = nbelements)
    	}
    	if (!is.null(res$quali.var)) {
        	cat("\nCategorical variables", file = file, append = TRUE)
        	if (nrow(res$quali.var$coord) > nbelements) cat(paste(" (the ", nbelements, " first)", sep = ""), file = file, append = TRUE)
        	cat("\n", file = file, append = TRUE)
        	print3(res$quali.var, file = file, ncp = ncp, width.row = width.row, nbelements = nbelements)
    	}
}