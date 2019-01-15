print.CaGalt<-function (x, file = NULL, sep = ";", ...) 
{
	res.cagalt <- x
	if (!inherits(res.cagalt, "CaGalt")) stop("non convenient data")
	cat("**Results for the Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt)**\n")
    	cat("*The results are available in the following objects:\n\n")
    	res <- array("", c(15, 2), list(1:15, c("name", "description")))
   	res[1, ] <- c("$eig", "eigenvalues")
    	res[2, ] <- c("$ind", "results for the individuals")
    	res[3, ] <- c("$ind$coord", "coordinates for the individuals")
    	res[4, ] <- c("$ind$cos2", "cos2 for the individuals")
	res[5, ] <- c("$freq", "results for the frequencies")
    	res[6, ] <- c("$freq$coord", "coordinates for the frequencies")
    	res[7, ] <- c("$freq$cos2", "cos2 for the frequencies")
    	res[8, ] <- c("$freq$contrib", "contributions of the frequencies")
    	indice <- 9
    	if (!is.null(res.cagalt$quanti.var)) {
      	res[indice, ] <- c("$quanti.var", "results for the quantitative variables")
        	res[indice + 1, ] <- c("$quanti.var$coord", "coordinates for the quantitative variables")
        	res[indice + 2, ] <- c("$quanti.var$cor", "correlations between quantitative variables and dimensions")
        	res[indice + 3, ] <- c("$quanti.var$cos2", "cos2 for the quantitative variables")
		indice <- indice + 3
    	}
    	if (!is.null(res.cagalt$quali.var)) {
        	res[indice, ] <- c("$quali.var", "results for the categorical variables")
        	res[indice + 1, ] <- c("$quali.var$coord", "coordinates for the categories")
        	res[indice + 2, ] <- c("$quali.var$cos2", "cos2 for the categories")
        	indice <- indice + 2
	}
	if(!is.null(res.cagalt$ellip)) {
		res[indice + 1, ] <- c("$ellip", "coordinates to construct confidence ellipses")
    		res[indice + 2, ] <- c("$ellip$freq", "coordinates of the ellipses for the frequencies")
    		res[indice + 3, ] <- c("$ellip$var", "coordinates of the ellipses for the variables")
    		indice <- indice + 3
	}
    	print(res[1:indice, ])
    	if (!is.null(file)) {
        	write.infile(res.cagalt, file = file, sep = sep)
        	print(paste("All the results are in the file", file))
    	}
}