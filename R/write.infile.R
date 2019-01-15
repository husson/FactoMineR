    write.infile <- function(X, file, sep=";", append = FALSE,nb.dec=4) {
        if (!append) cat("", file = file, append = FALSE,sep="")
        if (inherits(X, "array")) affichetableau(X, file, sep, nb.dec=nb.dec)
        else if (is.matrix(X)) affichmatrice(X, file, sep,nb.dec=nb.dec)
        else if (is.data.frame(X)) affichtabldon(X, file, sep,nb.dec=nb.dec)
        else if (is.list(X)) affichlist(X, file,sep=sep,nb.dec=nb.dec)
        else if (is.numeric(X)) cat(X, "\n", file = file, append = TRUE,sep=sep)
        else if (is.character(X)) cat(X, "\n", file = file, append = TRUE,sep=sep)
        else if (is.vector(X)) cat(X, "\n", file = file, append = TRUE,sep=sep)
        else {
            cat("format non affichable", "\n", file = file, append = TRUE,sep="")
            cat(X, "\n", file = file, append = TRUE,sep="")
        }
    }
    
    affichetableau <- function(X, file, sep,nb.dec=4) {
        X <- round(X, nb.dec)
        for (j in 1:(dim(X)[[3]])) {
            cat("sous tableau", j, "\n", file = file, append = TRUE,sep="")
            affichmatrice(X[, , j], file, sep=sep,nb.dec=nb.dec)
        }
        cat("\n", file = file, append = TRUE)
    }
    affichtabldon <- function(X, file, sep,nb.dec=nb.dec) {
        nomligne <- labels(X)[[1]]
        nomcol <- labels(X)[[2]]
        cat("  ", nomcol, "\n", file = file, append = TRUE, sep = sep)
        alpha <- dim(X)[[1]]
        for (i in 1:alpha) {
            deb <- nomligne[i]
            for (j in 1:(dim(X)[[2]])) deb <- c(deb, sep, X[i, j])
            cat(deb, "\n", file = file, append = TRUE,sep="")
        }
        cat("\n", file = file, append = TRUE,sep="")
    }
    affichmatrice <- function(X, file, sep,nb.dec=4) {
        X <- round(X, nb.dec)
        nomligne <- rownames(X)
        nomcol <- colnames(X)
        cat("   ",nomcol, "\n", file = file, append = TRUE, sep = sep)
        alpha <- dim(X)[[1]]
        for (i in 1:alpha) cat(nomligne[i], X[i, ], "\n", file = file, append = TRUE, sep=sep)
        cat("\n", file = file, append = TRUE,sep="")
    }
    affichlist <- function(X, file,sep,nb.dec=4) {
        taillelist <- length(X)
        noms <- labels(X)
        for (i in 1:taillelist) {
            if (noms[i]!= "call"){
              cat(noms[i], "\n", file = file, append = TRUE,sep="")
              write.infile(X[[i]], file, sep=sep, append = TRUE,nb.dec=nb.dec)
            }
        }
    }
