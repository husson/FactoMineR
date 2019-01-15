    coeffRV <- function(X, Y) {
    
        multiply.vec <- function(vec) {
        k <- length(vec)
        rep(vec, rep(factorial(k - 1), k))
    }
    permute <- function(nbp) {
        if (nbp == 2) {
            perm <- rbind(c(1, 2), c(2, 1))
        }
        else {
            perm <- matrix(nrow = factorial(nbp), ncol = nbp)
            perm[, 1] <- multiply.vec(1:nbp)
            for (j in 2:(nbp - 1)) {
                restants <- apply(matrix(perm[seq(1, factorial(nbp),
                  factorial(nbp - j + 1)), 1:(j - 1)], ncol = j -
                  1, nrow = length(seq(1, factorial(nbp), factorial(nbp -
                  j + 1)))), 1, setdiff, x = 1:nbp)
                perm[, j] <- as.vector(apply(restants, 2, multiply.vec))
            }
            perm[, nbp] <- apply(perm[, 1:nbp - 1], 1, setdiff,
                x = 1:nbp)
        }
        perm
    }
    coefficientRV <- function(X, Y) {
        if (dim(X)[[1]] != dim(Y)[[1]])
            stop("no the same dimension for X and Y")
        if (dim(X)[[1]] == 1) {
            print("1 configuration RV is  NA")
            rv = NA
        }
        else {
            Y <- scale(Y, scale = FALSE)
            X <- scale(X, scale = FALSE)
            W1 <- X %*% t(X)
            W2 <- Y %*% t(Y)
            rv <- sum(diag(W1 %*% W2))/(sum(diag(W1 %*% W1)) *
                sum(diag(W2 %*% W2)))^0.5
        }
        return(rv)
    }
 asym=function(X,Y){

        n <- dim(X)[[1]]
        X <- scale(X, scale = FALSE)
        Y <- scale(Y, scale = FALSE)

       S3=sum(diag(X %*% t(X))^3)    #S3OK
       S3etoile= sum(diag(Y %*% t(Y))^3)

       U=sum((X %*% t(X))^3)    #U OK
       Uetoile=sum((Y %*% t(Y))^3)

       B= t(diag(X %*% t(X))) %*%  (X %*% t(X)) %*% diag(X %*% t(X))      #B OK
       Betoile= t(diag(Y %*% t(Y))) %*%  (Y %*% t(Y)) %*% diag(Y %*% t(Y))

       R=t(diag( X %*% t(X)))%*%    diag(X %*%t(X) %*% X %*% t(X))       #R OK
       Retoile= t(diag( Y %*% t(Y)))%*%    diag(Y %*%t(Y) %*% Y %*% t(Y))

       TT=sum(diag(  X %*% t(X)))         #TT OK
       Tetoile=   sum(diag(  Y %*% t(Y)))

       S2= sum(diag(X %*% t(X))^2)         #S2OK
       S2etoile= sum(diag(Y %*% t(Y))^2)

       T2= sum(diag(X %*%t(X) %*% X %*% t(X) ))
       T2etoile=sum(diag(Y %*%t(Y) %*% Y %*% t(Y) ))

       T3=sum(diag(X %*%t(X) %*% X %*% t(X) %*% X %*% t(X) ))   #OH
       T3etoile=sum(diag(Y %*%t(Y) %*% Y %*% t(Y)%*% Y %*% t(Y) ))

        total  = n^2*(n+1)*(n^2+15*n-4)* S3  *  S3etoile   +
          4*(n^4-8*n^3+19*n^2-4*n-16)*U  * Uetoile  +
          24*(n^2-n-4)*(U*Betoile[,1] + B[,1]*Uetoile)+
          6*(n^4-8*n^3+21*n^2-6*n-24)* B[,1]*Betoile[,1]+
          12*(n^4-n^3-8*n^2+36*n-48)* R[,1]*Retoile[,1]+
          12*(n^3-2*n^2+9*n-12)  *(TT*S2*Retoile[,1]+R[,1]*Tetoile*S2etoile) +

          3*(n^4-4*n^3-2*n^2+9*n-12)*(TT*Tetoile*S2*S2etoile )  +
          24*( (n^3-3*n^2-2*n+8)*(R[,1]*Uetoile+U*Retoile[,1])+(n^3-2*n^2-3*n+12)*
          (R[,1]*Betoile[,1]+B[,1]*Retoile[,1]))+
          12*(n^2-n+4)*(TT*S2*Uetoile+U*Tetoile*S2etoile)+
         6*(2*n^3-7*n^2-3*n+12)*(TT*S2*Betoile[,1]+B[,1]*Tetoile*S2etoile) -

          2*n*(n-1)*(n^2-n+4)*((2*U+3*B[,1])*S3etoile+(2*Uetoile+3*Betoile[,1])*S3)-

          3*n*((n-1)^2) * (n+4)*((TT*S2+4*R[,1])*S3etoile+(Tetoile*S2etoile+4*Retoile[,1])*S3)+

          2*n*(n-1)*(n-2)*( (TT^3+6*TT*T2+8*T3)*S3etoile +
          (Tetoile^3+6*Tetoile*T2etoile+8*T3etoile)*S3)  +
          TT^3*((n^3-9*n^2+23*n-14)*Tetoile^3+ 6*(n-4)*Tetoile*T2etoile+8*T3etoile)+
          6*TT*T2*((n-4)*Tetoile^3+(n^3-9*n^2+24*n-14)*Tetoile*T2etoile+4*(n-3)*T3etoile)+
          8*T3*(Tetoile^3+3*(n-3)*Tetoile*T2etoile+(n^3-9*n^2+26*n-22)*T3etoile)  -
          16*(TT^3*Uetoile+U*Tetoile^3)-6*(TT*T2*Uetoile+U*Tetoile*T2etoile)*(2*n^2-10*n+16)-
          8*(T3*Uetoile+U*T3etoile)*(3*n^2-15*n+16)-(TT^3*Betoile[,1]+B[,1]*Tetoile^3)*(6*n^2-30*n+24)-6*(TT*T2*Betoile[,1]+B[,1]*Tetoile*T2etoile)*(4*n^2-20*n+24)-
          8*(T3*Betoile[,1]+B[,1]* T3etoile)*(3*n^2-15*n+24)   -
           (n-2)*(24*(TT^3*Retoile[,1]+R[,1]*Tetoile^3)+6*(TT*T2*Retoile[,1]+R[,1]*Tetoile*T2etoile)*(2*n^2-10*n+24)+
           8*(T3*Retoile[,1]+R[,1]*T3etoile)*(3*n^2-15*n+24)+(3*n^2-15*n+6)*(TT^3*Tetoile*S2etoile+TT*S2*Tetoile^3)+
           6*(TT*T2*Tetoile*S2etoile+TT*S2*Tetoile*T2etoile)*(n^2-5*n+6)+
           48*(T3*Tetoile*S2etoile+TT*S2*T3etoile))

           esperancet3=total/(n*(n-1)*(n-2)*(n-3)*(n-4)*(n-5) )

           esperance=TT*Tetoile/(n-1)
           variance= (2*((n-1)*T2-TT^2)*((n-1)*T2etoile-Tetoile^2)/(((n-1)^2)*(n+1)*(n-2))) + (n*(n+1)*S2-(n-1)*(TT^2+2*T2))*(n*(n+1)*S2etoile-(n-1)*(Tetoile^2+2*T2etoile))/((n+1)*n*(n-1)*(n-2)*(n-3))


           cumulant3=esperancet3-3*esperance *variance-esperance^3
           asym=cumulant3/(variance^(3/2) )
           return (asym=asym)
          }
          
######### Begin program###
        if (dim(X)[[1]] != dim(Y)[[1]]) stop("not the same dimension for X and Y")
        n <- dim(X)[[1]]
        Y <- scale(Y, scale = FALSE)
        X <- scale(X, scale = FALSE)
        rv <- coefficientRV(X, Y)
#        if (n < 4) {
        if (n < 6) {
            if (n == 1) {
                rvstd = NA
                esperance <- variance <- NA
            }
            else {
                perm24 <- permute(n)
                listX <- array(0, c(dim(X)[[1]], dim(X)[[2]],
                  dim(perm24)[[1]]))
                for (i in 1:dim(perm24)[[1]]) {
                  listX[, , i] <- scale(X[perm24[i, ], ], scale = FALSE)
                }
                listRV <- NULL
                listRVbis <- NULL
                for (i in 1:dim(perm24)[[1]]) {
                  listRV <- c(listRV, coefficientRV(listX[, , i], Y))
                }
                esperance <- mean(listRV)
                variance <- sum((listRV - esperance)^2)/dim(perm24)[[1]]

            return(list(rv = rv, rvstd = (rv - esperance)/variance^0.5, mean = esperance,
            variance = variance, skewness = NA, p.value =  sum(rv<listRV)/dim(perm24)[[1]] ))
            }
        }
        else {
            betax <- (sum(diag(X %*% t(X))))^2/sum(diag(X %*%
                t(X) %*% X %*% t(X)))
            betay <- (sum(diag(Y %*% t(Y))))^2/sum(diag(Y %*%
                t(Y) %*% Y %*% t(Y)))
            alphax <- n - 1 - betax
            alphay <- n - 1 - betay
            deltax <- sum(diag(X %*% t(X))^2)/sum(diag(X %*%
                t(X) %*% X %*% t(X)))
            gammax <- (n - 1) * (n * (n + 1) * deltax - (n -
                1) * (betax + 2))/((n - 3) * (n - 1 - betax))
            deltay <- sum(diag(Y %*% t(Y))^2)/sum(diag(Y %*%
                t(Y) %*% Y %*% t(Y)))
            gammay <- (n - 1)/((n - 3) * (n - 1 - betay)) * (n *
                (n + 1) * deltay - (n - 1) * (betay + 2))
            esperance <- (betax^0.5) * betay^0.5/(n - 1)
            variance <- 2 * alphay * alphax/((n + 1) * (n - 1)^2 *
                (n - 2)) * (1 + (n - 3) * gammax * gammay/(2 *
                n * (n - 1)))
        }
        rvstd <- (rv - esperance)/variance^0.5
        a <- asym(X,Y)
        if (a>=0) prob <- pgamma(rvstd-(-2/a),shape=(4/a^2),scale=(a/2),lower.tail=FALSE)
        if (a<0) prob = pgamma(a/abs(a)*rvstd+2/abs(a),shape=(4/a^2),scale=(abs(a)/2))

        return(list(rv = rv, rvstd = rvstd, mean = esperance,
            variance = variance, skewness = a, p.value = prob))
    }
