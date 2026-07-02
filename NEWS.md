# FactoMineR 2.16

* function theme_factominer
* theme_factominer used by default
* the svd.triplet function gives a new output sumvp for the sum of all the eigenvalues (not all the eigenvalues are given by default, but sumvp gives their sum)
* in functions PCA, CA, MCA, MFA, FAMD, the possibility to pass further arguments to or from other methods, such as the ones of plot.PCA, plot.CA, ...
* the catdes function gives html tables by default with the new argument html.table=TRUE

# FactoMineR 2.15

* The suggested partition in the function HCPC is the one that maximizes the Krzanowski-Lai index
* use of the irlba function of the irlba package to perform svd faster. Only the first eigenvalues are given, not all.
