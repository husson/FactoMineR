.onLoad <-
function(libname,pkgname){
library.dynam("FactoMineR",pkgname,libname)
}

.onAttach <- function(libname, pkgname) {
  if (getRversion() < "4.5") {
    packageStartupMessage(
      "Note: This package is optimized and recommended for R >= 4.5.\n",
      "You are currently using R version ", getRversion(), "."
    )
  }
}