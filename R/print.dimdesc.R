print.dimdesc<-function(x, ...){
  if (!inherits(x, "dimdesc")) 
    stop("need to be a dimdesc object")
  print(x[-length(x)])
}