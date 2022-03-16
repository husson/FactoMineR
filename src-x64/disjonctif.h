#ifndef DISJONCTIF_H_
#define DISJONCTIF_H
SEXP  disjoVar(SEXP rfact, SEXP rparamint) ;
SEXP  disjoVarProp(SEXP rfact, SEXP rparamint, SEXP rroww) ;
SEXP  disjoMat(SEXP rfact, SEXP rparamint, SEXP rniveaux) ;
SEXP  disjoMatProp(SEXP rfact, SEXP rparamint, SEXP rniveaux, SEXP rroww) ;
#endif // DISJONCTIF_H
