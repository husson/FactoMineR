#include <R.h>
#include <Rinternals.h>
#include "disjonctif.h"

/** \fn disjoVar
 *  \brief disjonctif 1 variable
 *
 * \param  rfact: vector int [1:n] of int (between 1 and niveaux)
 * \param  rparamint vector int
 *           - [0] = n (number of observations)
 *           - [1] = niveaux (number of levels)
 * \return rans : matrix n x niveaux of int
* ******************************************************************/

SEXP  disjoVar(SEXP rfact, SEXP rparamint)
{
  size_t i, j,  n, niveaux;
  int *fact, *paramint, *ans, jp1;

  fact= INTEGER(rfact);
  paramint= INTEGER(rparamint);
  n = (size_t) paramint[0];
  niveaux = (size_t) paramint[1];
  /*********************************************************************/
  /* output to R */
  /*********************************************************************/
  SEXP rans = PROTECT(allocMatrix(INTSXP, n, niveaux));
  ans = INTEGER(rans);
  /*********************************************************************/
  /* loop */
  /*********************************************************************/
  jp1=1; /* jp1 = j+1 */
  for (j=0 ; j<niveaux; j++) {
    for (i=0 ; i<n; i++) {
      if(fact[i]==NA_INTEGER) {
        ans[i + n*j] = NA_INTEGER;
      } else if(fact[i]==jp1) {
        ans[i + n*j] = 1;
      } else {
        ans[i + n*j] =  0 ;
      }
    }
    jp1++;
  }
  UNPROTECT(1); /* end */
  return rans;
}
/** \fn disjoMat
 *  \brief hot encoding of a matrix of categorical variables
 *
 * \param  rfact: vector int [1:n] of int (between 1 and niveaux)
 * \param  rparamint vector int
 *           - [0] = n (number of observations)
 *           - [1] = K (number of categorical variables)
 * \param  rniveaux vector int
 *           - [0] = nlevels of categorical variable 1
 *           - [1] = nlevels of categorical variable 2
 *           - [2] = nlevels of categorical variable 3
 *           - ...
 * \return rans : matrix n x Ktot of int
* ******************************************************************/
SEXP  disjoMat(SEXP rfact, SEXP rparamint, SEXP rniveaux)
{
  size_t i, j, jj, ctrcol, n, K, Ktot=0;
  int *fact, *paramint, *niveaux, *ans,  jp1;
  fact = INTEGER(rfact);
  paramint = INTEGER(rparamint);
  niveaux = INTEGER(rniveaux);
  n = (size_t) paramint[0];
  K = (size_t) paramint[1];
  /* sum of all nlevels */
  for (j=0 ; j<K; j++) {
    Ktot += (size_t) niveaux[j] ;
  }
  /*********************************************************************/
  /* output to R */
  /*********************************************************************/
  SEXP rans = PROTECT(allocMatrix(INTSXP, n, Ktot));
  ans = INTEGER(rans);
   /*********************************************************************/
  /* loop */
  /*********************************************************************/
  ctrcol=0;
  for (j=0 ; j<K; j++) {
    jp1=1;
    for (jj=0 ; jj<niveaux[j]; jj++) {
      for (i=0 ; i<n; i++) {
        if(fact[i+ n*j]==NA_INTEGER) {
          ans[i + n*ctrcol] = NA_INTEGER;
        } else if(fact[i+ n*j]==jp1) {
          ans[i + n*ctrcol] = 1;
        } else {
          ans[i + n*ctrcol] =  0 ;
        }
      }
      ctrcol++;
      jp1++;
    }
  }
  UNPROTECT(1); /* end */
  return rans;
}


SEXP  disjoVarProp(SEXP rfact, SEXP rparamint, SEXP rroww)
{
  size_t i, j,  n, niveaux;
  int *fact, *paramint, jp1;
  double  *roww, *ans, *percByLevels, nnonNA;
  fact= INTEGER(rfact);
  paramint= INTEGER(rparamint);
  roww = REAL(rroww);
  n = (size_t) paramint[0];
  niveaux = (size_t) paramint[1];
  /*********************************************************************/
  /* output to R */
  /*********************************************************************/
  SEXP rans = PROTECT(allocMatrix(REALSXP, n, niveaux));
  ans = REAL(rans);
  /*********************************************************************/
  /* percByLevels */
  /*********************************************************************/
  percByLevels = (double *) R_alloc(niveaux, sizeof(double));
  for (j=0 ; j<niveaux; j++) {
      percByLevels[j]=0;
    }
  nnonNA=0;
  /* freq by level */
  for (i=0 ; i<n; i++) {
    if (fact[i]!=NA_INTEGER) {
      percByLevels[fact[i]-1] += roww[i];
      nnonNA += roww[i];
    }
  }
  /* percentage by level */
  for (j=0 ; j<niveaux; j++) {
    percByLevels[j] /= nnonNA ;
  }
  /*********************************************************************/
  /* loop */
  /*********************************************************************/
  jp1=1; /* jp1 = j+1 */
  for (j=0 ; j<niveaux; j++) {
    for (i=0 ; i<n; i++) {
      if(fact[i]==NA_INTEGER) {
        ans[i + n*j] = percByLevels[j];
      } else if(fact[i]==jp1) {
        ans[i + n*j] = 1;
      } else {
        ans[i + n*j] =  0 ;
      }
    }
    jp1++;
  }
  UNPROTECT(1); /* end */
  return rans;
}
/** \fn disjoMatProp
 *  \brief hot encoding of a matrix of categorical variables
 *  (NA values replaced by percentage of non NA)
 *
 * \param  rfact: vector int [1:n] of int (between 1 and K)
 * \param  rparamint vector int
 *           - [0] = n (number of observations)
 *           - [1] = K (number of categorical variables)
 * \param  rniveaux vector int
 *           - [0] = nlevels of categorical variable 1
 *           - [1] = nlevels of categorical variable 2
 *           - [2] = nlevels of categorical variable 3
 *           - ...
 * \param  rroww vector [1:n] of double : weight of observations
 * \return rans : matrix n x Ktot of double
* ******************************************************************/

SEXP  disjoMatProp(SEXP rfact, SEXP rparamint, SEXP rniveaux, SEXP rroww)
{
  rfact = PROTECT(rfact);
  rparamint = PROTECT(rparamint);
  rniveaux = PROTECT(rniveaux);
  rroww = PROTECT(rroww);
  size_t i, j, jj, ctrcol=0, n, K, Ktot;
  int *fact, *paramint, *niveaux,   jp1;
  double *roww, *ans, *percByLevels, nnonNA ;
  fact = INTEGER(rfact);
  paramint = INTEGER(rparamint);
  niveaux = INTEGER(rniveaux);
  n = (size_t) paramint[0];
  K = (size_t) paramint[1];
  roww = REAL(rroww);
  /* sum of all nlevels */
  Ktot=0;
  for (j=0 ; j<K; j++) {
    Ktot += (size_t) niveaux[j] ;
  }
  /*********************************************************************/
  /* output to R */
  /*********************************************************************/
  SEXP rans = PROTECT(allocMatrix(REALSXP, n, Ktot));
  ans = REAL(rans);
  /*********************************************************************/
  /* percByLevels */
  /*********************************************************************/
  percByLevels = (double *) R_alloc(Ktot, sizeof(double));
  for (j=0 ; j<Ktot; j++) {
      percByLevels[j]=0;
    }
  for (j=0 ; j<K; j++) {
    nnonNA=0;
    /* sum of weights by level */
    for (i=0 ; i<n; i++) {
      if (fact[i+ n*j]!=NA_INTEGER) {
        percByLevels[fact[i+ n*j]-1 + ctrcol] += roww[i];
        nnonNA += roww[i];
      }
    }
    /* division  by the sum of weights (equal to 1 if no NA) */
    for (i=0 ; i<niveaux[j]; i++) {
      percByLevels[i + ctrcol] /= nnonNA ;
    }
    ctrcol += (size_t) niveaux[j] ;
  }
  /*********************************************************************/
  /* loop */
  /*********************************************************************/
  ctrcol=0;/* reset */
  for (j=0 ; j<K; j++) {
    jp1=1;
    for (jj=0 ; jj<niveaux[j]; jj++) {
      for (i=0 ; i<n; i++) {
        if(fact[i+ n*j]==NA_INTEGER) {
          ans[i + n*ctrcol] = percByLevels[ctrcol];
        } else if(fact[i+ n*j]==jp1) {
          ans[i + n*ctrcol] = 1;
        } else {
          ans[i + n*ctrcol] =  0 ;
        }
      }
      ctrcol++;
      jp1++;
    }
  }
  UNPROTECT(1); /* result */
  UNPROTECT(4); /* args */
  return rans;
}
