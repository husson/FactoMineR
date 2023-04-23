#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>  // optional
#include <R.h>
#include <Rinternals.h>
#include "disjonctif.h"


#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  CALLDEF(disjoVar, 2),
  CALLDEF(disjoVarProp, 3),
   CALLDEF(disjoMatProp, 4),
   CALLDEF(disjoMat, 3),
    {NULL, NULL, 0}
};

void
attribute_visible  // optional
R_init_disjo(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
