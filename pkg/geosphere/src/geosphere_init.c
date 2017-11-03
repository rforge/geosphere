#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP geodesic(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP inversegeodesic(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP polygonarea(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"geodesic",        (DL_FUNC) &geodesic,        6},
    {"inversegeodesic", (DL_FUNC) &inversegeodesic, 6},
    {"polygonarea",     (DL_FUNC) &polygonarea,     4},
    {NULL, NULL, 0}
};

void R_init_geosphere(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
