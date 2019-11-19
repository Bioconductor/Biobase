#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

SEXP anyMissing(SEXP);
SEXP rowQ(SEXP, SEXP);
SEXP rowMedians(SEXP, SEXP, SEXP, SEXP);
SEXP rowQuantiles(SEXP, SEXP);
SEXP unsafe_set_slot(SEXP obj, SEXP slot, SEXP value);
SEXP lc_prefix(SEXP, SEXP);
SEXP sublist_extract(SEXP, SEXP, SEXP, SEXP);

/* Automate using sed or something. */
#if _MSC_VER >= 1000
__declspec(dllexport)
#endif

    static const R_CallMethodDef R_CallDef[] = {
        {"anyMissing", (DL_FUNC)&anyMissing, 1},
        {"rowQ", (DL_FUNC)&rowQ, 2},
        {"rowMedians", (DL_FUNC)&rowMedians, 4},
        {"unsafe_set_slot", (DL_FUNC)&unsafe_set_slot, 3},
        {"lc_prefix", (DL_FUNC)&lc_prefix, 2},
        {"sublist_extract", (DL_FUNC)&sublist_extract, 4},
        {NULL, NULL, 0},
    };

void R_init_Biobase(DllInfo *info)
{
  R_registerRoutines(info,NULL,R_CallDef,NULL,NULL);
}

