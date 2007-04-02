#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/RConverters.h>
#include <R_ext/Rdynload.h>

SEXP copyEnv(SEXP e1, SEXP e2, SEXP all);
SEXP listToEnv(SEXP, SEXP);
SEXP listLen(SEXP);
SEXP rowQ(SEXP, SEXP);
SEXP unsafe_set_slot(SEXP obj, SEXP slot, SEXP value);
SEXP lc_prefix(SEXP, SEXP);

/* Automate using sed or something. */
#if _MSC_VER >= 1000
__declspec(dllexport)
#endif

    static const R_CallMethodDef R_CallDef[] = {
        {"copyEnv", (DL_FUNC)&copyEnv, 3},
        {"listToEnv", (DL_FUNC)&listToEnv, 2},
	{"listLen", (DL_FUNC)&listLen, 1},
	{"rowQ", (DL_FUNC)&rowQ, 2},
	{"unsafe_set_slot", (DL_FUNC)&unsafe_set_slot, 3},
	{"lc_prefix", (DL_FUNC)&lc_prefix, 2},
        {NULL, NULL, 0},
    };

void R_init_Biobase(DllInfo *info)
{
  R_registerRoutines(info,NULL,R_CallDef,NULL,NULL);
}

