#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/RConverters.h>
#include <R_ext/Rdynload.h>

SEXP listToEnv(SEXP, SEXP);

/* Automate using sed or something. */
#if _MSC_VER >= 1000
__declspec(dllexport)
#endif

    static const R_CallMethodDef R_CallDef[] = {
        {"listToEnv", (DL_FUNC)&listToEnv, 2},
        {NULL, NULL, 0},
    };

void R_init_Biobase(DllInfo *info)
{
  R_registerRoutines(info,NULL,R_CallDef,NULL,NULL);
}

