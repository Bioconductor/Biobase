/* Copyright R. Gentleman, 2003 */

#include <Rinternals.h>
#include <R_ext/RConverters.h>


/* 
   speed up the conversion, this could become as.environment at some
   time
   we might want to check the names though

*/

SEXP listLen(SEXP x)
{
  SEXP ans;
  int i;

  if( !Rf_isNewList(x) )
    error("require a list");

  PROTECT(ans = allocVector(REALSXP, length(x)));

  for(i=0; i<length(x); i++)
    REAL(ans)[i] = length(VECTOR_ELT(x, i));
  UNPROTECT(1);
  return(ans);
}


SEXP listToEnv(SEXP x, SEXP env)
{
  SEXP name, nm, s;
  int i;

  if( !Rf_isNewList(x) )
    error("require a list");

  if( !isEnvironment(env) )
    error("second argument must be an environment");


  PROTECT(nm = getAttrib(x, R_NamesSymbol));
  if( length(nm) != length(x) )
    error("all elements must have names");


  for( i=0; i<length(nm); i++) {
    name = Rf_install(CHAR(STRING_ELT(nm, i)));
    Rf_defineVar(name, VECTOR_ELT(x, i), env);
  }

  UNPROTECT(1);
  return(env);
}
