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
  SEXP name, nm, nx;
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
    PROTECT(nx = Rf_duplicate(VECTOR_ELT(x, i)));
    Rf_defineVar(name, nx, env);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return(env);
}


#ifdef ALLDONENOW
##looks like we need to either expose some of the ls functionality 
##or copy a lot of code - my preference is to expose the ls

## code looks a lot like do_ls; but we get the objects too...
## with a duplicate - I suppose -- otherwise it will be a problem....


SEXP envToList(SEXP env, SEXP allnames) {
  SEXP nms, ans;
  int i, k, n;

    if( !isEnvironment(env) )
        error("argument must be an environment");

    if( !isLogical(allnames) )
      error("second argument must be a logical");

    all = asLogical(allNames);
    if (all == NA_LOGICAL)
	all = 0;
    
    /* Step 1 : Compute the Vector Size */
    k = 0;
    n = length(env);
    for (i = 0; i < n; i++) {
	if (VECTOR_ELT(env, i) == R_NilValue)
	    k += BuiltinSize(all, 0);
	else if (isEnvironment(VECTOR_ELT(env, i))) {
	    if (HASHTAB(VECTOR_ELT(env, i)) != R_NilValue)
		k += HashTableSize(HASHTAB(VECTOR_ELT(env, i)), all);
	    else
		k += FrameSize(FRAME(VECTOR_ELT(env, i)), all);
	}
	else error("invalid envir= argument");
    }
    /* Step 2 : Allocate and Fill the Result */
    nms = allocVector(STRSXP, k);
    ans = allocVector(VECSXP, k);
    k = 0;
    for (i = 0; i < n; i++) {
      if (VECTOR_ELT(env, i) == R_NilValue) {
	    BuiltinNames(all, 0, nms, &k);
      }
	else if (isEnvironment(VECTOR_ELT(env, i))) {
	  if (HASHTAB(VECTOR_ELT(env, i)) != R_NilValue) {
		HashTableNames(HASHTAB(VECTOR_ELT(env, i)), all, nms,
    &k);
	  }
	  else {
		FrameNames(FRAME(VECTOR_ELT(env, i)), all, nms, &k);
	  }
	}
    }
    UNPROTECT(1);
    sortVector(ans, FALSE);
    return ans;
}
#endif
