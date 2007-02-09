/* Copyright R. Gentleman, 2003 */

#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/RConverters.h>
#include <R_ext/Utils.h>

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

/* fast computation of row order statistics */

SEXP rowQ(SEXP inmat, SEXP which)
{
  SEXP ans;
  int i, j,  nrow, ncol, medval;
  double *rowData;

  /* we can do integers later */
  if( !isMatrix(inmat) || !isReal(inmat) )
    error("argument must be a numeric matrix");

  /* FIXME: should check for FINITE/NA */
  if( !isNumeric(which) || length(which) != 1 )
    error("which must be numeric");

  /* subtract one here, since rPsort does zero based addressing*/
  medval = asInteger(which) - 1;

  PROTECT(ans = getAttrib(inmat, R_DimSymbol));
  nrow = INTEGER(ans)[0];
  ncol = INTEGER(ans)[1];

  /* sanity check */
  if( medval < 0 || medval >= ncol )
    error("which  is larger than the number of rows");

  PROTECT(ans = allocVector(REALSXP, nrow));
  
  rowData = (double *) R_alloc(ncol, sizeof(double));

  for( i=0; i<nrow; i++) {
    for(j=0; j<ncol; j++) 
      rowData[j] = REAL(inmat)[i+j*nrow];
    rPsort(rowData, ncol, medval);
    REAL(ans)[i] = rowData[medval];
  }
  UNPROTECT(2);
  return(ans);
}

SEXP unsafe_set_slot(SEXP obj, SEXP slot, SEXP value)
{
    /* Set an S4 object's slot without copying.

       This is unsafe because it changes obj in place.
    */
    if (NAMED(value))
        value = duplicate(value);
    SET_SLOT(obj, slot, value);
    return obj;
}

SEXP lc_prefix(SEXP x, SEXP ignoreCase)
{
    /* probably does not work for non-C locale */
    int i, j, nc, min_nc, done, ucase;
    char *prefix, *first, c;
    SEXP ans;

    x = coerceVector(x, STRSXP);
    if (length(x) < 2)
        return x;
    PROTECT(x);

    if (!isLogical(ignoreCase))
        error("invalid arg, 'ignoreCase' must be logical");
    ucase = LOGICAL(ignoreCase)[0];
    if (ucase == NA_LOGICAL)
        error("invalid arg, 'ignoreCase' must be TRUE or FALSE");

    min_nc = strlen(CHAR(STRING_ELT(x, 0)));
    for (i = 1; i < length(x); i++) {
        ans = STRING_ELT(x, i);
        if (ans == NA_STRING)
            error("lc_prefix cannot handle NA");
        nc = strlen(CHAR(ans));
        if (nc < min_nc)
            min_nc = nc;
    }

    /* init to last char in x[1] */
    first = CHAR(STRING_ELT(x, 0));
    prefix = (char *)Calloc(min_nc, char);
    if (ucase)
        prefix[0] = toupper(first[0]);
    else
        prefix[0] = first[0];
    done = 0;
    for (i = 0; i < min_nc; i++) {
        for (j = 0; j < length(x); j++) {
            c = CHAR(STRING_ELT(x, j))[i];
            if (ucase)
                c = toupper(c);
            if (c != prefix[i]) {
                if (i == 0)
                    prefix[0] = '\0';
                else
                    prefix[i] = '\0';
                done = 1;
                break;
            }
        }
        if (done || i+1 >= min_nc)
            break;
        if (ucase)
            prefix[i+1] = toupper(first[i+1]);
        else
            prefix[i+1] = first[i+1];
    }

    ans = mkString(prefix);
    Free(prefix);
    UNPROTECT(1);
    return ans;
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
