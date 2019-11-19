/* Copyright R. Gentleman, 2003 */

#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Utils.h>
#include <ctype.h>

/* fast computation of row order statistics */

SEXP rowQ(SEXP inmat, SEXP which)
{
  SEXP ans;
  int i, j,  nrow, ncol, medval;
  double *rowData;

  /* we can do integers later */
  if( !isMatrix(inmat) || !isReal(inmat) )
    error("'imat' must be a numeric matrix");

  /* FIXME: should check for FINITE/NA */
  if( !isNumeric(which) || length(which) != 1 )
    error("'which' order statistic must be numeric");

  /* subtract one here, since rPsort does zero based addressing*/
  medval = asInteger(which) - 1;

  PROTECT(ans = getAttrib(inmat, R_DimSymbol));
  nrow = INTEGER(ans)[0];
  ncol = INTEGER(ans)[1];

  /* sanity check */
  if( medval < 0 || medval >= ncol )
      error("cannot calculate order statistic on object with %d columns",
            ncol);

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
    if (MAYBE_REFERENCED(value))
        value = duplicate(value);
    SET_SLOT(obj, slot, value);
    return obj;
}

SEXP lc_prefix(SEXP x, SEXP ignoreCase)
{
    /* XXX: x must be a character vector containing ASCII only */
    int i, j, nc, min_nc, done, ucase;
    char *prefix, c;
    const char *first;
    SEXP ans;

    x = coerceVector(x, STRSXP);
    if (length(x) < 2)
        return x;
    PROTECT(x);

    if (!isLogical(ignoreCase))
        error("'ignoreCase' must be logical");
    ucase = LOGICAL(ignoreCase)[0];
    if (ucase == NA_LOGICAL)
        error("'ignoreCase' must be TRUE or FALSE");

    min_nc = strlen(CHAR(STRING_ELT(x, 0)));
    for (i = 1; i < length(x); i++) {
        ans = STRING_ELT(x, i);
        if (ans == NA_STRING)
            error("lc_prefix cannot handle NA's in argument 'x'");
        nc = strlen(CHAR(ans));
        if (nc < min_nc)
            min_nc = nc;
    }

    /* init to first char in first elem. of x */
    first = CHAR(STRING_ELT(x, 0));
    prefix = (char *)Calloc(min_nc + 1, char);
    done = 0;
    i = 0;
    while (1) {
        if (ucase)
            prefix[i] = toupper(first[i]);
        else
            prefix[i] = first[i];
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
        i++;
        if (done || i > min_nc)
            break;
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
