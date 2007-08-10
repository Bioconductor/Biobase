/* finds nearest neighbour, either for every point in one array (if only one is given)
   or nearest neighbour in the second array for every point in the first */

/* O.Sklyar, EBI, 2006 */

#include <Rinternals.h>
#include <Rdefines.h>

#define INDEX(i, j, n) ((i) + (j)*(n))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP matchpt(SEXP x, SEXP y) {
    int * dimx;

    double dist, mdist, tmp, *dx, *dy, *pd;
    int i, j, k, index, nptx, npty, ncol, noY, *pidx;
    SEXP res, namesres, idx, d;

    dimx = INTEGER(GET_DIM(x));
    nptx = dimx[0];
    ncol = dimx[1];
    dx = REAL(x);

    if (y != R_NilValue) {
      dy = REAL(y);
      npty = INTEGER(GET_DIM(y))[0];
      noY = 0;
    } else {
      dy = dx;
      npty = nptx;
      noY = 1;
    }

    PROTECT(d   = allocVector(REALSXP, nptx));
    PROTECT(idx = allocVector(INTSXP,  nptx));
    pd    = REAL(d);
    pidx  = INTEGER(idx);

    for (i = 0; i<nptx; i++) {
        index = NA_INTEGER;
        mdist = R_PosInf;
        for (j = 0; j < npty; j++) {
            if (noY && i == j) continue;
            dist = 0;
            for (k = 0; k < ncol; k++) {
                tmp = dx[INDEX(i, k, nptx)] - dy[INDEX(j, k, npty)];
                dist += tmp * tmp;
            }
            if (dist < mdist) {
                index = j+1;
                mdist = dist;
            }
        }
        pidx[i] = index;
        pd[i]   = sqrt(mdist);
    }
    
    /* return value: a list with two elements, idx and d */
    PROTECT(res = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, idx);
    SET_VECTOR_ELT(res, 1, d);

    PROTECT(namesres = allocVector(STRSXP, 2));
    SET_STRING_ELT(namesres, 0, mkChar("index"));
    SET_STRING_ELT(namesres, 1, mkChar("distance"));
    setAttrib(res, R_NamesSymbol, namesres);

    UNPROTECT(4);
    return res;
}
