/* finds nearest neighbour, either for every point in one array (if only one is given)
   or nearest neighbour in the second array for every point in the first */

/* O.Sklyar, EBI, 2006 */

#include <Rinternals.h>
#include <Rdefines.h>
#include <float.h>

#ifndef DBL_MAX
#define DBL_MAX 1e37
#endif

#define INDEX(i, j, n) ((i) + (j)*(n))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP matchpt(SEXP x, SEXP y) {
    int * dimx;
    int * dimy;
    double dist, mdist, tmp;
    int i, j, k, index, nptx, npty, ncol, useY;
    SEXP res, newDim;

    dimx = INTEGER(GET_DIM(x));
    nptx = dimx[0];
    npty = nptx;
    if (y != R_NilValue) {
        useY = 1;
        dimy = INTEGER(GET_DIM(y));
        npty = dimy[0];
    }
    else
        useY = 0;
    ncol = dimx[1];
    PROTECT(res = allocVector(REALSXP, nptx * 2));
    for (i = 0; i < nptx; i++) {
        index = i;
        mdist = DBL_MAX;
        for (j = 0; j < npty; j++) {
            if (useY == 0 && i == j) continue;
            dist = 0;
            for (k = 0; k < ncol; k++) {
                if (useY == 1)
                    tmp = REAL(x)[INDEX(i, k, nptx)] - REAL(y)[INDEX(j, k, npty)];
                else
                    tmp = REAL(x)[INDEX(i, k, nptx)] - REAL(x)[INDEX(j, k, nptx)];
                dist += tmp * tmp;
            }
            dist = sqrt(dist);
            if (dist < mdist) {
                index = j;
                mdist = dist;
            }
        }
        REAL(res)[INDEX(i, 0, nptx)] = index + 1;
        REAL(res)[INDEX(i, 1, nptx)] = mdist;
    }
    PROTECT(newDim = allocVector(INTSXP, 2));
    INTEGER(newDim)[0] = nptx;
    INTEGER(newDim)[1] = 2;
    SET_DIM(res, newDim);
    UNPROTECT(2);
    return res;
}
