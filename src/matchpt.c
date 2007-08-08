/* finds nearest neighbour, either for every point in one array (if only one is given)
   or nearest neighbour in the second array for every point in the first */

/* O.Sklyar, EBI, 2006 */

#include <Rinternals.h>
#include <Rdefines.h>
#include <float.h>

#define INDEX(i, j, n) ((i) + (j)*(n))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP matchpt(SEXP x, SEXP y) {
    int * dimx;
    int * dimy;
    double dist, mdist, tmp, * dres, *dx, *dy=NULL;
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
    dres = REAL(res);
    dx = REAL(x);
    if ( useY == 1 ) dy = REAL(y);
    for (i = 0; i < nptx; i++) {
        index = i;
        mdist = R_PosInf;
        for (j = 0; j < npty; j++) {
            if (useY == 0 && i == j) continue;
            dist = 0;
            for (k = 0; k < ncol; k++) {
                if (useY == 1)
                    tmp = dx[INDEX(i, k, nptx)] - dy[INDEX(j, k, npty)];
                else
                    tmp = dx[INDEX(i, k, nptx)] - dx[INDEX(j, k, nptx)];
                dist += tmp * tmp;
            }
            dist = sqrt(dist);
            if (dist < mdist) {
                index = j;
                mdist = dist;
            }
        }
        dres[INDEX(i, 0, nptx)] = index + 1;
        dres[INDEX(i, 1, nptx)] = mdist;
    }
    PROTECT(newDim = allocVector(INTSXP, 2));
    INTEGER(newDim)[0] = nptx;
    INTEGER(newDim)[1] = 2;
    SET_DIM(res, newDim);
    UNPROTECT(2);
    return res;
}
