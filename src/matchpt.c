/* finds nearest neighbour, either for every point in one array (if only one is given)
   or nearest neighbour in the second array for every point in the first */

/* O.Sklyar, EBI, 2006 */

#include <Rinternals.h>
#include <Rdefines.h>

#define INDEX(i, j, n) ((i) + (j)*(n))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP matchpt(SEXP x, SEXP y) {
    int * dimx;
    int * dimy;
    double dist, mdist, tmp, * dres, *dx, *dy=NULL;
    int i, j, k, index, nptx, npty, ncol, noY=1;
    SEXP res, newDim;

    dimx = INTEGER(GET_DIM(x));
    nptx = dimx[0];
    npty = nptx;
    dx = REAL(x);
    dy = dx;
    if (y != R_NilValue) {
        dy = REAL(y);
        dimy = INTEGER(GET_DIM(y));
        npty = dimy[0];
        noY = 0;
    }
    ncol = dimx[1];
    PROTECT(res = allocVector(REALSXP, nptx * 2));
    dres = REAL(res);
    for (i = 0; i < nptx * 2; i++) dres[i] = 0;
    for (i = 0; i < nptx; i++) {
        if (noY && dres[INDEX(i, 0, nptx)] > 0 ) continue;
        index = -1;
        mdist = R_PosInf;
        for (j = 0; j < npty; j++) {
            if (noY && i == j) continue;
            dist = 0;
            for (k = 0; k < ncol; k++) {
                tmp = dx[INDEX(i, k, nptx)] - dy[INDEX(j, k, npty)];
                dist += tmp * tmp;
            }
            if (dist < mdist) {
                index = j;
                mdist = dist;
            }
        }
        dres[INDEX(i, 0, nptx)] = index + 1;
        dres[INDEX(i, 1, nptx)] = sqrt(mdist);
        if (noY && index >= 0) {
          dres[INDEX(index, 0, nptx)] = i + 1;
          dres[INDEX(index, 1, nptx)] = sqrt(mdist);
        }
    }
    PROTECT(newDim = allocVector(INTSXP, 2));
    INTEGER(newDim)[0] = nptx;
    INTEGER(newDim)[1] = 2;
    SET_DIM(res, newDim);
    UNPROTECT(2);
    return res;
}
