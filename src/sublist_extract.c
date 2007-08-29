#include <Rdefines.h>

static SEXP list_el(SEXP list, SEXP name)
{
    SEXP elmt = R_NilValue;
    SEXP names = getAttrib(list, R_NamesSymbol);
    SEXP cur_name;
    const char *cname = CHAR(name);
    int i, found = 0;

    for (i = 0; i < length(list); i++) {
        cur_name = STRING_ELT(names, i);
        if (cur_name == name || strcmp(CHAR(cur_name), cname) == 0) {
            elmt = VECTOR_ELT(list, i);
            found = 1;
            break;
        }
    }
    if (!found)
        error("no element named '%s'", cname);
    return elmt;
}

Rboolean checkScalarLogical(SEXP v, Rboolean na_ok)
{
    if (!isLogical(v))
        error("expecting a logical vector, found a %s",
              type2char(TYPEOF(v)));
    if (length(v) != 1)
        error("expecting a logical vector of length one, found length %d",
              length(v));
    if (!na_ok && LOGICAL(v)[0] == NA_LOGICAL)
        error("found NA where TRUE/FALSE needed");

    return TRUE;
}

static void fill_integer(SEXP L, SEXP name, SEXP dest)
{
    SEXP el;
    int *ansi;
    int i;
    ansi = INTEGER(dest);
    for (i = 0; i < length(L); i++) {
        el = list_el(VECTOR_ELT(L, i), name);
        if (length(el) != 1)
            error("unable to simplify, element %d has length %d",
                  i, length(el));
        ansi[i] = INTEGER(el)[0];
    }
}

static void fill_double(SEXP L, SEXP name, SEXP dest)
{
    SEXP el;
    double *ansd;
    int i;
    ansd = REAL(dest);
    for (i = 0; i < length(L); i++) {
        el = list_el(VECTOR_ELT(L, i), name);
        if (length(el) != 1)
            error("unable to simplify, element %d has length %d",
                  i, length(el));
        ansd[i] = REAL(el)[0];
    }
}

static void fill_logical(SEXP L, SEXP name, SEXP dest)
{
    SEXP el;
    int *ansi;
    int i;
    ansi = LOGICAL(dest);
    for (i = 0; i < length(L); i++) {
        el = list_el(VECTOR_ELT(L, i), name);
        if (length(el) != 1)
            error("unable to simplify, element %d has length %d",
                  i, length(el));
        ansi[i] = LOGICAL(el)[0];
    }
}

static void fill_complex(SEXP L, SEXP name, SEXP dest)
{
    SEXP el;
    Rcomplex *anscplx;
    int i;
    anscplx = COMPLEX(dest);
    for (i = 0; i < length(L); i++) {
        el = list_el(VECTOR_ELT(L, i), name);
        if (length(el) != 1)
            error("unable to simplify, element %d has length %d",
                  i, length(el));
        anscplx[i] = COMPLEX(el)[0];
    }
}

static void fill_string(SEXP L, SEXP name, SEXP dest)
{
    SEXP el;
    int i;

    for (i = 0; i < length(L); i++) {
        el = list_el(VECTOR_ELT(L, i), name);
        if (length(el) != 1)
            error("unable to simplify, element %d has length %d",
                  i, length(el));
        SET_STRING_ELT(dest, i, STRING_ELT(el, 0));
    }
}

static void fill_raw(SEXP L, SEXP name, SEXP dest)
{
    SEXP el;
    unsigned char *ansr;
    int i;

    ansr = RAW(dest);
    for (i = 0; i < length(L); i++) {
        el = list_el(VECTOR_ELT(L, i), name);
        if (length(el) != 1)
            error("unable to simplify, element %d has length %d",
                  i, length(el));
        ansr[i] = RAW(el)[0];
    }
}

SEXP sublist_extract(SEXP L, SEXP namev, SEXP simplify, SEXP keep_names)
{
    SEXP name, el, ans, ansnms;
    int i;

    if (!isNewList(L))
        error("'L' must be a list");
    if (!isString(namev) || length(namev) != 1)
        error("'name' must be a character vector of length one");
    name = STRING_ELT(namev, 0);
    if (name == NA_STRING)
        error("'name' cannot be NA");
    checkScalarLogical(simplify, 0);
    checkScalarLogical(keep_names, 0);

    if (LOGICAL(simplify)[0]) {
        if (!length(L))
            error("can't extract from an empty list when simplify=TRUE");
        el = list_el(VECTOR_ELT(L, 0), name);
        if (length(el) != 1)
            error("unable to simplify, element 0 has length %d", length(el));
        PROTECT(ans = allocVector(TYPEOF(el), length(L)));
        switch (TYPEOF(el)) {
        case REALSXP:
            fill_double(L, name, ans);
            break;
        case INTSXP:
            fill_integer(L, name, ans);
            break;
        case LGLSXP:
            fill_logical(L, name, ans);
            break;
        case CPLXSXP:
            fill_complex(L, name, ans);
            break;
        case STRSXP:
            fill_string(L, name, ans);
            break;
        case RAWSXP:
            fill_raw(L, name, ans);
            break;
        default:
            error("unample to simplify when type is '%s'",
                  type2char(TYPEOF(el)));
            break;
        }
    } else {
        PROTECT(ans = allocVector(VECSXP, length(L)));
        for (i = 0; i < length(L); i++) {
            SET_VECTOR_ELT(ans, i, list_el(VECTOR_ELT(L, i), name));
        }
    }

    if (LOGICAL(keep_names)[0]) {
        ansnms = duplicate(getAttrib(L, R_NamesSymbol));
        setAttrib(ans, R_NamesSymbol, ansnms);
    }

    UNPROTECT(1);
    return ans;
}
