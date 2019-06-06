#include <igraph/igraph.h>
#include <igraph/igraph_error.h>

#include "util.h"
#include "newick_parser.h"
#include "tree.h"
#include "mmpp.h"

#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


void yy_scan_string(const char *);

igraph_t * R_clmp_parse_newick(SEXP newick) {
    // derived from Rosemary's parse_newick() function
    igraph_t *tree;
    const char * newick_str = CHAR(STRING_ELT(newick, 0));

    extern int yynode;  // in newick_parser.y
    igraph_vector_t edge, branch_length, size;
    igraph_strvector_t label;

    // initialize vector containers
    igraph_vector_init(&edge, 0);
    igraph_vector_init(&size, 0);
    igraph_vector_init(&branch_length, 0);
    igraph_strvector_init(&label, 0);

    yynode = 0;  // set iterator for newick_parser
    yy_scan_string(newick_str);  // prepare to take scanner's input from string
    yyparse(&edge, &size, &branch_length, &label);  // parse input stream

    // allocate memory for igraph object
    tree = malloc(sizeof(igraph_t));
    igraph_empty(tree, igraph_vector_size(&size), 1);  // initialize with empty graph
    igraph_add_edges(tree, &edge, 0);

    for (int i = 0; i < igraph_vector_size(&size); ++i)
    {
        igraph_incident(tree, &edge, i, IGRAPH_IN);
        if (igraph_vector_size(&edge) > 0) {
            SETEAN(tree, "length", (int) VECTOR(edge)[0], VECTOR(branch_length)[i]);
        }
        SETVAS(tree, "id", i, STR(label,i));  // assign node label to vertex
    }

    // free up memory allocated to vectors
    igraph_vector_destroy(&edge);
    igraph_vector_destroy(&size);
    igraph_vector_destroy(&branch_length);
    igraph_strvector_destroy(&label);
    return tree;
}



void display_results(int nrates, double *theta, double branch_scale)
{
    // from Rosemary's pcbr.c (github.com/rmcclosk/netabc)
    // DEPRECATED
    int i, j;
    int *rate_order = malloc(nrates * sizeof(int));

    order(theta, rate_order, sizeof(double), nrates, compare_doubles);

    fprintf(stderr, "rates: ");
    for (i = 0; i < nrates; ++i)
	    fprintf(stderr, "%f ", theta[rate_order[i]] * branch_scale);
    fprintf(stderr, "\n");

    fprintf(stderr, "Q: ");
    for (i = 0; i < nrates; ++i) {
        fprintf(stderr, "[ ");
        for (j = 0; j < nrates; ++j) {
            if (i == j)
                fprintf(stderr, "   *   ");
            else if (i > j)
	            fprintf(stderr, "%f", theta[nrates + rate_order[i]*(nrates-1) + rate_order[j]] * branch_scale);
            else
	            fprintf(stderr, "%f", theta[nrates + rate_order[i]*(nrates-1) + rate_order[j] - 1] * branch_scale);
        }
        if (i < nrates - 1)
            fprintf(stderr, " ]\n   ");
        else
            fprintf(stderr, " ]\n");
    }
    free(rate_order);
}


SEXP R_clmp(SEXP nwk, SEXP nrates_arg, SEXP bounds_arg, SEXP trace_arg) {
    /*
     Implement MMPP method
     @arg nwk:  <input> Newick tree string
     @arg nrates:  <input>  number of rate classes
     @arg bounds_arg:  <input>  vector of length 4, min-max values for 
                       rates and transition rates
     @arg trace_arg:  <input> integer, write verbose output to stderr
                      if >0.  Also used to set the log interval.
     */
    SEXP result, cindex, sindex, names, loglik, mle_rates, mle_trans;

    int nrates = (int) REAL(nrates_arg)[0];
    int trace = (int) REAL(trace_arg)[0];
    
    double *theta = malloc(nrates * nrates * sizeof(double));
    int *rate_order = malloc(nrates * sizeof(int));
    
    int i, j, *states, *clusters;
    int nnodes;

    
    //TODO: implement bounds
    SEXP bounds = PROTECT(allocVector(REALSXP, 4));
    for (i=0; i<4; i++) {
        REAL(bounds)[i] = REAL(bounds_arg)[i];
    }
    UNPROTECT(1);

    // annotate igraph object with branch lengths (edge attributes)
    igraph_i_set_attribute_table(&igraph_cattribute_table);
    igraph_t * tree = R_clmp_parse_newick(nwk);
    nnodes = igraph_vcount(tree);

    // allocate vectors
    cindex = PROTECT(allocVector(INTSXP, nnodes));
    names = PROTECT(allocVector(STRSXP, nnodes));
    loglik = PROTECT(allocVector(REALSXP, 1));
    mle_rates = PROTECT(allocVector(REALSXP, nrates));
    mle_trans = PROTECT(allocVector(REALSXP, nrates*nrates));
    sindex = PROTECT(allocVector(INTSXP, nnodes));
    
    result = PROTECT(allocVector(VECSXP, 5));

    // allocate vectors given number of nodes in tree
    states = malloc(igraph_vcount(tree) * sizeof(int));
    clusters = malloc(igraph_vcount(tree) * sizeof(int));

    // run MMPP analysis
    REAL(loglik)[0] = fit_mmpp(tree, &nrates, &theta, trace, NULL, 
         states, LRT, 0, REAL(bounds));

    
    display_results(nrates, theta, 1.);
    order(theta, rate_order, sizeof(double), nrates, compare_doubles);
    for (i = 0; i < nrates; ++i) {
        REAL(mle_rates)[i] = theta[rate_order[i]];
        
        for (j = 0; j < nrates; ++j) {
            if (i==j) {
                REAL(mle_trans)[i*nrates + j] = 0;  // placeholder
            }
            else if (i > j) {
                REAL(mle_trans)[i*nrates + j] = theta[nrates + rate_order[i]*(nrates-1) + rate_order[j]];
            }
            else {
                REAL(mle_trans)[i*nrates + j] = theta[nrates + rate_order[i]*(nrates-1) + rate_order[j] - 1];
            }
        }
    }
    

    get_clusters(tree, states, clusters, 1);
    for (i = 0; i < nnodes; ++i) {
        INTEGER(cindex)[i] = clusters[i];
        INTEGER(sindex)[i] = states[i];
        SET_STRING_ELT(names, i, mkChar(VAS(tree, "id", i)));
    }
    setAttrib(cindex, R_NamesSymbol, names);
    setAttrib(sindex, R_NamesSymbol, names);
    
    SET_VECTOR_ELT(result, 0, cindex);
    SET_VECTOR_ELT(result, 1, loglik);
    SET_VECTOR_ELT(result, 2, mle_rates);
    SET_VECTOR_ELT(result, 3, mle_trans);
    SET_VECTOR_ELT(result, 4, sindex);

    // free up memory
    igraph_destroy(tree);

    UNPROTECT(7);

    return(result);
}
