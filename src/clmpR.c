#include <igraph.h>
#include <igraph_error.h>

#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "newick_parser.h"
#include "tree.h"
#include "mmpp.h"

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

SEXP R_clmp(SEXP nwk, SEXP nrates_arg, SEXP bounds_arg) {
    /*
     Implement MMPP method
     @arg nwk:  <input> Newick tree string
     @arg nrates:  <input>  number of rate classes
     */
    SEXP result;
    SEXP names;

    int nrates = (int) REAL(nrates_arg)[0];
    double *theta = malloc(nrates * nrates * sizeof(double));
    int i, error, *states, *clusters;
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

    // allocate return matrix
    result = PROTECT(allocVector(REALSXP, nnodes));
    names = PROTECT(allocVector(STRSXP, nnodes));

    // allocate vectors given number of nodes in tree
    states = malloc(igraph_vcount(tree) * sizeof(int));
    clusters = malloc(igraph_vcount(tree) * sizeof(int));

    // run MMPP analysis
    error = fit_mmpp(tree, &nrates, &theta, FALSE, NULL,
            states, LRT, 0, REAL(bounds));
    display_results(nrates, theta, 1.);

    get_clusters(tree, states, clusters, 1);

    for (i = 0; i < nnodes; ++i) {
        REAL(result)[i] = clusters[i];
        SET_STRING_ELT(names, i, mkChar(VAS(tree, "id", i)));
    }
    setAttrib(result, R_NamesSymbol, names);

    // free up memory
    igraph_destroy(tree);

    UNPROTECT(2);

    return(result);
}
