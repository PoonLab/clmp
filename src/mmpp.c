#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h> 
#include <gsl/gsl_matrix.h> 
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_cblas.h>
#include <igraph/igraph.h>

#include "cmaes_interface.h"
#include "boundary_transformation.h"

#include "mmpp.h"
#include "tree.h"
#include "util.h"
#include "stats.h"

#include <R_ext/Utils.h>

#define CMAES_POP_SIZE 100
#define MAX_NRATES 6

// ck = Chapman-Kolmogorov? <art>
struct ck_params {
    int nrates;  // dimensionality of Q matrix
    double *Q;  // state transition matrix
};

struct mmpp_workspace {
    struct ck_params ckpar;
    double *y;  // transition matrix cache
    double *P;  // store transition matrices for every branch in tree
    double *L;  // vector of state likelihoods per node
    double *Li;
    int *C;
    double *pi;
    double *branch_lengths;
    int *scale;
    int *bl_order;
    gsl_matrix *Q;
    gsl_vector_complex *eval;
    gsl_matrix_complex *evec;
    gsl_eigen_nonsymmv_workspace *ew;
    igraph_adjlist_t al;
};

int ckdiff(double t, const double y[], double f[], void *params);
void calculate_P(const igraph_t *tree, int nrates, const double *theta, 
        mmpp_workspace *w, int use_tips);
void calculate_pi(const igraph_t *tree, int nrates, const double *theta, 
        mmpp_workspace *w);
void mmpp_workspace_set_params(mmpp_workspace *w, const double *theta);
int _fit_mmpp(const igraph_t *tree, int nrates, double *theta, int trace,
             const char *cmaes_settings, int *states, double *loglik,
             int use_tips, double bounds[4], double tol, double tolhist, int seed);


double fit_mmpp(const igraph_t *tree, int *nrates, double **theta, int trace,
             const char *cmaes_settings, int *states, model_selector sel,
             int use_tips, double bounds[4], double tol, double tolhist, int seed)
{
    int error = 0;
    double loglik;

    error = _fit_mmpp(tree, *nrates, *theta, trace, cmaes_settings, states,
                      &loglik, use_tips, bounds, tol, tolhist, seed);
    if (error) {
      fprintf(stderr, "Warning in fit_mmpp: parameter estimates did not converge\n");
    }
    //fprintf(stderr, "log likelihood for %d state model is %f\n", *nrates, loglik);
    return loglik;  //error;
}


void get_clusters(const igraph_t *tree, const int *states, int *clusters,
        int cluster_state)
{
    // Assign branches to clusters given their inferred state (rate class)

    igraph_adjlist_t al;
    igraph_vector_int_t *children;
    int i, j, child, ccount = 0;

    igraph_adjlist_init(tree, &al, IGRAPH_OUT);

    memset(clusters, 0, igraph_vcount(tree) * sizeof(int));

    if (states[root(tree)] >= cluster_state) {
        // root node is in clustering state
        clusters[root(tree)] = ++ccount;
    }

    for (i = igraph_vcount(tree)-1; i >= 0; --i) {
        children = igraph_adjlist_get(&al, i);
      
        // if i-th node is internal
        if (igraph_vector_int_size(children) > 0)
        {
            // assume strictly bifurcating tree
            for (j = 0; j < 2; ++j) {
                child = VECTOR(*children)[j];
              
                if (states[child] < cluster_state) {
                  clusters[child] = 0;
                }
                else if (states[i] < cluster_state) {
                  // child is in a clustering state and its parent is not
                  // start new cluster
                  clusters[child] = ++ccount;
                }
                else {
                  // child is in same cluster as parent
                  clusters[child] = clusters[i];
                }
            }
        }
    }

    igraph_adjlist_destroy(&al);
}


mmpp_workspace *mmpp_workspace_create(const igraph_t *tree, int nrates)
{
    struct mmpp_workspace *w = malloc(sizeof(struct mmpp_workspace));
    igraph_vector_t vec;

    w->ckpar.nrates = nrates;
    w->ckpar.Q = malloc(nrates * nrates * sizeof(double));
    w->y = malloc(nrates * nrates * sizeof(double));
    w->P = malloc(nrates * nrates * igraph_vcount(tree) * sizeof(double));
    w->L = malloc(nrates * igraph_vcount(tree) * sizeof(double));
    w->Li = malloc(nrates * nrates * sizeof(double));
    w->C = malloc(nrates * igraph_vcount(tree) * sizeof(int));
    w->scale = malloc(igraph_vcount(tree) * sizeof(int));
    w->pi = malloc(nrates * sizeof(double));
    w->branch_lengths = malloc(igraph_ecount(tree) * sizeof(double));
    w->bl_order = malloc(igraph_ecount(tree) * sizeof(int));
    igraph_adjlist_init(tree, &w->al, IGRAPH_OUT);

    w->Q = gsl_matrix_alloc(nrates, nrates);
    w->eval = gsl_vector_complex_alloc(nrates);
    w->evec = gsl_matrix_complex_alloc(nrates, nrates);
    w->ew = gsl_eigen_nonsymmv_alloc(nrates);

    // collect and order branch lengths
    igraph_vector_init(&vec, igraph_ecount(tree));
    EANV(tree, "length", &vec);
    order(VECTOR(vec), w->bl_order, sizeof(double), igraph_ecount(tree), compare_doubles);
    memcpy(w->branch_lengths, VECTOR(vec), igraph_ecount(tree) * sizeof(double));

    igraph_vector_destroy(&vec);
    return w;
}

void mmpp_workspace_free(mmpp_workspace *w)
{
    free(w->ckpar.Q);
    free(w->y);
    free(w->P);
    free(w->L);
    free(w->Li);
    free(w->C);
    free(w->scale);
    free(w->pi);
    free(w->bl_order);
    free(w->branch_lengths);
    igraph_adjlist_destroy(&w->al);
    gsl_matrix_free(w->Q);
    gsl_vector_complex_free(w->eval);
    gsl_matrix_complex_free(w->evec);
    gsl_eigen_nonsymmv_free(w->ew);
    free(w);
}

void guess_parameters(const igraph_t *tree, int nrates, double *theta)
{
    int i, nbranch = (igraph_vcount(tree) - 3)/2, cur = 0;
    int part_size = (int) ceil((float) nbranch / (float) nrates);
    igraph_inclist_t il;
    igraph_vector_t degree;
    igraph_vector_int_t *edge;
    double *int_edges = malloc(nbranch * sizeof(double));
    double intsum = 0;

    // find out-degree of all nodes
    igraph_vector_init(&degree, igraph_vcount(tree));
    igraph_degree(tree, &degree, igraph_vss_all(), IGRAPH_OUT, 0);

    // collect the internal branch lengths
    igraph_inclist_init(tree, &il, IGRAPH_IN);
    for (i = 0; i < igraph_vcount(tree); ++i) {
        edge = igraph_inclist_get(&il, i);
        if (igraph_vector_int_size(edge) > 0 && (int) VECTOR(degree)[i] > 0) {
            int_edges[cur++] = EAN(tree, "length", VECTOR(*edge)[0]);
            intsum += int_edges[cur-1];
        }
    }

    // for branching rates, partition internal branch lengths, and take means of partitions
    qsort(int_edges, nbranch, sizeof(double), compare_doubles);
    memset(theta, 0, nrates * sizeof(double));
    for (i = 0; i < nbranch; ++i)
        theta[nrates - (i / part_size) - 1] += int_edges[i] / part_size;

    // the guessed rates are the inverse of the means
    for (i = 0; i < nrates; ++i)
        theta[i] = 1.0 / fmax(theta[i], 1e-4);

    // choose transition rates all equal such that the expected number of
    // transitions is 1% of the number of branches
    for (i = 0; i < nrates * (nrates - 1); ++i)
        theta[nrates+i] = nbranch / nrates / intsum / 100.0;

    // clean up
    igraph_inclist_destroy(&il);
    free(int_edges);
}

double likelihood(const igraph_t *tree, int nrates, const double *theta,
                  mmpp_workspace *w, int use_tips,
                  int reconstruct)
{ 
    // likelihoods are stored in linearized vector w->L 
    //   e.g., if nrates = 3, node i=0 likelihoods stored in 0, 1, 2
    //         node i=1 likelihoods stored in 3, 4, 5
    
    int i, j, rt = root(tree);  // node indices
    double lik = 0;
    int lchild, rchild, pstate, cstate, new_scale;
    igraph_vector_int_t *children;  // vector of indices to children, assuming binary tree

    mmpp_workspace_set_params(w, theta);
    calculate_P(tree, nrates, theta, w, use_tips);

    for (i = 0; i < igraph_vcount(tree); ++i)
    {
        children = igraph_adjlist_get(&w->al, i);
        if (igraph_vector_int_size(children) == 0)
        {
            lchild = -1; rchild = -1;
            w->scale[i] = 0;
        }
        else
        {
            lchild = VECTOR(*children)[0];
            rchild = VECTOR(*children)[1];
            w->scale[i] = w->scale[lchild] + w->scale[rchild];
        }

        // iterate over rate class assignments to parent node
        for (pstate = 0; pstate < nrates; ++pstate)
        {
            // iterate over rate class assignments to child node
            for (cstate = 0; cstate < nrates; ++cstate) {
                if (lchild != -1) {
                    // node is internal
                    w->Li[cstate] = w->L[lchild * nrates + cstate] *
                                    w->L[rchild * nrates + cstate] *
                                    w->P[i * nrates * nrates + pstate * nrates + cstate];
                    // last factor is probability density of first arrival
                }
                else {
                    // node is terminal
                    w->Li[cstate] = w->P[i * nrates * nrates + pstate * nrates + cstate];
                }
            }

            if (reconstruct) {
                w->C[i * nrates + pstate] = which_max(w->Li, nrates);
                w->L[i * nrates + pstate] = w->Li[w->C[i * nrates + pstate]];
            }
            else {
                w->L[i * nrates + pstate] = sum_doubles(w->Li, nrates);
            }
        }
        // rescale log-likelihoods for i-th node
        new_scale = get_scale(&w->L[i * nrates], nrates);
        for (pstate = 0; pstate < nrates; ++pstate) {
            w->L[i * nrates + pstate] /= pow(10, new_scale);
        }
        w->scale[i] += new_scale;
    }
    // from utils.c
    lik = (reconstruct ? max_doubles : sum_doubles)(&w->L[rt * nrates], nrates);
    return log10(lik) + w->scale[rt];
}

double reconstruct(const igraph_t *tree, int nrates, const double *theta,
        mmpp_workspace *w, int *states, int use_tips)
{
    /*
     * \param[in] tree:  tree as igraph object, to reconstruct ancestral states on
     * \param[in] nrates:  number of rate categories estimated by MMPP
     * \param[in] theta:  fitted MMPP parameters
     * \param[in] w:  workspace object from mmpp_workspace_create()
     * \param[out] states:  vector to return ancestral states
     * \param[in] use_tips:  if 0, ignore terminal nodes
     */
    int i, rt = root(tree), lchild, rchild;
    double lik = likelihood(tree, nrates, theta, w, use_tips, 1);
    igraph_vector_int_t *children;

    // which_max from util.c, returns max of first argument (vector)
    // assign maximum likelihood state at root
    states[rt] = which_max(&w->L[rt * nrates], nrates);

    // traverse internal nodes of tree
    for (i = rt; i >= 0; --i)
    {
        children = igraph_adjlist_get(&w->al, i);
        if (igraph_vector_int_size(children) > 0)
        {
            lchild = VECTOR(*children)[0];
            rchild = VECTOR(*children)[1];
            states[lchild] = w->C[lchild * nrates + states[i]];
            states[rchild] = w->C[rchild * nrates + states[i]];
        }
    }
    return lik;
}

/* Private. */
int _fit_mmpp(const igraph_t *tree, int nrates, double *theta, int trace,
             const char *cmaes_settings, int *states, double *loglik, 
             int use_tips, double bounds[4], double tol, double tolhist, int seed)
{
    int i, j, verbose, step = 0,
        dimension = nrates * nrates, error = 0, cur = nrates;
    int *state_order;
    double *lbound = malloc(dimension * sizeof(double));
    double *ubound = malloc(dimension * sizeof(double));
    double *init_sd = malloc(dimension * sizeof(double));
    double *funvals, *tmp, *const *pop;
    struct mmpp_workspace *w = mmpp_workspace_create(tree, nrates);
    cmaes_t evo;
    cmaes_boundary_transformation_t trbound;

    for (i = 0; i < nrates; ++i)
    {
        lbound[i] = log(bounds[0]);
        ubound[i] = log(bounds[1]);
        init_sd[i] = 1;
    }
    for (i = nrates; i < dimension; ++i)
    {
        lbound[i] = log(bounds[2]);
        ubound[i] = log(bounds[3]);
        init_sd[i] = 1;
    }

    cmaes_boundary_transformation_init(&trbound, lbound, ubound, dimension);

    guess_parameters(tree, nrates, theta);
    for (i = 0; i < dimension; ++i)
        theta[i] = log(theta[i]);

    // seed 0 sets random seed from system clock
    funvals = cmaes_init(&evo, dimension, theta, init_sd, seed, CMAES_POP_SIZE, 
                         cmaes_settings, tol, tolhist);

  	while (!cmaes_TestForTermination(&evo)) {
        // generate search points
	    	pop = cmaes_SamplePopulation(&evo);
  	    verbose = (trace && (step%trace==0));
  	    
  	    // calculate likelihoods for each search point
	    	for (i = 0; i < CMAES_POP_SIZE; ++i) {
	    	    if (verbose)
	    	        fprintf(stderr, "%d\t%d\t", step, i);
	    	    
            cmaes_boundary_transformation(&trbound, pop[i], theta, dimension);
            for (j = 0; j < dimension; ++j)
            {
                theta[j] = exp(theta[j]);
                if (verbose)
                    fprintf(stderr, "%f\t", theta[j]);
            }
            funvals[i] = -likelihood(tree, nrates, theta, w, use_tips, 0);
            
            if (funvals[i] != funvals[i])  // detect numeric overflow?
                funvals[i] = FLT_MAX;
            if (verbose)
                fprintf(stderr, "%f\n", -funvals[i]);
        }
	    	
	    	// check for user interrupt
	    	R_CheckUserInterrupt();
	    	
	    	// update search distribution
	    	cmaes_UpdateDistribution(&evo, funvals);
	    	step++;
    }

    if (strncmp(cmaes_TestForTermination(&evo), "TolFun", 6) != 0)
    {
        error = 1;
        fprintf(stderr, "%s", cmaes_TestForTermination(&evo));
    }

    cmaes_boundary_transformation(&trbound, 
        (double const *) cmaes_GetPtr(&evo, "xbestever"), theta, dimension);

    state_order = malloc(nrates * sizeof(int));
    tmp = malloc(dimension * sizeof(double));
    for (i = 0; i < dimension; ++i)
        tmp[i] = exp(theta[i]);
    order(tmp, state_order, sizeof(double), nrates, compare_doubles);
    for (i = 0; i < nrates; ++i)
    {
        theta[i] = tmp[state_order[i]];
        for (j = 0; j < nrates; ++j)
        {
            if (state_order[i] > state_order[j])
	            theta[cur++] = tmp[nrates + state_order[i]*(nrates-1) + state_order[j]];
            else if (state_order[i] < state_order[j])
	            theta[cur++] = tmp[nrates + state_order[i]*(nrates-1) + state_order[j]-1];
        }
    }
    loglik[0] = likelihood(tree, nrates, theta, w, use_tips, 0);
    if (states != NULL)
        reconstruct(tree, nrates, theta, w, states, use_tips);

    cmaes_exit(&evo);
    cmaes_boundary_transformation_exit(&trbound);
    free(lbound);
    free(ubound);
    free(init_sd);
    free(state_order);
    free(tmp);
    mmpp_workspace_free(w);
    return error;
}


/**
 */
void mmpp_workspace_set_params(mmpp_workspace *w, const double *theta)
{
    double rowsum = 0;
    int i, j, nrates = w->ckpar.nrates, cur = nrates;

    for (i = 0; i < nrates; ++i)
    {
        rowsum = 0;
        for (j = 0; j < nrates; ++j)
        {
            if (j != i)
            {
                w->y[i * nrates + j] = 0;
                w->ckpar.Q[i * nrates + j] = theta[cur];
                rowsum += theta[cur++];
            }
        }
        w->ckpar.Q[i * nrates + i] = -rowsum - theta[i];
        w->y[i * nrates + i] = 1;  // reset to identity matrix
    }
}


void calculate_P(const igraph_t *tree, int nrates, const double *theta,
        struct mmpp_workspace *w, int use_tips)
{
    int i, j, k, from, to, cur = nrates; 
    double t = 0.0;
    igraph_vector_int_t *children;
    gsl_odeiv2_system sys;
    gsl_odeiv2_driver *d;

    // set the values at the root of the tree to the equilibrium frequencies
    calculate_pi(tree, nrates, theta, w);
    memset(&w->P[root(tree) * nrates * nrates], 0, nrates * nrates * sizeof(double));
    for (i = 0; i < nrates; ++i) {
        w->P[root(tree) * nrates * nrates + i * nrates + i] = w->pi[i];
    }

    // set up ODE
    sys.function = ckdiff;
    sys.jacobian = NULL;
    sys.dimension = nrates * nrates;
    sys.params = &w->ckpar;
    d = gsl_odeiv2_driver_alloc_y_new(&sys, gsl_odeiv2_step_rkf45, 1e-6, 1e-6, 0.0);

    // calculate values for each branch
    for (i = 0; i < igraph_ecount(tree); ++i)
    {
        if (w->branch_lengths[w->bl_order[i]] != t) {
            // Evolve system d from t to (branch length), with initial vector (y) containing
            //  values of dependent variables at time (t)
            gsl_odeiv2_driver_apply(d, &t, w->branch_lengths[w->bl_order[i]], w->y);
        }
        igraph_edge(tree, w->bl_order[i], &from, &to);

        // store matrix in y
        memcpy(&w->P[to * nrates * nrates], w->y, nrates * nrates * sizeof(double));

        // for non-terminal nodes, multiply by branching rate
        children = igraph_adjlist_get(&w->al, to);
        for (j = 0; j < nrates; ++j) {
            for (k = 0; k < nrates; ++k) {
                if (igraph_vector_int_size(children) == 0 && !use_tips) {
                    w->P[to * nrates * nrates + j * nrates + k] = (j == k);
                }
                if (igraph_vector_int_size(children) > 0) {
                    w->P[to * nrates * nrates + j * nrates + k] *= theta[k];
                }
            }
        }
    }
    gsl_odeiv2_driver_free(d);
}

void calculate_pi(const igraph_t *tree, int nrates, const double *theta, mmpp_workspace *w)
{
    int i, j, cur = nrates;
    double sum;

    // calculate Q
    for (i = 0; i < nrates; ++i) {
        sum = 0;
        for (j = 0; j < nrates; ++j) {
            if (i != j) {
                gsl_matrix_set(w->Q, i, j, theta[cur]);
                sum += theta[cur++];
            }
        }
        gsl_matrix_set(w->Q, i, i, -sum);
    }

    // calculate pi
    gsl_matrix_transpose(w->Q);
    gsl_eigen_nonsymmv(w->Q, w->eval, w->evec, w->ew);
    gsl_eigen_nonsymmv_sort(w->eval, w->evec, GSL_EIGEN_SORT_ABS_ASC);

    sum = 0;
    for (i = 0; i < nrates; ++i)
    {
        w->pi[i] = GSL_REAL(gsl_matrix_complex_get(w->evec, i, 0));
        sum += w->pi[i];
    }
    for (i = 0; i < nrates; ++i)
        w->pi[i] /= sum;
}

int ckdiff(double t, const double y[], double f[], void *params)
{
    struct ck_params *ckpar = (struct ck_params *) params;
    int n = ckpar->nrates;
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, n, n, n, 1.0, y, n, 
                ckpar->Q, n, 0.0, f, n);
    return GSL_SUCCESS;
}
