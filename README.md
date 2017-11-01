# clmp

clmp is an R package for extracting genetic clusters from a phylogeny using a Markov-modulated Poisson process to model variation in branching rates.

## Usage
```R
> require(clmp)
Loading required package: clmp
Loading required package: ape
Loading required package: phangorn

> t1 <- read.tree('examples/structSIR.n500.nwk')
> t1

Phylogenetic tree with 492 tips and 491 internal nodes.

Tip labels:
	T96_I_0_Sampling, T95_I_0_Sampling, T94_I_0_Sampling, T93_I_0_Sampling, T92_I_0_Sampling, T91_I_0_Sampling, ...
Node labels:
	Node1, Node9, Node8, Node7, Node6, Node5, ...

Rooted; includes branch lengths.

> res <- clmp(t1)
log likelihood for 2 state model is 903.303757
rates: 170.256367 481.467274 
Q: [    *   2.990365 ]
   [ 6.715850   *    ]

> table(res)
res
  0   1 
904  79 
```

## Prerequisites

* [R](cran.r-project.org), obviously!
* [ape](https://cran.r-project.org/web/packages/ape/index.html)
* [phangorn](https://cran.r-project.org/web/packages/phangorn/index.html)

For compiling from source (developers only!):
* [GSL](http://www.gnu.org/software/gsl/)
* [igraph](https://github.com/igraph/igraph) - note this is C-igraph, not the igraph package that is available for R.

