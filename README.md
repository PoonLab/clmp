# clmp

`clmp` is an R extension, mostly written in C, for extracting [genetic clusters](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5210024/) from a [phylogeny](https://en.wikipedia.org/wiki/Phylogenetic_tree) using a [Markov-modulated Poisson process](http://giantoak.github.io/MMPP_Tutorial/) to model variation in branching rates.

## Usage
```R
> require(clmp)
Loading required package: clmp
Loading required package: ape
Loading required package: phangorn

> t1 <- read.tree('examples/test.nwk')  # a simulated tree with 1000 tips, 100 in clusters
> res <- clmp(t1)
log likelihood for 2 state model is 2238.543290
rates: 495.368085 1305.115860 
Q: [    *   2.526691 ]
   [ 23.309483   *    ]

> index <- match(t1$tip.label, names(res))  
> labels <- grepl("_1_", t1$tip.label)  # extract truth from the tip labels
> table(labels, res[index])
      
labels    0   1
  FALSE 860   3  # false positive rate, 3/(3+860)=0.34%
  TRUE   13  98  # true positive rate, 98/(98+13)=88.2%
```

## Prerequisites

* [R](http://cran.r-project.org), obviously!
* [ape](https://cran.r-project.org/web/packages/ape/index.html)
* [phangorn](https://cran.r-project.org/web/packages/phangorn/index.html)

For compiling from source (developers only!):
* [GSL](http://www.gnu.org/software/gsl/)
* [igraph](https://github.com/igraph/igraph) - note this is C-igraph, not the igraph package that is available for R.

