# clmp

`clmp` is an R extension, mostly written in C, for extracting [genetic clusters](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5210024/) from a [phylogeny](https://en.wikipedia.org/wiki/Phylogenetic_tree) using a [Markov-modulated Poisson process](http://giantoak.github.io/MMPP_Tutorial/) to model variation in branching rates.
Our paper that describes and evaluates MMPP as a model-based clustering method for HIV epidemiology was recently accepted in [PLOS Computational Biology](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005868).

`clmp` is also available as a Shiny web application that you can try out here:
https://shiny.filogeneti.ca/clmp/


## Background
A genetic cluster is a subset of nucleotide or amino acid sequences that are more similar to each other than to the remaining sequences in the sample population.
For infectious diseases, a genetic cluster may correspond to an outbreak of cases that are clustered in space and/or time, which can imply that the cases are related by a common source. 


## Usage
```R
> require(clmp)
Loading required package: clmp
Loading required package: ape

> t1 <- read.tree('examples/test.nwk')  # a simulated tree with 1000 tips, 100 in clusters
> res <- clmp(t1)  # returns an ape::phylo tree object 
log likelihood for 2 state model is 2238.543290
rates: 495.368085 1305.115860 
Q: [    *   2.526691 ]
   [ 23.309483   *    ]

> index <- match(t1$tip.label, names(res$clusters))  
> labels <- grepl("_1_", t1$tip.label)  # extract truth from the tip labels
> table(labels, res$clusters[index])
      
labels    0   1
  FALSE 860   3  # false positive rate, 3/(3+860)=0.34%
  TRUE   13  98  # true positive rate, 98/(98+13)=88.2%
```

## Installation

For step-by-step instructions to compile the `clmp` R package from source, please refer to our [installation documentation](INSTALL.md).
