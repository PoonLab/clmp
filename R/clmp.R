
clmp <- function(tree, nrates=2, bounds=c(0, 1e4, 0, 1e4), 
                 trace=FALSE, nsites=NA, min.bl=0.2) {
  # @param tree:  object of class "phylo" (ape package)
  # @param nrates:  number of lineage birth rate classes
  # @param bounds:  vector of length 4, for lower and upper bounds of 
  #                 birth and transition rates, respectively.
  # @param trace:  option to display verbose log of model optimization
  # @param min.bl:  minimum branch length in expected number of 
  #                 substitutions over all sites (full alignment length)
  
  
  # make sure <tree> is an object of class "phylo"
  if (class(tree) != 'phylo') {
    if (class(tree) == 'character') {
      # make sure string is standard Newick format
      tree <- read.tree(text=tree)
      if (is.null(tree)) {
        stop(".to.newick(): String failed to parse as Newick tree string")
      } else {
        stop(".to.newick(): tree argument must be a phylo or character object.")
      }
    }
    
    # if tree is class multiPhylo, iterate on all trees
    else if (class(tree) == 'multiPhylo') {
      # TODO: option to use mclapply?
      print(paste("Processing", length(tree), "trees..."))
      return (lapply (1:length(tree), function(i) {
        clmp(tree[[i]], nrates=nrates, bounds=bounds, trace=trace,
             nsites=nsites, min.bl=min.bl)
      }))
    }
    else {
      stop("argument <tree> must be of class character, phylo or multiphylo")
    }
  }
  
  
  # input validation
  stopifnot(is.numeric(nrates), nrates>0)
  stopifnot(is.numeric(bounds), length(bounds)==4, all(bounds>=0))
  stopifnot(is.numeric(min.bl), min.bl>=0)

  # pre-process tree
  tree2 <- multi2di(tree)  # resolve polytomies
  if (!is.rooted(tree2)) {
    # no root, use midpoint rooting
    tree2 <- midpoint(tree2)
  }
  tree2 <- ladderize(tree2)
  tree2$edge.length[tree2$edge.length<0] <- 0  # zero out negative branch lengths

  # create arbitrary internal node labels if not already present
  if(is.null(tree$node.label)) {
    tree2$node.label <- paste0("Node", 1:Nnode(tree2))
  }

  # check for near-zero branch lengths
  if (!is.na(nsites) & is.numeric(nsites)) {
    bl <- tree2$edge.length * nsites
    # substitute new minimum length for any branches below cutoff
    tree2$edge.length[bl<=min.bl] <- min.bl/nsites
  }

  # serialize tree (defaults to stdout)
  nwk <- write.tree(tree2)

  res <- .Call("R_clmp", nwk, nrates, bounds, as.double(trace), PACKAGE='clmp')
  
  # unpack outputs
  index <- match(c(tree2$tip.label, tree2$node.label), names(res[[1]]))
  tree2$clusters <- res[[1]][index]
  tree2$loglik <- res[[2]][1]
  tree2$rates <- res[[3]]
  tree2$tr.rates <- matrix(res[[4]], nrow=nrates, ncol=nrates, byrow=T)
  tree2$states <- res[[5]][index]
  
  # make rows of transition rate matrix sum to 0
  diag(tree2$tr.rates) <- -1 * apply(tree2$tr.rates, 1, sum)
  
  # for compatibility with generic AIC
  tree2$nobs <- tree2$Nnode
  tree2$df <- nrates*nrates
  
  class(tree2) <- c('clmp', class(tree2))
  tree2
}


print.clmp <- function(obj, printlen=6, ...) {
  print.phylo(obj, printlen=printlen, ...)
  if (!is.null(obj$clusters)) {
    cat('Cluster assignments:')
    if (length(obj$clusters) > printlen) {
      cat(paste("\t", paste(obj$clusters[1:printlen], collapse=", "), ", ...\n", sep=""))
    } else {
      print(obj$clusters)
    }
  }
}

summary.clmp <- function(obj, ...) {
  summary.phylo(obj, ...)
  if (is.null(obj$clusters)) {
    cat("  No cluster assignments.\n")
  } else {
    cat("  Cluster assignments:\n")
    print(table(obj$clusters))
  }
  
}

plot.clmp <- function(obj, ...) {
  if (is.null(obj$clusters)) {
    plot.phylo(obj, ...)
  } else {
    df <- fortify(obj)
    df$cluster <- obj$cluster
    ggtree(obj, aes(color=df$cluster))
  }
}

as.phylo.clmp <- function(obj, ...) {
  obj  # override as.phylo() which rejects derived class objects
}

logLik.clmp <- function(obj, ...) {
  ll <- obj$loglik
  attr(ll, 'df') <- obj$df
  ll
}



