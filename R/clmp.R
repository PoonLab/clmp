
clmp <- function(tree, nrates=2, bounds=c(0, 1e4, 0, 1e4), scale='none', trace=FALSE) {
  # Make sure that the tree argument is an ape phylo object
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
  }

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

  # rescale branch lengths if requested by user
  scale.factor <- 1.
  if (scale == 'mean') {
    scale.factor <- mean(tree2$edge.length)
  }
  if (scale == 'median') {
    scale.factor <- median(tree2$edge.length)
  }
  tree2$edge.length <- tree2$edge.length / scale.factor

  # serialize tree (defaults to stdout)
  nwk <- write.tree(tree2)

  res <- .Call("R_clmp", nwk, nrates, bounds, as.double(trace), PACKAGE='clmp')

  # TODO: annotate tree with rate class assignments
  index <- match(c(tree2$tip.label, tree2$node.label), names(res))
  tree2$clusters <- res[index]
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
  # TODO: summarize rate class estimates
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
