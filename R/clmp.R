
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
  ladderize(tree2)
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

  return(list(tree=tree2, result=res))
}
