
clmp <- function(tree, nrates=2, bounds=c(0, 1e4, 0, 1e4), scale='none') {
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
  tree <- multi2di(tree)  # resolve polytomies
  if (!is.rooted(tree)) {
    # no root, use midpoint rooting
    tree <- midpoint(tree)
  }
  ladderize(tree)
  tree$edge.length[tree$edge.length<0] <- 0  # zero out negative branch lengths

  # create arbitrary internal node labels if not already present
  if(is.null(tree$node.label)) {
    tree$node.label <- paste0("Node", 1:Nnode(tree))
  }

  # rescale branch lengths if requested by user
  scale.factor <- 1.
  if (scale == 'mean') {
    scale.factor <- mean(tree$edge.length)
  }
  if (scale == 'median') {
    scale.factor <- median(tree$edge.length)
  }
  tree$edge.length <- tree$edge.length / scale.factor

  # serialize tree (defaults to stdout)
  nwk <- write.tree(tree)

  .Call("R_clmp", nwk, nrates, bounds, PACKAGE='clmp')
}
