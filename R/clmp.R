
.to.newick <- function(tree) {
  # Make sure that the tree argument is an ape phylo object
  if (class(tree)=='phylo') {
    return (write.tree(tree))
  } else if (class(tree) == 'character') {
    # make sure string is standard Newick format
    tree <- read.tree(text=tree)
    if (is.null(tree)) {
      stop(".to.newick(): String failed to parse as Newick tree string")
    }
    return (write.tree(tree))
  } else {
    stop(".to.newick(): tree argument must be a phylo or character object.")
  }
}

clmp <- function(tree, nrates=2) {
  nwk <- .to.newick(tree)
  res <- .Call("R_clmp", nwk, nrates, PACKAGE='clmp')
}
