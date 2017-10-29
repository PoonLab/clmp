
.to.newick <- function(tree) {
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
  ladderize(tree)
  # TODO: scale branches
  nwk <- write.tree(tree)
  return(nwk)
}

clmp <- function(tree, nrates=2, bounds=c(0, 1e5, 0, 1e5)) {
  nwk <- .to.newick(tree)  # serialize ape phylo object
  .Call("R_clmp", nwk, nrates, bounds, PACKAGE='clmp')
}
