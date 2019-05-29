
require(MASS)
require(ape)

Ntip <- function(tr) {
  length(tr$tip.label)
}

is.tip <- function(tr) {
  tr$edge[,2] <= Ntip(tr)
}

clockify <- function(tree, seq.len) {
  # Gamma-Poisson smoothing of branch lengths
  # @param tree:  object of class "phylo" (ape package)
  # @param seq.len:  length of alignment from which tree 
  #                  was reconstructed (codons)
  
  # expected number of substitutions per branch
  ens <- round(tree$edge.length * seq.len)
  
  for (tf in c(TRUE, FALSE)) {
    # subset internal and terminal branches
    index <- xor(is.tip(tree), tf)
    
    fit <- fitdistr(ens[index], 'negative binomial')
    size <- fit$est[1]  # aka shape, r, alpha
    mu <- fit$est[2]
    beta <- size/mu  # 1/theta
    
    tree$edge.length[index] <- (ens[index]/(1+beta) + 
                                  (size-1)/(1+beta)) / seq.len
  }
  
  tree  
}
