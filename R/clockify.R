
require(MASS)
require(ape)

is.tip <- function(tr) {
  tr$edge[,2] <= Ntip(tr)
}


clockify <- function(tree, seq.len, sample=FALSE) {
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
    
    if (sample) {
      # sample rate param from gamma-poisson distribution
      tree$edge.length[index] <- sapply(
        tree$edge.length[index]*seq.len, function(y) {
          rgamma(n=1, shape=size+y, rate=beta+1)/seq.len
        })
    } else {
      # assign MLE
      tree$edge.length[index] <- (tree$edge.length[index]*seq.len/(1+beta) + 
                                    (size-1)/(1+beta)) / seq.len  
    }
    
  }
  
  tree  
}
