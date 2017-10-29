\name{clmp}

\title{CLustering with a Markov-modulated Poisson process}
\description{
  \code{clmp} assigns rate classes to branches of the input phylogeny to define clusters of high branching rates in the tree.
}

\usage{
clmp(tree, nrates=2, bounds=c(0, 1e5, 0, 1e5))
}

\arguments{
  \item{tree}{an object of class "phylo" or a valid Newick tree specification string}
  \item{nrates}{the number of rate classes for fitting the MMPP model to the tree}
  \item{bounds}{an optional vector of length 4 that specifies the (1) lower and (2) upper bounds for branching rates, and the (3) lower and (4) upper bounds on transition rates. Defaults to (0, 1e5, 0, 1e5).}
}

\details{
    A Poisson process is a stochastic process where an event occurs at a constant rate  \eqn{\lambda} over time, such that the waiting time between events follows an exponential distribution.
    A Markov-modulated Poisson process (MMPP) allows the process to switch between a finite number of rate classes, \eqn{\lambda = {\lambda_1,  \lambda_2, \ldots, \lambda_m}}.
    The rate class switches (state transitions) occur at constant rates over time, where the probability of a state transition depends only on the current state (Markov property).
  
}