\name{clmp}
\alias{clmp}

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
  \item{trace}{an optional flag that defaults to FALSE.  If set to TRUE, then the C function \code{_fit_mmpp} will print updates to \code{stderr}.  Use only for debugging or if running \code{clmp} for a very large tree (over 10,000 tips) and you need to monitor progress.}
  \item{min.bl}{minimum branch length as expected number of substitutions over all sites (full alignment length).  Used to avoid problems with near-zero branch lengths in trees reconstructed by maximum likelihood.}
}

\details{
    A Poisson process is a stochastic process where an event occurs at a constant rate  \eqn{\lambda} over time, such that the waiting time between events follows an exponential distribution.
    A Markov-modulated Poisson process (MMPP) allows the process to switch between a finite number of rate classes, \eqn{\lambda = {\lambda_1,  \lambda_2, \ldots, \lambda_m}}.
    The rate class switches (state transitions) occur at constant rates over time, where the probability of a state transition depends only on the current state (Markov property).
}

\value{
\code{clmp} returns a list containing the following members:
    \item{tree}{the processed input tree, now strictly bifurcating and optionally rescaled and rerooted.  It also includes a named vector (tree$clusters) that contains the integer-based rate class assignments, with names corresponding to node labels.  0 is the index of the background rate class, and }
    \item{loglik}{the scaled log-likelihood of the model.  This is only useful for comparison against other fits with different number of rate classes on the same tree.}
}
