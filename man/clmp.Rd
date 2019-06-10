\name{clmp}
\alias{clmp}

\title{CLustering with a Markov-modulated Poisson process}

\description{
  \code{clmp} fits a model of "evolving" branching rate classes to the distribution of inter-node distances in a phylogenetic tree.
  Variation in these distances roughly approximates variation in transmission rates for an infectious diseases spreading through a structured host population.
}


\usage{
clmp(tree, nrates=2, bounds=c(0, 1e5, 0, 1e5), trace=FALSE, nsites=NA,
min.bl=0.2, tol=1e-3, tolhist=1e-3, seed=0)
}

\arguments{
  
  \item{tree}{an object of class "phylo" or a valid Newick tree specification string}
  
  \item{nrates}{the number of rate classes for fitting the MMPP model to the tree}
  
  \item{bounds}{an optional vector of length 4 that specifies the (1) lower and (2) upper bounds for branching rates, and the (3) lower and (4) upper bounds on transition rates. Defaults to (0, 1e5, 0, 1e5).}
  
  \item{trace}{an optional flag that defaults to FALSE.  If set to TRUE, then the C function \code{_fit_mmpp} will print updates to \code{stderr}.  Use only for debugging or if running \code{clmp} for a very large tree (over 10,000 tips) and you need to monitor progress.  
  
  You can also set \code{trace} to an integer value >0 to reduce the amount of \code{stderr} output; for example, \code{trace=10} will only output every 10th line to the \code{stderr} stream.  Note that a line is printed for every member of the population (defaults to size 100).  The composition of }
  
  \item{nsites}{(optional) sequence (alignment) length in number of sites.  This is used to rescale branch lengths to the expected numbers of substitutions across sites, so that \code{min.bl} can be applied generically to phylogenies reconstructed by maximum likelihood.}
  
  \item{min.bl}{(optional) minimum branch length as expected number of substitutions over all sites (full alignment length).  Used to avoid problems with near-zero branch lengths in trees reconstructed by maximum likelihood.  \code{nsites} must be specified for branches to be adjusted.}
  
  \item{tol}{(optional) sets CMA-ES optimization tolerance for objective function.  Default set to a relatively high value (1e-3) to adjust for likelihood scaling.  Reduce to smaller values (e.g., 1e-11) if you need greater precision in maximum likelihood parameter estimation.}
  
  \item{tolhist}{(optional) sets CMA-ES optimization tolerance in change history of objective function.  Default set to relatively high value (1e-3) to adjust for likelihood scaling.  Reduce to smaller values (e.g., 1e-12) if you need greater precision in maximum likelihood parameter estimation.}
  
  \item{seed}{(debugging only)  passes seed value to CMA-ES optimization method. Defaults to 0, which tells CMA-ES to set random seed with system clock.}
}


\details{
    \code{clmp} is a method for detecting sections of a phylogenetic tree (subset trees) where the branching rate is higher than the background level.
    When the tree relates genetic sequences derived from an infectious disease sampled from different individuals, the variation in branching rates can roughly approximate the variation in transmission rates within different host subpopulations.
    Thus, \code{clmp} represents a model-based method for extracting clusters of potentially high transmission rates from a molecular phylogeny.
    
    A Poisson process is a stochastic process where an event occurs at a constant rate  \eqn{\lambda} over time, such that the waiting time between events follows an exponential distribution.
    A Markov-modulated Poisson process (MMPP) allows the process to switch between a finite number of rate classes, \eqn{\lambda = {\lambda_1,  \lambda_2, \ldots, \lambda_m}}.
    The rate class switches (state transitions) occur at constant rates over time, where the probability of a state transition along a branch depends only on the current state (Markov property).
    Thus, the MMPP model comprises two sets of parameters: the branching rates \eqn{\lambda} and the transition rate matrix among branching rate classes \eqn{\theta}.
    
    \code{clmp} uses an evolutionary algorithm (covariance matrix adaptation evolution strategy, CMA-ES) implemented in C by Nikolaus Hansen (\url{https://github.com/CMA-ES/c-cmaes}).
    In brief, a population of candidate solutions (parameter vectors) is updated by selecting a parental subset of solutions to produce the next generation of solutions.
    Solutions in the next generation are "mutated" by drawing variates from a multivariate normal distribution centred on the parental solution, where the covariance matrix is adjusted to the local curvature of the objective function.
}


\value{

\code{clmp} returns a copy of the input tree, now strictly bifurcating and optionally rescaled and rerooted.  It also includes the following attributes:

    \item{clusters}{a named vector that contains cluster assignments for each node.  A unique cluster index is assigned to all nodes in the same non-background rate class that comprise a subset tree.}
    
    \item{loglik}{the scaled log-likelihood of the model.  This is only useful for comparison against other fits with different number of rate classes on the same tree.}
    
    \item{rates}{maximum likelihood estimates of branching rates for each rate class.  The expected inter-node distance can be obtained from 1/\eqn{\lambda}.}
    
    \item{tr.rates}{maximum likelihood estimate of the rate matrix for transitions among the rate classes}
    
    \item{states}{a named vector that represents the integer-based rate class assignments, with names corresponding to node labels.  0 is the index of the background (lowest) rate class.  These states are used to generate cluster assignments.}
    
}

\author{
The MMPP method was implemented in C by Rosemary McCloskey.
It was validated through simulation experiments and adapted as an R extension by Art Poon.

The CMA-ES method was implemented by C by Nikolaus Hansen.
}


\references{
McCloskey RM, Poon AF. \emph{A model-based clustering method to detect infectious disease transmission outbreaks from sequence variation.} PLoS Computational Biology. 2017 Nov 13;13(11):e1005868.

Hansen N. \emph{The CMA evolution strategy: a comparing review.} In: Towards a new evolutionary computation 2006 (pp. 75-102). Springer, Berlin, Heidelberg.
}

\examples{
  require(clmp)  # lazy-loads example tree struct.SIR
  
  # forward simulation of susceptible-infected-removed (SIR)
  # process in a structured population 
  summary(structSIR)
  help(structSIR)  # more info on simulation
  
  # fit one rate-class model to this tree
  res0 <- clmp(structSIR, nrates=1)
  
  # fit two rate-class model - this should take about a minute
  res <- clmp(structSIR, nrates=2)
  
  # likelihood-based model selection
  AIC(res0, res)
  BIC(res0, res)
  
  # S3 method uses ggtree to colour branches by rate class
  plot(res)
  
  # use tip labels to measure accuracy
  tip.states <- res$states[1:length(res$tip.label)]
  actual <- grepl("_1_", names(tip.states))
  table(tip.states, actual)
}



