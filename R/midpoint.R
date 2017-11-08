# port of phangorn::midpoint function and related helper functions
# file found at  https://github.com/KlausVigo/phangorn/blob/master/R/treeManipulation.R
# t4 <- read.tree(text="(((A:0.1,B:0.2):0.3,C:0.2):0.1,D:0.3):0;")
# t5 <- read.tree(text="(((A:0.4,B:0.2):0.15,C:0.25):0.05,D:0.3):0;")

midpoint <- function(tree, node.labels = "support", ...)
  UseMethod("midpoint")



midpoint.phylo <- function(tree, node.labels = "support", ...){
  oldtree <- tree
  tree <- unroot(tree)   
  nTips <- length(tree$tip.label)
  maxD1 <- node2root(tree)[1:nTips] 
  ind <- which.max(maxD1)
  tmproot <- Ancestors(tree, ind, "parent")
  nTips  <- length(tree$tip.label)
  if(tmproot>nTips) tree <- root(tree, node=tmproot)
  else  tree <- root(tree, tmproot)   
  el <- numeric(max(tree$edge))
  el[tree$edge[,2]] <- tree$edge.length  
  maxdm <- el[ind]
  tree$edge.length[tree$edge[,2]==ind] <- 0 
  maxD1 <- node2root(tree)[1:nTips]  
  tree$edge.length[tree$edge[,2]==ind] <- maxdm 
  ind <- c(ind, which.max(maxD1) ) 
  maxdm <- maxdm + maxD1[ind[2]]    
  rn <- max(tree$edge)+1
  edge <- tree$edge
  el <- tree$edge.length
  children <- tree$edge[,2]
  left <- match(ind[1], children)
  tmp <- Ancestors(tree, ind[2], "all")
  tmp <- c(ind[2], tmp[-length(tmp)]) 
  right <- match(tmp, children)
  if(el[left]>= (maxdm/2)){
    edge <- rbind(edge, c(rn, ind[1]))       
    edge[left,2] <- rn 
    el[left] <- el[left] - (maxdm/2)
    el <- c(el, maxdm/2) 
  }
  else{
    sel <- cumsum(el[right]) 
    i <- which(sel>(maxdm/2))[1]
    edge <- rbind(edge, c(rn, tmp[i]))       
    edge[right[i],2] <- rn  
    eltmp <-  sel[i] - (maxdm/2)
    el <- c(el, el[right[i]] - eltmp)
    el[right[i]] <- eltmp
  }
  tree$edge.length <- el
  tree$edge <- edge
  tree$Nnode <- tree$Nnode+1
  attr(tree, "order") <- NULL
  tree <- reroot(tree, rn) 
  if(!is.null(tree$node.label)){
    node.label <- tree$node.label
    tmp <- node.label[1]
    node.label[1] <- node.label[rn-nTips]
    node.label[rn-nTips] <- tmp
    node.label[is.na(node.label)] <- ""
    tree$node.label <- node.label
  }
  attr(tree, "order") <- NULL
  tree <- reorder(tree, "postorder")
  #    tree <- reorder(reroot(tree, rn), "postorder")
  #    if(!is.null(oldtree$node.label))tree <- addConfidences.phylo(tree, oldtree)
  if(!is.null(oldtree$node.label)){
    type <- match.arg(node.labels, c("support", "label", "delete"))
    if(type=="support") tree <- addConfidences.phylo(tree, oldtree)
    if(type=="delete") tree$node.label <- NULL
  }
  tree 
}



midpoint.multiPhylo <- function(tree, node.labels = "support", ...){
  if(!is.null(attr(tree, "TipLabel"))) compress <- TRUE
  else compress <- FALSE
  tree <- lapply(tree, midpoint.phylo, node.labels = node.labels)
  class(tree) <- "multiPhylo"
  if(compress) tree <- .compressTipLabel(tree)
  tree
}



# renames root node 
reroot <-  function (tree, node) 
{
  anc <-  Ancestors(tree, node, "all")
  l <-  length(anc)
  if(is.na(match(node,tree$edge[,1])))stop("node not in tree")
  if(l==0)return(tree)
  ind <-  match(c(node, anc[-l]), tree$edge[, 2])
  tree$edge[ind, c(1, 2)] <-  tree$edge[ind, c(2, 1)]
  root <-  anc[l]
  tree$edge[tree$edge == root] <-  0L
  tree$edge[tree$edge == node] <-  root
  tree$edge[tree$edge == 0L] <-  node
  # needed for unrooted trees    
  tree <- collapse.singles(tree)
  attr(tree, "order") <- NULL
  reorder(tree, "postorder")
}



# distance from node to root
node2root <- function(x){
  x <- reorder(x, "postorder")
  el <- numeric(max(x$edge))   
  parents <- x$edge[, 1]
  child <- x$edge[, 2]
  el[child] <- x$edge.length  
  l <- length(parents)
  res <- numeric(max(x$edge))
  for(i in l:1){            
    res[child[i]] <- el[child[i]]  + res[parents[i]]
  } 
  res
}


allAncestors <- function(x){
  x <- reorder(x, "postorder")
  parents <- x$edge[, 1]
  child <- x$edge[, 2]
  l <- length(parents)
  res <- vector("list",max(x$edge))
  for(i in l:1){
    pa <- parents[i]  
    res[[child[i]]] <- c(pa, res[[pa]])
  } 
  res
}


Ancestors <- function (x, node, type = c("all", "parent")) 
{
  parents <- x$edge[, 1]
  child <- x$edge[, 2]
  pvector <- integer(max(x$edge)) # parents
  pvector[child] <- parents    
  type <- match.arg(type)
  if (type == "parent") 
    return(pvector[node])
  anc <- function(pvector, node){
    res <- numeric(0)
    repeat {
      anc <- pvector[node]
      if (anc == 0) break
      res <- c(res, anc)
      node <- anc
    }
    res
  }
  if(!missing(node) && length(node)==1) return(anc(pvector, node))
  else allAncestors(x)[node] 
}

