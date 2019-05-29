# port of phangorn::bip phylo related function
# file found at https://github.com/KlausVigo/phangorn/blob/master/R/phylo.R

bip <- function(x) {
  x <- reorder(x, "postorder")
  nTips <- as.integer(length(x$tip.label))
  .Call("_clmp_bipCPP", PACKAGE = "clmp", x$edge, nTips)
}



# port of phangorn::oneWise phylo related function
# file found at https://github.com/KlausVigo/phangorn/blob/master/R.treedist.R

oneWise <- function(x, nTips = NULL) {
  if (is.null(nTips)) nTips <- length(x[[1L]])
  v <- 1:nTips
  for (i in seq_along(x)) {
    y <- x[[i]]
    if (y[1] != 1)
      y <- v[-y]
    if (y[1] != 1)
      y <- v[-y]
    x[[i]] <- y
  }
  x
}



# port of phangorn::splits and phangorgn::changeOrder phylo related function
# file found at https://github.com/KlausVigo/phangorn/blob/master/R/splits.R

as.splits <- function(x, ...) {
  if (inherits(x, "splits")) return(x)
  UseMethod("as.splits")
}

as.splits.phylo <- function(x, ...) {
  if (hasArg(as.is))
    as.is <- list(...)$as.is
  else as.is <- TRUE
  result <- bip(x)
  if (!is.null(x$edge.length)) {
    edge.weights <- numeric(max(x$edge))
    edge.weights[x$edge[, 2]] <- x$edge.length
    attr(result, "weights") <- edge.weights
  }
  if (!is.null(x$node.label)) {
    conf <- x$node.label
    if (is.character(conf)) conf <- as.numeric(conf)
    if (!as.is) if (max(na.omit(conf)) > (1 + 1e-8)) conf <- conf / 100
    attr(result, "confidences") <- c(rep(NA_real_, length(x$tip.label)), conf)
  }
  attr(result, "labels") <- x$tip.label
  class(result) <- c("splits", "prop.part")
  result
}



changeOrder <- function(x, labels) {
  oldL <- attr(x, "labels")
  ind <- match(oldL, labels)
  for (i in seq_along(x))
    x[[i]] <- sort(ind[x[[i]]])
  if (!is.null(attr(x, "cycle")))
    attr(x, "cycle") <- ind[attr(x, "cycle")]
  attr(x, "labels") <- labels
  x
}




# port of phangorn::networx phylo related function
# file found at https://github.com/KlausVigo/phangorn/blob/master/R/networx.R

addConfidences <- function(x, y, ...) UseMethod("addConfidences")

addConfidencesMultiPhylo <- function(spl, trees) {
  fun <- function(spl, intersect_labels) {
    spl2 <- spl
    index <- match(attr(spl, "labels"), intersect_labels)
    attr(spl2, "labels") <- intersect_labels
    for (i in seq_along(spl2)) {
      spl2[[i]] <- sort(na.omit(index[spl[[i]]]))
    }
    l_spl <- lengths(spl2)
    l <- length(intersect_labels)
    ind <- which((l_spl > 1) & (l_spl < (l - 1L)))
    if (length(ind) == 0) return(NULL)
    list(spl = spl2[ind], index = ind)
  }
  
  spl_labels <- attr(spl, "labels")
  zaehler <- numeric(length(spl))
  nenner <- numeric(length(spl))
  for (i in seq_along(trees)) {
    intersect_labels <- intersect(trees[[i]]$tip.label, spl_labels)
    if (length(intersect_labels) > 3) {
      tmp <- fun(spl, intersect_labels)
      if (!is.null(tmp)) {
        tree_spl <- as.splits(trees[[i]])
        if (!identical(intersect_labels, trees[[i]]$tip.label))
          tree_spl <- fun(tree_spl, intersect_labels)[[1]]
        comp <- compatible_2(as.bitsplits(tmp[[1]]), as.bitsplits(tree_spl))
        ind <- tmp$index
        zaehler[ind] <- zaehler[ind] + comp
        nenner[ind] <- nenner[ind] + 1L
      }
    }
  }
  confidences <- zaehler / nenner
  attr(spl, "confidences") <- confidences
  spl
}



addConfidences.splits <- function(x, y, scaler = 1, ...) {
  if (hasArg(add))
    add <- list(...)$add
  else add <- FALSE
  
  tiplabel <- attr(x, "label")
  nTips <- length(tiplabel)
  #    x = addTrivialSplits(x)
  if (inherits(y, "phylo")) {
    ind <- match(tiplabel, y$tip.label)
    if (any(is.na(ind)) | length(tiplabel) != length(y$tip.label))
      stop("trees have different labels")
    y$tip.label <- y$tip.label[ind]
    ind2 <- match(seq_along(ind), y$edge[, 2])
    y$edge[ind2, 2] <- order(ind)
  }
  if (inherits(y, "multiPhylo")) {
    if (inherits(try(.compressTipLabel(y), TRUE), "try-error")) {
      res <- addConfidencesMultiPhylo(x, y)
      return(res)
    }
  }
  
  spl <- as.splits(y)
  spl <- changeOrder(spl, tiplabel)
  spl <- oneWise(spl, nTips)
  ind <- match(oneWise(x, nTips), spl)
  #    pos <-  which(ind > nTips)
  pos <-  which(!is.na(ind))
  confidences <- rep(NA_real_, length(x)) # numeric(length(x))  #character
  confidences[pos] <- attr(spl, "confidences")[ind[pos]] * scaler
  if (add == TRUE) confidences <- paste(prettyNum(attr(x, "confidences")),
                                        prettyNum(confidences * scaler), sep = "/")
  #        y$node.label[ind[pos] - nTips]
  attr(x, "confidences") <- confidences
  x
}



addConfidences.phylo <- function(x, y, ...) {
  #    call <- x$call
  if (hasArg(as.is))
    as.is <- list(...)$as.is
  else as.is <- TRUE
  nTips <- length(x$tip.label)
  spl <- as.splits(x) %>% oneWise(nTips = nTips)
  conf <- attr(addConfidences(spl, y), "confidences")
  l <- lengths(spl)
  if (is.character(conf)) conf <- as.numeric(conf)
  ind <- (l == 1L) | (l == (nTips - 1L)) | (l == nTips)
  conf[ind == TRUE] <- NA_real_
  nTips <- length(x$tip.label)
  if (!as.is) conf <- conf * 100
  x$node.label <- conf[-c(1:nTips)]
  x
}



# port of phangorn::midpoint function and related helper functions
# file found at  https://github.com/KlausVigo/phangorn/blob/master/R/treeManipulation.R
# t4 <- read.tree(text="(((A:0.1,B:0.2):0.3,C:0.2):0.1,D:0.3):0;")
# t5 <- read.tree(text="(((A:0.4,B:0.2):0.15,C:0.25):0.05,D:0.3):0;")

midpoint <- function(tree, node.labels = "support", ...) {
  UseMethod("midpoint")
}


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
  }else{
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

