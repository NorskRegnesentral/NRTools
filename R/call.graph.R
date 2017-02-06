#' Helper function to reduce the proftools call graph to limited namespace
#' 
#' @description This is a hack that takes an object from the proftools package and slims it down.  Basically the callgraph code in proftools included all sorts of R internals nonsense and had no option to reduce it.  This code does that, in a pretty aggressive way.  Unclear how this could be useful beyond the call.graph.setup routine.
#' @param p The p object crunched by proftools
#' @param e The environment agains which p should be reduced.
#' @inheritParams global_arguments (Include this IF one or more arguments are declared in the function global_arguments)
#' @export reduce.p
#' @return A new object with the exact same form as p, but with only those functions in e
#' @author Alex
#' @examples
#' data(market_95)
#' f <- "pr <- price.response(DF)"
#' pd <- prof.routine(f)
reduce.p <- function(p, e)
{
  f.include <- setdiff(c(ls(e),"FUN","lapply","mclapply"),c("prof.routine", "benchmark.package", "hash.obj"))
  sp <- strsplit(p$gNodes,"\n")
  f.p <- NULL
  for(i in 1:length(sp))
  {
    f.p[i] <- sp[[i]][1]
  }

  w.in <- which(f.p %in% f.include)
  n.in <- length(w.in)
  
  r <- list()
  r$nodes <- p$nodes[w.in]
  nms.in <- f.p[w.in]
  
  r$edges <- list()
  r$callCounts <- list()
  r$edgeL <- list()
  for(i in 1:n.in)
  {
    w <- w.in[i]
    res.i <- p$edges[[w]]
    c.i <- p$callCounts[[w]]
    w.i.in <- which(res.i %in% f.include)
    r$edgeL[[i]] <- list()
    
    if(length(w.i.in) > 0)
    {
      r$edges[[i]] <- res.i[w.i.in]
      r$callCounts[[i]] <- c.i[w.i.in]
      r$edgeL[[i]]$edges <- rep(NA, length(w.i.in))
      for(j in 1:length(w.i.in))
      {
        r$edgeL[[i]]$edges[j] <- which(nms.in == res.i[w.i.in[j]])
      }
    }else{
      r$edges[[i]] <- character(0)
      r$callCounts[[i]] <- NULL
      r$edgeL[[i]]$edges <- integer(0)
    }
  }

  r$gNodes <- p$gNodes[w.in]
  r$totalPercent <- p$totalPercent[w.in]
  r$selfPercent <- p$selfPercent[w.in]
  
  return(r)
}

#' A wrapper for proftools to profile a set of commands
#' @description This is a helper function to run a set of commands in an Rprof'ed session and return the profile data
#' @param f a string containing R code to be parsed and run.
#' @param rep An integer for the number of times to repear the f evaluation, helps capture fast routines
#' @export prof.routine
#' @return a proftools parse of the profile data
#' @author Alex
prof.routine <- function(f, rep = 1)
{
  tmp <- tempfile()
  Rprof(tmp)
  for(j in 1:rep)
  {
    benchmark.result <- eval(parse(text=f))
  }
  Rprof(NULL)
  pd <- readProfileData(tmp)
  return(pd)
}

#' Use the proftools output to create a call graph
#' @description Using the processed output from an Rprof session, this will create an graph object representing the call graph.  It is focused on a particular namespace and colors nodes according to their documentation/review states
#' 
#' @param pd The processed output of an Rprof session for a block of R code (see example)
#' @export call.graph.setup
#' @returns A Rgraphviz rendered graph object
#' @author Alex
#' @examples
#' data(market_95)
#' f <- "pr <- price.response(DF)"
#' pd <- prof.routine(f)
#' g <- call.graph.setup(pd)
call.graph.setup <- function(pd)
{

  ##------ Load Environment ---------
  e <- as.environment("package:pricemethodresearch")
  ##---------------------------------
  
  ##------- Process profiling -------
  pd <- cvtProfileData(pd, TRUE, NA, 0)
  p <- np2x(pd, "none")
  p.new <- reduce.p(p,e)
  g <- g2g(p.new, FALSE)
  ##---------------------------------

  ##------- Node setup -------------
  shape = "ellipse"
  names(labels) <- labels <- graph::nodes(g)
  p$nodeColors <- rep("red",length(labels))
  names(p$nodeColors) <- labels
  for(i in 1:length(labels)) ## Find out if we have docs
  {
    a <- suppressWarnings(example(labels[i], package="pricemethodresearch", character.only=TRUE, give.lines = TRUE))
    if(!is.null(a)) p$nodeColors[i] <- "blue"
  }
  ##---------------------------------

  ##------- Edge setup ----------------
  edges <- graph::edgeL(g)
  edgeNames <- list()
  for (i in seq(along = edges))
  {
    if (length(edges[[i]]$edges) > 0)
    {
      edgeNames[[i]] <- paste(labels[i], labels[edges[[i]]$edges],sep = "~")
    }
  }
  edgeNames <- unlist(edgeNames)
  edgeCounts <- unlist(p.new$callCounts)
  anyEdges <- (length(edgeCounts) > 0)
  ##------------------------------------

  ##------ Setup Graph  ----------------
  attrs <- list(node = list(shape = shape, fixedsize = FALSE))
  attrs$graph <- list(rankdir = "TB")
  g <- Rgraphviz::layoutGraph(g, layoutType = "dot", attrs = attrs)
  graph::nodeRenderInfo(g) <- list(col = p$nodeColors)
  ##------------------------------------

  return(g)
}
