#' Create the md5 hash of an object
#' @description R's tools only allow one to create the md5 hash of a file.  This function takes an R object, creates a file, calculates the hash, deletes the file and returns the result.
#' @param obj Any R object that can be saved with the save command.
#' @export hash.obj
#' @return The md5 hash of an object
#' @author Alex
#' @examples
#' ## BENCHMARK
#' ## hash.obj
#' set.seed(1)
#' r <- rnorm(1e5)
#' r.md5 <- hash.obj(r)
#' names(r.md5) <- NULL
#' r.md5
#' ## END BENCHMARK
hash.obj <- function(obj)
{
  ## Every once in a while, floats cause problems.
  if( is.numeric(obj) ) obj <- round(obj,6)
  if( is.list(obj) )
  {
    for(j in 1:length(obj) )
    {
      if(is.numeric(obj) )
      {
        obj[[j]] <- round(obj[[j]], 6)
      }
    }
  }
  m.sum <- digest::digest(obj)
  return(m.sum)
}

#' Benchmark all functions in a package
#' @description This function goes through the entire namespace of a given package, tests if there are benchmarks in the examples code .  The md5 hash of the function source code as well as the hashed results of any benchmarks are then stored in a new file
#' @inheritParams global_arguments (Include this IF one or more arguments are declared in the function global_arguments)
#' @export generate.benchmarks
#' @return Nothing yet.
#' @author Alex
compare.benchmarks <- function(pkg.loc="./"
                               pkg.name = tail(splitstr(normalizePath(pkg.loc),"/")[[1]],1),
                               branch.orig = "master",
                               branch.new = "dev"
                               skip.same = TRUE,
                               verbose=TRUE,
                               datasets=NULL)
{

  ##------- Setup up testing environment --------
  pkg.orig <- git.install.temp.branch(branch.orig, pkg.loc, pkg.name)
  pkg.new <- git.install.temp.branch(branch.new, pkg.loc, pkg.name)
  ##---------------------------------------------

  ##------ Get original branch namespace -----------
  fs = ls(as.environment(paste0("package:", pkg.orig)))
  ##-------------------------------------------------

  ##----- Run Data code -------
  if(length(datasets) > 0)
  {
    data(list = datasets, package = pkg.orig)
  }
  ##---------------------------

  ##------ Run through all benchmarks --------
  for(i in sample(1:length(fs)))  ## Random to ensure no serial dependence between outputs
  {
    str.f <- fs[i]

    f <- eval(parse(text = str.f))
    md5.f <- hash.obj(f)
    a <- suppressWarnings(example(str.f, package = pkg.name, character.only=TRUE, give.lines = TRUE))

    if(!is.null(a))
    {

      w.benchmark.start <- grep("## BENCHMARK",a)
      w.benchmark.stop <- grep("## END BENCHMARK",a)
      if(length(w.benchmark.start) > 0)
      {
        md5.out <- rep(NA, length(w.benchmark.start) + 1)
        md5.out[1] <- md5.f
        for(j in 1:length(w.benchmark.start))
        {
          
          if(verbose)print(paste("On Function",str.f, "benchmark",j))
          l.b <- (w.benchmark.start[j]+1):(w.benchmark.stop[j] - 1)
          b.code <- a[l.b]
          cat(paste0(paste(b.code,collapse="\n"),"\n"))
          eval(parse(text = paste0("f.temp <- function(){", paste(b.code, collapse="\n"),"}")))
          b.result <- f.temp()
          if(store.rda)
          {

          }
          md5.res <- hash.obj(b.result)
          md5.out[j + 1] <- md5.res
        }
      }else{
        md5.out <- md5.f
      }
    }else{
      md5.out <- md5.f
    }
    if(verbose){cat(paste0(paste(md5.out, collapse="\n"),"\n"))}
    
    f.out <- paste0(write.loc,"/benchmarks/",str.f)
    cat(paste0(paste(md5.out, collapse="\n"),"\n"), file = f.out, append = FALSE)
  }
}

#' Find the currently stored benchmarks and return them
#' @description For a given function, lookup the currently stored benchmarks and return them.
#' @param str.f The function name as a string
#' @param package.loc Where the source code for the package is, and benchmarks
#' @examples
#' ## BENCHMARK
#' a <- lookup.benchmarks("lookup.benchmarks")
#' a
#' ## END BENCHMARK
lookup.benchmarks <- function(str.f,
                           package.loc = "~/pkg/pricemethodresearch")
{
  f.current <- paste0(package.loc,"/benchmarks/",str.f)
  md5.benchmark <- NULL
  if(file.exists(f.current))
  {
    md5.benchmark <- readLines(f.current)
  }
  return(md5.benchmark)
}

#' Run the benchmark for a specific function and return relevant hashes
#' @description For a given function, compute the hash of the function source
#' as well as the hash of any associated benchmarks and return
#' @param str.f The function name as a string
#' @param package.loc Where the source code for the package is, and benchmarks
#' @param verbose Chatty
#' @examples
#' data(market_2)
#' ## BENCHMARK
#' a <- run.benchmarks("km")
#' a
#' ## END BENCHMARK
run.benchmarks <- function(str.f,
                           package.loc = "~/pkg/pricemethodresearch",
                           verbose = TRUE,
                           pkg.name = "pricemethodresearch")
{
  f <- eval(parse(text = str.f))
  md5.f <- hash.obj(f)
  a <- suppressWarnings(example(str.f, package = pkg.name, character.only=TRUE, give.lines = TRUE))

  if(!is.null(a))
  {
    
    w.benchmark.start <- grep("## BENCHMARK",a)
    w.benchmark.stop <- grep("## END BENCHMARK",a)
    if(length(w.benchmark.start) > 0)
    {
      md5.out <- rep(NA, length(w.benchmark.start) + 1)
      md5.out[1] <- md5.f
      for(j in 1:length(w.benchmark.start))
      {
        
        if(verbose)print(paste("On Function",str.f, "benchmark",j))
        l.b <- (w.benchmark.start[j]+1):(w.benchmark.stop[j] - 1)
        b.code <- a[l.b]
        if(verbose) cat(paste0(paste(b.code,collapse="\n"),"\n"))
        eval(parse(text = paste0("f.temp <- function(){", paste(b.code, collapse="\n"),"}")))
        b.result <- f.temp()
        md5.res <- hash.obj(b.result)
        md5.out[j + 1] <- md5.res
      }
    }else{
      md5.out <- md5.f
    }
  }else{
    md5.out <- md5.f
  }
  return(md5.out)
}
                           
#' Test a set of functions in memory against their pre-stored benchmarks
#' @description This just tests whether the current working version of a function
#' Lines up against its benchmark
#' @param str A vector of length n with the names of the functions you'd like to test
#' @param benchmark.loc The path where the package source code lives
#' @param verbose Tell me about your day
#' @return A vector of length n with the whether the benchmarks passed (TRUE) or at least one failed (FALSE) for each function
#' @examples
#' data(market_2)
#' ## BENCHMARK
#' km <- function(DF){return(DF)}
#' a <- test.benchmarks(str = "km") ## Should fail
#' a
#' ## END BENCHMARK
test.benchmarks <- function(pkg.name = "pricemethodresearch",
                            str = ls(as.environment(paste0("package:",pkg.name))),
                            benchmark.loc = "~/pkg/pricemethodresearch/",
                            verbose=TRUE)
{

  data(market_2)
  n <- length(str)
  res <- NULL

  for(i in 1:n)
  {
    str.f <- str[i]
    md5.out <- run.benchmarks(str.f, pkg.name = pkg.name)
    
    f.current <- paste0(benchmark.loc,"/benchmarks/",str.f)
    md5.benchmark <- NULL
    if(file.exists(f.current))
    {
      md5.benchmark <- readLines(f.current)
    }

    res[i] <- FALSE
    if(all.equal(md5.benchmark[-1],md5.out[-1]) == TRUE)
    {
      res[i] <- TRUE
    }
  }
  
  return(res)
}


