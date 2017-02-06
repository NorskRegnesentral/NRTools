#' Create the md5 hash of an object
#' @description R's tools only allow one to create the md5 hash of a file.  This function takes an R object, creates a file, calculates the hash, deletes the file and returns the result.
#' @param obj Any R object that can be saved with the save command.
#' @export hash.obj
#' @return The md5 hash of an object
#' @author Alex
#' @examples
#' ## BENCHMARK
#' ## DEPENDENT: hash.obj
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
#' @export compare.benchmarks
#' @return Nothing yet.
#' @author Alex
compare.benchmarks <- function(pkg.loc="./",
                               pkg.name = tail(splitstr(normalizePath(pkg.loc),"/")[[1]],1),
                               branch.orig = "master",
                               branch.new = "dev",
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

