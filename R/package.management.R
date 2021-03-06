#' Create the md5 hash of an object
#' @description R's tools only allow one to create the md5 hash of a file.  This function takes an R object, creates a file, calculates the hash, deletes the file and returns the result.
#' @param obj Any R object that can be saved with the save command.
#' @export hash.obj
#' @return The md5 hash of an object
#' @author Alex
#' @examples
#' ## BENCHMARK
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


#' Get the hash of an entire namespace
#' @export hash.namespace
hash.namespace <- function(pkg.name)
{
  ##------- Load the library -----------
  library(pkg.name, character.only=TRUE)
  ##------------------------------------
  
  ##------ Get namespace --------------
  fs = stringr::str_sort(ls(as.environment(paste0("package:",pkg.name))))
  ##-----------------------------------

  ##------- Hash ---------
  h.all <- list()
  for(i in 1:length(fs))
  {
    str.f <- fs[i]
    f.str <- eval(parse(text = str.f))
    h.all[[str.f]] <- hash.obj(f.str)
  }
  ##----------------------

  return(h.all)
}


#' Compare the output of two benchmarks
#' @export benchmark.compare.output
benchmark.compare.output <- function(pkg.name,branch.orig, branch.new,output.loc, verbose)
{
  path.orig <- paste0(output.loc,"/",pkg.name,"_",branch.orig,"/results/")
  f.orig <- list.files(path.orig)

  path.new <- paste0(output.loc,"/",pkg.name,"_",branch.new,"/results/")
  f.new <- list.files(path.new)

  missing.new <- NULL
  unequal.new <- NULL
  equal <- NULL
  for(i in 1:length(f.orig))
  {
    f <- f.orig[i]
    if(! (f %in% f.new))
    {
      missing.new <- c(missing.new, f)
    }else{
      load(paste0(path.orig,f))
      b.result.orig <- b.result
      load(paste0(path.new,f))
      if(all.equal(b.result.orig, b.result) != TRUE)
      {
        unequal.new <- c(unequal.new, f)
      }else{
        equal <- c(equal, f)
      }
    }
  }

  w.new <- which(!(f.new %in% f.orig))
  new.benchmarks <- NULL
  if(length(w.new) > 0)
  {
    new.benchmarks <- f.new[w.new]
  }

  l <- list(missing = missing.new, unequal = unequal.new, new = new.benchmarks, equal = equal)
  return(l)
  
}



#' @export benchmark.calculate.hashes
benchmark.calculate.hashes <- function(pkg.name,
                                       branch="master",
                                       output.loc="/tmp/")
{
  h.all <- hash.namespace(pkg.name)
  save(h.all, file = paste0(output.loc,"/hash_",pkg.name,"_",branch,".RData"))
}

#' @export benchmark.compare.hashed.branches
benchmark.compare.hashed.branches <- function(pkg.name,
                                              branch.orig = "master",
                                              branch.new = "dev",
                                              output.loc = "/tmp/")
{
  load(paste0(output.loc, "/hash_",pkg.name,"_",branch.orig,".RData"))
  h.orig <- h.all
  load(paste0(output.loc, "/hash_",pkg.name,"_",branch.new,".RData"))
  h.new <- h.all
  names.new <- names(h.new)
  names.orig <- names(h.orig)
  w.in.both <- which(names.orig %in% names.new)
  if(length(w.in.both) == 0)return(FALSE)
  fs.changed <- NULL
  for(j in 1:length(w.in.both))
  {
    f <- names.orig[ w.in.both[j] ]
    if(h.orig[[f]] != h.new[[f]])
    {
      fs.changed <- c(fs.changed, f)
    }
  }
  if(length(fs.changed) == 0)
  {
    return(FALSE)
  }else{
    return(fs.changed)
  }
}

#' Write benchmarks out for a comparison branch
#' @description This function goes through the entire namespace of a given package, tests if there are benchmarks in the examples code .  If there is a benchmark, the code for that benchmark, and the reulting output are saved.
#' @param datasets is the name of the datasets that should be loaded before benchmarking.
#' @export benchmark.generate.comparison
#' @return Nothing yet.
#' @author Alex
benchmark.generate.comparison <- function(pkg.loc="./",
                                          pkg.name = tail(splitstr(normalizePath(pkg.loc),"/")[[1]],1),
                                          branch = "master",
                                          output.loc = "/tmp/",
                                          verbose=TRUE,
                                          datasets=NULL,
                                          fs = NULL)
{

  ##------- Load the library -----------
  library(pkg.name, character.only=TRUE)
  ##------------------------------------
  
  ##------ Get namespace --------------
  if(is.null(fs) | fs == "")
  {
    fs = ls(as.environment(paste0("package:",pkg.name)))
  }
  ##-----------------------------------
  
  ##-------- Setup directories ------------------
  if(verbose) print("Setting up directories")
  f.name <- paste0(output.loc,"/",pkg.name,"_",branch,"/")
  if(file.exists(f.name))
  {
    system2("rm",paste0("-r ",f.name))
  }
  dir.create(f.name)
  if( !is.null(datasets) ){dir.create(paste0(f.name,"data"))}
  dir.create(paste0(f.name,"results"))
  ##---------------------------------------------
  
  ##----- Run Data code -------
  ## This needs to be generalized!
  if(length(datasets) > 0)
  {
    data(market_2)
  }
  ##---------------------------

  ##------ Run through all benchmarks --------
  #'  i <- 20
  ##for(i in sample(1:length(fs)))  ## Random to ensure no serial dependence between outputs
  for(i in 1:length(fs))  ## Random to ensure no serial dependence between outputs
  {

    str.f <- fs[i]
    a <- example(str.f, package = pkg.name, character.only=TRUE, give.lines = TRUE)
    if(is.null(a))
    {
      warning(paste0(str.f,"has no examples associated with it!"))
    }
    if(!is.null(a))
    {

      w.benchmark.start <- grep("## BENCHMARK",a)
      w.benchmark.stop <- grep("## END BENCHMARK",a)
      if(length(w.benchmark.start) > 0)
      {
        for(j in 1:length(w.benchmark.start))
        {
          if(verbose)print(paste("On Function",str.f, "benchmark",j))
          l.b <- (w.benchmark.start[j]+1):(w.benchmark.stop[j] - 1)
          b.code <- a[l.b]
##          cat(paste0(paste(b.code,collapse="\n"),"\n"))
          eval(parse(text = paste0("f.temp <- function(){", paste(b.code, collapse="\n"),"}")))
          b.result <- f.temp()
          f.out <- paste0(f.name,"results/benchmark_",str.f,"_",j,".RData")
          save(b.result, file = f.out)
        }
      }
    }
  }

  return(TRUE)
}

#' Compare the benchmark results from two versions of the repository
#' @description Note that this needs to launch separate R 
#' @export benchmark.run.comparison
benchmark.run.comparison <- function(pkg.loc="./",
                                     pkg.name = tail(strsplit(normalizePath(pkg.loc),"/")[[1]],1),
                                     branch.orig = "master",
                                     branch.new = "dev",
                                     output.loc = "/tmp/",
                                     verbose=TRUE,
                                     datasets=NULL,
                                     differences.only=TRUE)
{

  ##------ Our resulting object ---------
  report <- NULL
  report$differences.only <- differences.only
  ##-------------------------------------
  
  ##------ First check that environment is clean and committed -----
  setwd(pkg.loc)
  a.1 <- system2("git", paste0("checkout ",branch.orig),stdout=TRUE, stderr = TRUE)
  a.2 <- system2("git", paste0("checkout ",branch.new),stdout=TRUE, stderr = TRUE)
  if(!is.null(attr(a.1,"status")) | !is.null(attr(a.2,"status")) ) stop("Your develop environment is not committed, unable to switch between branches")
  ##----------------------------------------------------------------

  ##------ Hash namespace first? -----------------------------------
  if(differences.only)
  {
    for(branch in c(branch.orig, branch.new))
    {
      ##-------- Install and precalculate hashes --------------------------
      command <- paste(branch,pkg.loc,pkg.name,sep="','")
      system2("Rscript", paste0("-e \"NRTools::git.install.branch('",command,"')\"") )
      command <- paste(pkg.name, branch, output.loc,sep="','")
      system2("Rscript", paste0("-e \"NRTools::benchmark.calculate.hashes('",command, "')\""))
      ##-------------------------------------------------------------------
    }

    fs <- benchmark.compare.hashed.branches(pkg.name,branch.orig, branch.new,output.loc)
    if(fs == FALSE)
    {
      return(report)
    }
  }else{
    fs <- NULL
  }
  ##----------------------------------------------------------------


  for(b in c(branch.orig, branch.new))
  {
    ##-------- Install and save the original benchmarks ----------------
    command <- paste(b,pkg.loc,pkg.name,sep="','")
    system2("Rscript", paste0("-e \"NRTools::git.install.branch('",command,"')\"") )
    command <- paste(pkg.loc,pkg.name, b, output.loc, verbose, datasets,fs,sep="','")
    system2("Rscript", paste0("-e \"NRTools::benchmark.generate.comparison('",command, "')\""))
    ##-------------------------------------------------------------------
  }
  
  ##-------- Compare the benchmarks --------------
  report$differences.detail <- benchmark.compare.output(pkg.name,branch.orig, branch.new, output.loc, verbose)
  ##---------------------------------------------

  return(report)
  
}

#' @export benchmark.write.report
benchmark.write.report <- function(report, pkg.loc = "./", pkg.name, branch.new = "dev")
{
  a <- system2("git",paste0("checkout ",branch.new), stdout = TRUE, stderr = TRUE)

  ##--------- Hash the namespace for this report --------------
  md5 <- hash.obj(hash.namespace(pkg.name))
  descr <- readLines(paste0(pkg.loc,"/DESCRIPTION"))
  w.hash <- which(substr(descr,1,nchar("NRToolsHash:")) == "NRToolsHash:")
  if(length(w.hash) == 0)
  {
    descr <- c(descr, paste0("NRToolsHash: ",md5))
  }else{
    descr[w.hash[1]] <- paste0("NRToolsHash: ",md5)
  }
  cat(paste0(paste(descr, collapse="\n"),"\n"), file = paste0(pkg.loc,"/DESCRIPTION"), append = FALSE)
  ##-----------------------------------------------------------

  folder <- paste0(pkg.loc, "/benchmark/")
  if(!file.exists(folder))
  {
    dir.create(folder)
  }

  ##---------- Write out report -------------------------------
  report.text <- paste0("Benchmark report for branch ",branch.new,"\n")
  if(report$differences.only)
  {
    report.text <- paste0(report.text,"Benchmark run relative to source code differences only\n")
  }else{
    report.text <- paste0(report.text,"Benchmark run over all functions\n")
  }

  if(is.null(report$differences.detail))
  {
    report.text <- paste0(report.text,"No differences found in source code\n")
  }else{
    if(!is.null(report$differences.detail$unequal))
    {
      report.text <- paste0(report.text,"Following functions HAVE UNEQUAL benchmarks:\n\t", paste(report$differences.detail$unequal, collapse="\n\t"),"\n")
    }
    if(!is.null(report$differences.detail$missing))
    {
      report.text <- paste0(report.text,"Following functions MISSING from new codebase:\n\t", paste(report$differences.detail$missing, collapse="\n\t"),"\n")
    }
    if(!is.null(report$differences.detail$new))
    {
      report.text <- paste0(report.text,"Following functions NEW in new codebase:\n\t", paste(report$differences.detail$new, collapse="\n\t"),"\n")
    }
    if(!is.null(report$differences.detail$equal))
    {
      report.text <- paste0(report.text,"Following functions HAVE EQUAL benchmarks:\n\t", paste(report$differences.detail$equal,collapse="\n\t"),"\n")
    }
  }

  cat(report.text, file = paste0(folder, "benchmark_report.txt"), append= FALSE)
  ##---------------------------------------------------------------
}

#' @export benchmark.branch
benchmark.branch <- function(pkg.loc="./",
                             pkg.name = tail(strsplit(normalizePath(pkg.loc),"/")[[1]],1),
                             branch.orig = "master",
                             branch.new = "dev",
                             output.loc = "/tmp/",
                             verbose=TRUE,
                             datasets=NULL,
                             differences.only=TRUE)
{

  report <- benchmark.run.comparison(pkg.loc, pkg.name, branch.orig, branch.new,
                                     output.loc, verbose, datasets, differences.only)
  benchmark.write.report(report,pkg.loc, pkg.name, branch.new)
}

#' @export benchmark.test
benchmark.test <- function(pkg.name)
{
  library(pkg.name, character.only=TRUE)
  descr <- readLines(paste0(path.package(pkg.name),"/DESCRIPTION"))
  w.hash <- which(substr(descr,1,nchar("NRToolsHash:")) == "NRToolsHash:")
  if(length(w.hash) == 0)
  {
    return(FALSE)
  }

  md5.descr <- substr(descr[w.hash[1]], nchar("NRToolsHash:")+2, nchar(descr[w.hash[1]]))
  md5.source <- hash.obj(hash.namespace(pkg.name))
  return(md5.descr == md5.source)
}
