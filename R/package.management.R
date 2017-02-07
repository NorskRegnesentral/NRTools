#' Compare the output of two benchmarks
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


#' Compare the benchmark results from two versions of the repository
#' @description Note that this needs to launch separate R 
#' @export benchmark.run.comparison
benchmark.run.comparison <- function(pkg.loc="./",
                                     pkg.name = tail(strsplit(normalizePath(pkg.loc),"/")[[1]],1),
                                     branch.orig = "master",
                                     branch.new = "dev",
                                     output.loc = "/tmp/",
                                     verbose=TRUE,
                                     datasets=NULL)
{

  for(b in c(branch.orig, branch.new))
  {
    ##-------- Install and save the original benchmarks ----------------
    command <- paste(b,pkg.loc,pkg.name,sep="','")
    system2("Rscript", paste0("-e \"NRTools::git.install.branch('",command,"')\"") )
    command <- paste(pkg.loc,pkg.name, b, output.loc, verbose, datasets,sep="','")
    system2("Rscript", paste("-e \"NRTools::benchmark.generate.comparison('",command, "')\""))
    ##-------------------------------------------------------------------
  }
  
  ##-------- Compare the benchmarks --------------
  report <- benchmark.compare.output(pkg.name,branch.orig, branch.new, output.loc, verbose)
  ##---------------------------------------------

  return(report)
  
}


#' Write benchmarks out for a comparison branch
#' @description This function goes through the entire namespace of a given package, tests if there are benchmarks in the examples code .  If there is a benchmark, the code for that benchmark, and the reulting output are saved.
#' @param datasets is the name of the datasets that should be loaded before benchmarking.
#' @export 
#' @return Nothing yet.
#' @author Alex
benchmark.generate.comparison <- function(pkg.loc="./",
                                          pkg.name = tail(splitstr(normalizePath(pkg.loc),"/")[[1]],1),
                                          branch = "master",
                                          output.loc = "/tmp/",
                                          verbose=TRUE,
                                          datasets=NULL)
{

  ##------- Load the library -----------
  library(pkg.name, character.only=TRUE)
  ##------------------------------------
  
  ##------ Get namespace --------------
  fs = ls(as.environment(paste0("package:",pkg.name)))
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
  for(i in 1:length(fs))  ## Random to ensure no serial dependence between outputs
  {

    str.f <- fs[i]
    a <- suppressWarnings(example(str.f, package = pkg.name, character.only=TRUE, give.lines = TRUE))

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
          cat(paste0(paste(b.code,collapse="\n"),"\n"))
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

