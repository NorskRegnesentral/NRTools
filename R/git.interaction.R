#' Find out all local branches
#' @param loc Where to look for the package
#' @return A string with the name of the current branch
#' @export git.current.branch
git.current.branch <- function(loc="./")
{
  b <- system("git symbolic-ref --short HEAD", intern = TRUE)
  return(b)
}

#' @export git.install.temp.branch
git.install.temp.branch <- function(branch.name, loc="./",
                                    pkg.name = tail(strsplit(getwd(),"/")[[1]],1))
{
  temp.name <- paste0(pkg.name,branch.name)

  ##-------- Clean up first --------
  if(file.exists(paste0("/tmp/",temp.name)))
    {
      system2("rm",paste0(" -rf /tmp/", temp.name))
    }
  ##--------------------------------

  ##--------- Checkout branch ------
  options("warn" = 2) ## In Case changes need to be checked out 
  a <- system2("git",paste0("checkout ",branch.name), stdout = TRUE, stderr = TRUE)
  options("warn" = 0)
  ##---------------------------------

  ##--------- Copy package over -----
  b <- system2("mkdir",paste0("/tmp/",temp.name,"/"),stdout = TRUE, stderr = TRUE)
  d <- system2("cp",paste0("-R ",loc," /tmp/",temp.name,"/"), stdout = TRUE, stderr = TRUE)
  ##--------------------------------

  ##------- Update to new name -----
  sed.string <- paste0("'s/Package\\: ",pkg.name,"/Package\\: ",temp.name,"/g' /tmp/",temp.name,"/DESCRIPTION")
  e <- system2("sed",paste0("-i ",sed.string), stdout = TRUE, stderr = TRUE)
  sed.string <- paste0("'s/useDynLib\\(",pkg.name,"\\)/useDynLib\\(",temp.name,"\\)/g' /tmp/",temp.name,"/NAMESPACE")
  e <- system2("sed",paste0("-i ",sed.string), stdout = TRUE, stderr = TRUE)
  ##---------------------------------
  
  ##------ Pre Install --------------
  Rcpp::compileAttributes(paste0("/tmp/",temp.name))
  a <- system2("rm",paste0(" /tmp/",temp.name,"/src/*.o"))
  a <- system2("rm",paste0(" /tmp/",temp.name,"/src/*.so"))
  ##----------------------------------

  ##------ Run Installation -----------
  a <- system2("R",paste0("CMD INSTALL /tmp/",temp.name), stdout=TRUE, stderr = TRUE)
  ##-----------------------------------
  
  return(temp.name)
}

#' @export git.install.branch
git.install.branch <- function(branch,
                               pkg.loc="./",
                               pkg.name = tail(strsplit(getwd(),"/")[[1]],1),
                               verbose = TRUE)
{

  ##------ Move to package loc ------
  if(verbose) print("Setting Up")
  curr.dir <- getwd()
  setwd(pkg.loc)
  ##---------------------------------

  ##----- Current Status? -----------

  ##---------------------------------
  
  ##--------- Checkout branch ------
  if(verbose)print(paste("Switch to branch", branch))
  options("warn" = 2) ## In Case changes need to be checked out, should fix above
  a <- system2("git",paste0("checkout ",branch), stdout = TRUE, stderr = TRUE)
  options("warn" = 0)
  ##---------------------------------

  ##------ Update documentation ---
##  if(verbose)print("Updating documentation")
  ##devtools::document()
  ##-------------------------------

  ##------ Rcpp --------------
  if(verbose)print("Forcing compile of C++ code")
  Rcpp::compileAttributes(verbose=verbose)
  a <- system2("rm","./src/*.o")
  a <- system2("rm","./src/*.so")
  ##----------------------------

  ##------ Run Installation -----------
  if(verbose)print("Installing current branch")
  a <- system2("R",paste0("CMD INSTALL ../",pkg.name), stdout=TRUE, stderr = TRUE)
  ##-----------------------------------

  ##----- Go back to where you came from ---
  setwd(curr.dir)
  ##----------------------------------------

  return(a)
}

#' @export git.uninstall.temp.branch
git.uninstall.temp.branch <- function(temp.name)
{
  if(temp.name == "")error("Empty name passed to git.uninstall.temp.branch")
  remove.packages(temp.name, .libPaths()[1])
  system2("rm",paste0(" -rf /tmp/",temp.name))
}
