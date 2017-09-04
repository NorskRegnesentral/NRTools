rm(list = ls())

##--------- Setup ----------
library(NRTools)
target_pkg = "mgcv"
##--------------------------

##------ Setup -------------------------
e = as.environment(paste0("package:",target_pkg))
f_all = ls(e)
n_f = length(f_all)
A = matrix(NA,length(f_all), length(f_all))
##----------------------------------------

for(i in 1:n_f)
{
  f_str = deparse(get(f_all[i]))
  for(j in 1:n_f)
  {
    temp = grep(paste0(f_all[j], "(",b,fixed=TRUE),f_str, fixed = TRUE)    
  }
}


