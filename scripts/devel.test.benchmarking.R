rm(list = ls())

pkg.loc <- "~/pkg/ShotSliceSampler/"
pkg.name <- "ShotSliceSampler"
branch.orig <- "master"
branch.new <- "dev"
verbose <- TRUE
output.loc <- "/tmp/"
datasets <- NULL

report <- NRTools::benchmark.run.comparison(pkg.loc,pkg.name)
