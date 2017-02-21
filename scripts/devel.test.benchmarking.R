rm(list = ls())

library(NRTools)

pkg.loc="./"
pkg.name = tail(strsplit(normalizePath(pkg.loc),"/")[[1]],1)
branch.orig = "master"
branch.new = "benchmark"
output.loc = "/tmp/"
verbose=TRUE
datasets="market_2"
differences.only=FALSE

NRTools::benchmark.branch(branch.new = "benchmark", differences.only = FALSE)
