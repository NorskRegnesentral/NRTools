rm(list = ls())

library(NRTools)

pkg.loc="./"
pkg.name = tail(strsplit(normalizePath(pkg.loc),"/")[[1]],1)
branch.orig = "master"
branch.new = "benchmark"
output.loc = "/tmp/"
verbose=TRUE
datasets="market_2"
differences.only=TRUE

report <- NRTools::benchmark.run.comparison(branch.new = branch.new, differences.only = differences.only)
