library("iSEEindex")
library("BiocFileCache")
library("yaml")

bfc <- BiocFileCache(cache = tempdir())

dataset_fun <- function() {
  x <- yaml::read_yaml(system.file(package = "iSEEindex", "example.yaml"))
  x$datasets
}

initial_fun <- function() {
  x <- yaml::read_yaml(system.file(package = "iSEEindex", "example.yaml"))
  x$initial
}

iSEEindex(bfc, dataset_fun, initial_fun)
