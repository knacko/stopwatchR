#' @import datastructures
.onLoad <- function(libname, pkgname)
{
  assign(".stopwatchR", datastructures::hashmap(key.class = c("character")), envir=baseenv())
  assign(".stopwatchR2", numeric(), envir=baseenv())
}
