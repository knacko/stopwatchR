.onLoad <- function(libname, pkgname)
{
  assign(".stopwatchR", hashmap(), envir=baseenv())
}