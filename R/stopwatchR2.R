#' \code{start_time} - Starts an internal stopwatch exclusive to the calling function.
#' @details Uses a hashmap stored as an enviromental variable with a key matching the calling function name
#' to store the elapsed proc.time()
#' @return \code{start_time} returns an (invisible) \code{hashmap} with the inserted key
#' @export
start_time <- function() {
  assign("time.all", proc.time()["elapsed"], envir=.GlobalEnv)
  assign("time.split", proc.time()["elapsed"], envir=.GlobalEnv)
  invisible(start_time)
}

#' \code{split_time} - Outputs the split time exclusive the calling function
#' @details Gets the stored elapsed proc.time() from the .stopwatchR \code{hashmap} for the respective
#' calling func and outputs the time since the the previous split or start
#' @return \code{split_time} returns formatted elapsed time since start_time or last split_time
#' @export
split_time <- function() {
  time <- proc.time()["elapsed"]-get("time.split", envir=.GlobalEnv)
  assign("time.split", proc.time()["elapsed"], envir=.GlobalEnv)
  writeLines(paste(round(time,2),"sec elapsed"))
}

#' \code{stop_time} - Stops an internal stopwatch exclusive to the calling function.
#' @details Gets the stored elapsed proc.time() from the .stopwatchR \code{hashmap} for the respective
#' calling func and outputs the time since the the previous split or start. Same functionality as
#' \code{split_time} but will delete the key after execution
#' @return \code{split_time} returns formatted elapsed time since start_time or last split_time
#' @export
stop_time <- function() {
  time <- proc.time()["elapsed"]-get("time.all", envir=.GlobalEnv)
  remove("time.all", envir=.GlobalEnv)
  remove("time.split", envir=.GlobalEnv)
  writeLines(paste(round(time,2),"sec elapsed"))
}
