#' \code{start_time} - Starts an internal stopwatch exclusive to the calling function.
#' @details Uses a hashmap stored as an enviromental variable with a key matching the calling function name
#' to store the elapsed proc.time()
#' @return \code{start_time} returns an (invisible) \code{hashmap} with the inserted key
#' @export
start_time <- function(key = NULL) {
  stopwatchR <- get(".stopwatchR", envir=baseenv())

  if (!is.null(key)) {
    datastructures::insert(stopwatchR, key, proc.time()["elapsed"])
  } else {
    call.func <- paste(as.list(sys.call(-1))[[1]])
    datastructures::insert(stopwatchR, call.func, proc.time()["elapsed"])
  }
  invisible(start_time)
}

#' \code{split_time} - Outputs the split time exclusive the calling function
#' @details Gets the stored elapsed proc.time() from the .stopwatchR \code{hashmap} for the respective
#' calling func and outputs the time since the the previous split or start
#' @return \code{split_time} returns formatted elapsed time since start_time or last split_time
#' @export
split_time <- function(key = NULL) {
  stopwatchR <- get(".stopwatchR", envir=baseenv())

  if (!is.null(key)) {
    time <- proc.time()["elapsed"]-unlist(datastructures::at(stopwatchR,key))
    datastructures::insert(stopwatchR, call.func, proc.time()["elapsed"])
  } else {
    time <- proc.time()["elapsed"]-find_parent_call_time()
    datastructures::insert(stopwatchR, call.func, proc.time()["elapsed"])
  }

  return(paste(sprintf(time, fmt = '%#.2f'),"sec elapsed"))
}

#' \code{stop_time} - Stops an internal stopwatch exclusive to the calling function.
#' @details Gets the stored elapsed proc.time() from the .stopwatchR \code{hashmap} for the respective
#' calling func and outputs the time since the the previous split or start. Same functionality as
#' \code{split_time} but will delete the key after execution
#' @return \code{split_time} returns formatted elapsed time since start_time or last split_time
#' @export
stop_time <- function(key = NULL) {
  stopwatchR <- get(".stopwatchR", envir=baseenv())

  if(!is.null(key)) {
    time <- proc.time()["elapsed"]-unlist(datastructures::at(stopwatchR,key))
    datastructures::erase(stopwatchR, key)
  } else {
    call.func <-  paste(as.list(sys.call(-1))[[1]])
    time <- proc.time()["elapsed"]-find_parent_call_time()
    datastructures::erase(stopwatchR, call.func)
  }

  return(paste(sprintf(time, fmt = '%#.2f'),"sec elapsed"))
}

#' \code{stop_time} - Stops an internal stopwatch exclusive to the calling function.
#' @details Gets the stored elapsed proc.time() from the .stopwatchR \code{hashmap} for the respective
#' calling func and outputs the time since the the previous split or start. Same functionality as
#' \code{split_time} but will delete the key after execution
#' @return \code{split_time} returns formatted elapsed time since start_time or last split_time
#' @export
find_parent_call_time <- function() {
  i = 0

  while(TRUE) {

    i <- i-1

    halt = FALSE

    tryCatch( {
      call <- paste(as.list(sys.call(i))[[1]])
    },
    error=function(cond) {
      stop("Function call stack does not contain a started stopwatchR. Has start_time() been run?")
    })

    tryCatch( {
      time <- unlist(datastructures::at(stopwatchR,call))
      break
    },
    error=function(cond) {
      #message(paste(call,"was not found. Checking parent function.")))
    })

    if (i < -20) stop("Likely infinite loop")
  }

  #message("Found timer at ",call)

  return(time)
}



