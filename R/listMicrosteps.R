#' Take an initial network edgelist and a data frame of microsteps and create a list of edgelists of the same length as the number of microsteps + 1. 
#' 
#' @param dat The initial observed network edgelist. Must have a "from" column and a "to" column of vertex IDs.
#' @param microsteps A dataframe of output from \link{get_chain_info()}. Should be subset to the period of interest and have columns named "from" and "to". 
#' 
#' @export
listMicrosteps <- function(dat, microsteps){
  iters <- NULL
  iters[[1]] <- dat
  B <- nrow(microsteps)
  for (i in 2:(B+1)){
    iters[[i]] <- addMicrostep(iters[[(i-1)]], microsteps[(i-1),])
  }
  return(iters)
}
