#' A function to create the new edgelist resulting from a microstep in \pkg{RSiena}
#' 
#' @param dat \code{data.frame} The edgelist that is changing. Must have a "from" column and a "to" column of vertex IDs.
#' @param newedge A named vector of the new edge in the network. The nodes must be represented in the same way as in dat, and the names "from" and "to" must exist
#' 
#' @importFrom dplyr filter
#' @importFrom tibble add_row
#' 
#' @export
addMicrostep <- function(dat, newedge){
  N <- nrow(dat)
  selfie <- newedge$from == newedge$to
  check <- filter(dat, from == newedge$from & to == newedge$to)
  if (nrow(check) == 0 && !selfie){
    dat2 <- tibble::add_row(dat, from = newedge$from, to = newedge$to)
    dat2$col <- FALSE
    dat2$col[N+1] <- TRUE # color the new edge. 
  } else if (nrow(check) == 1 && !selfie){
    dat2 <- dplyr::filter(dat, !(from == newedge$from & to == newedge$to))
    dat2$col <- FALSE
  } else{
    dat2 <- dat
    dat2$col <- FALSE
  }
  return(dat2)
}
