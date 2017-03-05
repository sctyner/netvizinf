#' Take all possible edges and a given edgelist and "remove" the edges that aren't actually there by making the from and to node coordinates identical.
#' 
#' @param alledges \code{data.frame} Output from the \code{\link{layoutAll()}} function
#' @param edgelist \code{data.frame} Edgelist of network. Must have columns named "from" and "to"
#' 
#' @importFrom dplyr filter
#' 
#' @export
findTrueEdges <- function(alledgesLayout, edgelist){
  N <- nrow(alledgesLayout)
  for(i in 1:N){
    edge <- alledgesLayout[i,c("from","to")]
    dat <- filter(edgelist, from == edge$from & to == edge$to )
    if(nrow(dat) == 0){
      alledgesLayout[i, c("x.to", "y.to")] <- alledgesLayout[i, c("x.from", "y.from")]
    }
  }
  return(alledgesLayout)
} 
