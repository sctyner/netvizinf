#' A function to join a layout of a network with all possible edges in that network
#' 
#' @param layout \code{data.frame} A data frame of a network layout, created from \code{\link{seedLayout}}
#' @param alledges \code{data.frame} A data frame with "from" and "to" named columns that contains all possible nodes in a network
#' 
#' @importFrom dplyr %>% filter left_join
#' 
#' @export
layoutAll <- function(layout, alledges){
  alledgesLayout <- suppressMessages(left_join(alledges, layout, by = c("from" = "id")))
  names(alledgesLayout)[3:4] <- c("x.from", "y.from")
  alledgesLayout <- suppressMessages(left_join(alledgesLayout, layout, by = c("to" = "id")))
  names(alledgesLayout)[5:6] <- c("x.to","y.to")
  return(alledgesLayout)
}