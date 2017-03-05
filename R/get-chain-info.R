#' Function to extract chains from a \code{\link[RSiena]{sienaFit}} object
#' 
#' @param ans \code{sienaFit} A \code{\link[RSiena]{sienaFit}} object resulting from a \code{\link[RSiena]{siena07}} call.
#' 
#' @importFrom plyr ldply rbind.fill
#' @export
#' 
get_chain_info <- function(ans){
  L <- length(ans$chain)
  M <- length(ans$chain[[1]][[1]])
  bigres <- NULL
  res <- data.frame()
  for (l in 1:L){
    res <- data.frame()
    for (m in 1:M){
      sub <- data.frame(ldply(
        lapply(ansnull$chain[[l]][[1]][[m]], unlist),
        rbind), stringsAsFactors = FALSE)
      sub$rep <- l
      sub$period <- m
      res <- rbind(res, sub)
    }
    bigres[[l]] <- res  
  }
  ret <- rbind.fill(bigres)
  return(ret)
}