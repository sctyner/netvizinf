#' Function to extract chains from a \code{\link[RSiena]{sienaFit}} object
#'
#' @param ans \code{sienaFit} A \code{\link[RSiena]{sienaFit}} object resulting from a \code{\link[RSiena]{siena07}} call.
#'
#' @return Data frame with 8 columns containing information on the chain object. See Details.
#'
#' @section Details:
#' The colums of the resulting data frame are:
#' \itemize{
#'    \item \code{ego} - The acting node of the microstep
#'    \item \code{alter} - The receiving node of the microstep
#'    \item \code{recipRate} - The reciprocal of the aggregate rate function in the current wave/period
#'    \item \code{logEgoProb} - Log probability of choosing the ego node
#'    \item \code{logAlterProb} - Log probability of choosing that alter node given the chosen ego node
#'    \item \code{diag} - A logical value indicating if the ego node equals the alter node, called a "diagonal" network in RSiena
#'    \item \code{rep} - The repetition number. Each microstep process from the initial network to the next wave is repeated 1000 times. \code{rep} runs from 1-1000.
#'    \item \code{period} - The period number indicates the wave that is being simulated toward. If \code{period} = m, then the microsteps simulate from observation \eqn{t_m} to observation \eqn{t_{m+1}}
#' }
#'
#' @importFrom plyr ldply rbind.fill
#'
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
      sub <- sub[,-c(1:3,6,10)]
      sub$X7 <- as.numeric(as.character(sub$X7))
      sub$X8 <- as.numeric(as.character(sub$X8))
      sub$X9 <- as.numeric(as.character(sub$X9))
      sub$rep <- l
      sub$period <- m
      res <- rbind(res, sub)
    }
    bigres[[l]] <- res
  }
  ret <- rbind.fill(bigres)

  names(ret) <- c("ego", "alter", "recipRate", "logEgoProb", "logAlterProb",
                  "diag", "rep", "period")

  return(ret)
}
