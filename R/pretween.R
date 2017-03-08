#' Create a list of data frames of network microsteps able to be \code{tween}'d.
#'
#' @param microsteps \code{list} A list of microsteps of class \code{"data.frame"} of the continuous time Markov chain from one network observation to another.
#' @param layoutParams \code{list} A named list of arguments to pass to \code{\link{seedLayout()}}. Needs elements named "n" and "directed" at minimum.
#'
#' @export
#'
pretween <- function(microsteps, layoutparams){
  step1 <- lapply(X = microsteps, seedLayout, n = layoutparams$n)
  step2 <- lapply(X = seq_along(step1), FUN = function(i,x){
    x[[i]]$ms <- i-1
    return(x[[i]])
    }, step1
    )
  return(step2)
}
