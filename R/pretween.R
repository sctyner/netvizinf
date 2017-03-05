#' Create a list of data frames of network microsteps able to be \code{tween}'d.
#'
#' @param microsteps \code{list} A list of microsteps of class \code{"data.frame"} of the continuous time Markov chain from one network observation to another. 
#' @param layoutParams \code{list} A named list of arguments to pass to \code{\link{seedLayout()}}. Needs elements named "n" and "directed" at minimum.
#' 
#' @export
#' 
pretween <- function(microsteps, layoutparams){
  B <- length(microsteps)
  vertexids <- paste0("V", 1:layoutparams$n)
  if (layoutparams$directed) {
    alledges <- expand.grid(from = vertexids, to = vertexids)
    alledges <- alledges %>% filter(from!=to)
  } else {
    alledges <- data.frame(t(combn(vertexids,2, simplify = T)))
    names(alledges) <- c("from", "to")
  }
  step1 <- lapply(X = microsteps, seedLayout, n = layoutparams$n)
  step2 <- lapply(X = step1, layoutAll, alledges = alledges)
  to_tween <- list()
  for (i in 1:B){
    to_tween[[i]] <- findTrueEdges(alledgesLayout = step2[[i]], edgelist = microsteps[[i]])
  }
  to_tween[[1]]$addedge <- FALSE
  to_tween[[1]]$rmvedge <- FALSE
  to_tween[[1]]$from <- as.factor(to_tween[[1]]$from)
  to_tween[[1]]$to <- as.factor(to_tween[[1]]$to)
  to_tween[[1]]$microstep <- 0
  for (i in 2:B){
    # create new columns for use later
    to_tween[[i]]$addedge <- FALSE
    to_tween[[i]]$rmvedge <- FALSE
    # compare the differences. the one that isn't identical moving from i-1 to i is the different edge
    idx <- which(!((to_tween[[i-1]]$x.from == to_tween[[i-1]]$x.to) == (to_tween[[i]]$x.from == to_tween[[i]]$x.to))) 
    # if below is false, the edge disappears in i from i-1, not arrives
    # if below is true, the edge appears
    if (to_tween[[i-1]]$x.from[idx] == to_tween[[i-1]]$x.to[idx]){
      to_tween[[i]]$addedge[idx] <- TRUE
    } else {
      to_tween[[i]]$rmvedge[idx] <- TRUE
    }
   # final step for tween_states - ids need to be factors, not chars
    to_tween[[i]]$from <- as.factor(to_tween[[i]]$from)
    to_tween[[i]]$to <- as.factor(to_tween[[i]]$to)
    to_tween[[i]]$microstep <- i-1
  }
  return(to_tween)
}
