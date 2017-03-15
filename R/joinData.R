#' Function to join the edgelist and \code{tween}'d layout information. To be used on a \code{tween}'d data frame \code{\link[tidyr]{nest}}ed by microstep and frame.
#'
#' @param currentlayout \code{data.frame} Current layout of the network. Column names should be (at least) x, y, id, addedge, rmvedge
#' @param currentMS \code{numeric} Current microstep
#' @param microsteps \code{list} Result of \code{\link{pretween_edges}}
#'
#' @export
joinData <- function(currentlayout, currentMS, microsteps){
  if (sum(currentlayout$addedge) == 0 | currentMS == floor(currentMS)){
    appropriate_el <- microsteps[[floor(currentMS) + 1]] # bc ms starts at 0
    joinfrompoint <- left_join(appropriate_el, currentlayout, by = c("from" = "id", "addedge" = "addedge", "rmvedge" = "rmvedge"))
    names(joinfrompoint)[names(joinfrompoint) %in% c("x", "y")] <- c("x.from", "y.from")
    # addedge logical causes problems. overwrite them when they occur
    if (length(which(is.na(joinfrompoint$x.from))) > 0){
      missfromidx <- which(is.na(joinfrompoint$x.from))
      froms <- joinfrompoint$from[missfromidx]
      joinfrompoint[missfromidx, c("x.from", "y.from")] <-
        currentlayout[match(froms,currentlayout$id), c("x", "y")] %>% data.frame
    }
    # remove the logicals because they're not relevant to the TO node.
    rmvcols <- which(names(currentlayout) %in% c("addedge","rmvedge"))
    jointopoint <- left_join(joinfrompoint, currentlayout[,-rmvcols], by = c("to" = "id"))
    names(jointopoint)[names(jointopoint) %in% c("x", "y")] <- c("x.to", "y.to")
  } else if (any(currentlayout$addedge > 0)) {
    appropriate_el <- microsteps[[ceiling(currentMS) + 1]] # bc ms starts at 0
    joinfrompoint <- left_join(appropriate_el, currentlayout, by = c("from" = "id", "addedge" = "addedge", "rmvedge" = "rmvedge"))
    names(joinfrompoint)[names(joinfrompoint) %in% c("x", "y")] <- c("x.from", "y.from")
    # addedge logical causes problems. overwrite them when they occur
    if (length(which(is.na(joinfrompoint$x.from))) > 0){
      missfromidx <- which(is.na(joinfrompoint$x.from))
      froms <- joinfrompoint$from[missfromidx]
      joinfrompoint[missfromidx, c("x.from", "y.from")] <-
        currentlayout[match(froms,currentlayout$id), c("x", "y")] %>% data.frame
    }
    rmvcols <- which(names(currentlayout) %in% c("addedge","rmvedge"))
    jointopoint <- left_join(joinfrompoint, currentlayout[,-rmvcols], by = c("to" = "id"))
    names(jointopoint)[names(jointopoint) %in% c("x", "y")] <- c("x.to", "y.to")
  }
  return(jointopoint)
}
