#' A function that takes a list of microsteps creates some variables for cool visualization
#'
#' @param microsteps \code{list} A list of microsteps from \code{\link{listMicrosteps}}.
#'
#' @export
#'
pretween_edges <- function(microsteps){
  B <- length(microsteps)
  microsteps[[1]]$addedge <- FALSE
  microsteps[[1]]$rmvedge <- FALSE
  microsteps[[1]]$microstep <- 0
  for (i in 2:B){
    # create new columns for use later
    microsteps[[i]]$addedge <- FALSE
    microsteps[[i]]$rmvedge <- FALSE
    old <- microsteps[[i-1]]
    new <- microsteps[[i]]
    # find matching rows
    newInold <- dplyr:::match_data_frame(new[,c("from", "to")], old[,c("from", "to")])
    oldInnew <- dplyr:::match_data_frame(old[,c("from", "to")], new[,c("from", "to")])
    # if any of the new in old are NA, that means there's a new edge in new that wasn't in old.
    if (anyNA(newInold)){
     new[[which(is.na(newInold)),"addedge"]] <- TRUE
    } else if (anyNA(oldInnew)) {
     old[[which(is.na(oldInnew)), "rmvedge"]] <- TRUE
    } else {
      new <- new
      old <- old
    }
    microsteps[[i-1]] <- old
    microsteps[[i]] <- new
    microsteps[[i]]$microstep <- i-1
  }

  return(microsteps)
}
