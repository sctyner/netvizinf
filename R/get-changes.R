#' A function to get a data frame of vertices that change in the microsteps
#'
#' @param pte \code{list} A list of microstep edgelists. Created from \code{\link{pretween_edges}}
#'
#' @export
get_changes <- function(pte){
#  require(dplyr,lib.loc = "/Library/Frameworks/R.framework/Versions/3.4/Resources/oldpkgs")
  B <- length(pte)
  changedf <- data.frame(id = character(0), ms = integer(0),
                         addedge = logical(0), rmvedge = logical(0))
  # pte[[1]]$addedge <- FALSE
  # pte[[1]]$rmvedge <- FALSE
  # pte[[1]]$microstep <- 0
  for (i in 2:B){
    # create new columns for use later
    # pte[[i]]$addedge <- FALSE
    # pte[[i]]$rmvedge <- FALSE
    # pte[[i]]$microstep <- i - 1
    old <- pte[[i-1]]
    new <- pte[[i]]
    # find differences:
    new_not_in_old <- dplyr:::setdiff_data_frame(new[,c("from", "to")], old[,c("from", "to")])
    old_not_in_new <- dplyr:::setdiff_data_frame(old[,c("from", "to")], new[,c("from", "to")])
    # find matching rows
    #newInold <- dplyr050:::match_data_frame(new[,c("from", "to")], old[,c("from", "to")])
    #oldInnew <- dplyr050:::match_data_frame(old[,c("from", "to")], new[,c("from", "to")])
    # if nrow(new_not_in_old) > 0, there is a new edge
    # if nrow(old_not_in_new) > 0, there is a removed edges
    if (nrow(new_not_in_old) > 0){
      idx <- which(new$from == new_not_in_old$from & new$to == new_not_in_old$to)
      ego <- new$from[idx]
      changedf <- changedf %>%
            add_row(id = ego, ms = unique(new$microstep), addedge = TRUE, rmvedge = FALSE)
    } else if (nrow(old_not_in_new) > 0){
      idx <- which(old$from == old_not_in_new$from & old$to == old_not_in_new$to)
      ego <- old$from[idx]
      changedf <- changedf %>%
             add_row(id = ego, ms = unique(old$microstep), addedge = FALSE, rmvedge = TRUE)
    }

    # if any of the new in old are NA, that means there's a new edge in new that wasn't in old.
    # if (anyNA(newInold)){
    #   #new[[which(is.na(newInold)),"addedge"]] <- TRUE
    #   ego <- new$from[which(is.na(newInold))]
    #   changedf <- changedf %>%
    #     add_row(id = ego, ms = unique(new$microstep), addedge = TRUE, rmvedge = FALSE)
    #
    # } else if (anyNA(oldInnew)) {
    #   #old[[which(is.na(oldInnew)), "rmvedge"]] <- TRUE
    #   ego <- old$from[which(is.na(oldInnew))]
    #   changedf <- changedf %>%
    #     add_row(id = ego, ms = unique(old$microstep), addedge = FALSE, rmvedge = TRUE)
    # }
  }
  changedf$id <- as.factor(changedf$id)
  return(changedf)
}
