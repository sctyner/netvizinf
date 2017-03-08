#' Create a list of data frames of network microsteps able to be \code{tween}'d.
#'
#' @param microsteps \code{list} A list of microsteps of class \code{"data.frame"} of the continuous time Markov chain from one network observation to another.
#' @param layoutParams \code{list} A named list of arguments to pass to \code{\link{seedLayout()}}. Needs elements named "n" and "directed" at minimum.
#'
#' @importFrom dplyr %>% left_join
#' @importFrom tibble add_row
#' @export
#'
pretween_vertices <- function(microsteps, layoutparams){
  B <- length(microsteps)
  changedf <- data.frame(id = character(0), ms = integer(0),
                         addedge = logical(0), rmvedge = logical(0))
  microsteps[[1]]$addedge <- FALSE
  microsteps[[1]]$rmvedge <- FALSE
  microsteps[[1]]$microstep <- 0
  for (i in 2:B){
    # create new columns for use later
    microsteps[[i]]$addedge <- FALSE
    microsteps[[i]]$rmvedge <- FALSE
    microsteps[[i]]$microstep <- i - 1
    old <- microsteps[[i-1]]
    new <- microsteps[[i]]
    # find matching rows
    newInold <- dplyr:::match_data_frame(new[,c("from", "to")], old[,c("from", "to")])
    oldInnew <- dplyr:::match_data_frame(old[,c("from", "to")], new[,c("from", "to")])
    # if any of the new in old are NA, that means there's a new edge in new that wasn't in old.
    if (anyNA(newInold)){
      #new[[which(is.na(newInold)),"addedge"]] <- TRUE
      ego <- new$from[which(is.na(newInold))]
      changedf <- changedf %>%
        add_row(id = ego, ms = unique(new$microstep), addedge = TRUE, rmvedge = FALSE)
    } else if (anyNA(oldInnew)) {
      #old[[which(is.na(oldInnew)), "rmvedge"]] <- TRUE
      ego <- old$from[which(is.na(oldInnew))]
      changedf <- changedf %>%
        add_row(id = ego, ms = unique(old$microstep), addedge = FALSE, rmvedge = TRUE)
    }
  }
  step1 <- lapply(X = microsteps, seedLayout, n = layoutparams$n)
  step2 <- lapply(X = seq_along(step1), FUN = function(i,x){
    x[[i]]$ms <- i-1
    return(x[[i]])
    }, step1
    )
  step3 <- do.call("rbind", step2)
  step3$id <- as.factor(step3$id)
  changedf$id <- factor(changedf$id, levels = levels(step3$id))
  step4 <- left_join(step3, changedf, by = c("id" = "id", "ms" = "ms")) %>%
    replace_na(list(addedge = FALSE, rmvedge = FALSE))
  final_step <- split(step4, as.factor(step4$ms))
  return(final_step)
}
