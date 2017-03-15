#' Create a list of data frames of network microsteps able to be \code{tween}'d.
#'
#' @param pte \code{list} Short for "pre-tweened edges". A list of microsteps of class \code{"data.frame"} of the continuous time Markov chain from one network observation to another. Result of \code{\link{pretween-edges}}
#' @param layoutParams \code{list} A named list of arguments to pass to \code{\link{seedLayout()}}. Needs elements named "n" and "directed" at minimum.
#'
#' @importFrom dplyr %>% left_join
#' @importFrom tibble add_row
#' @export
#'
pretween_vertices <- function(pte, layoutparams){
  step1 <- lapply(X = pte, seedLayout, n = layoutparams$n)
  step2 <- lapply(X = seq_along(step1), FUN = function(i,x){
    x[[i]]$ms <- i-1
    return(x[[i]])
    }, step1
    )
  step3 <- do.call("rbind", step2)
  step3$id <- as.factor(step3$id)
  changedf <- get_changes(pte)
  step4 <- left_join(step3, changedf, by = c("id" = "id", "ms" = "ms")) %>%
    replace_na(list(addedge = FALSE, rmvedge = FALSE))
  step4$addedge <- as.logical(step4$addedge)
  step4$rmvedge <- as.logical(step4$rmvedge)
  step4$id <- as.factor(step4$id)
  final_step <- split(step4, as.factor(step4$ms))
  return(final_step)
}
