#' Get a data frame of microstep changes
#'
#' @param microsteps \code{list} A list of data frames resulting from \code{\link{listMicrosteps}}
#'
#' @importFrom purrr map2_df
#' @importFrom dplyr mutate %>% arrange
#'
#' @return A data frame of microsteps with the following columns:
#' \itemize{
#'    \item \code{step} The microstep in which the change occurs
#'    \item \code{from} The ego node changing a tie
#'    \item \code{to} The alter node to which the tie from the ego node is changing
#'    \item \code{edge.weight} Unimportant. Is NA if an edge is added
#'    \item \code{type} Either "add" or "remove" indicating whether the (from,to) edge is being added or removed in that microstep
#' }
#'
#'
#' @export
getMicrostepsDF <- function(microsteps) {
  n <- length(microsteps)
  added <- purrr::map2_df(microsteps[-1], microsteps[-n], dplyr::anti_join, .id="step")
  removed <- purrr::map2_df(microsteps[-n], microsteps[-1], dplyr::anti_join, .id="step")
  if (nrow(added) > 0) added$type <- "add"
  if (nrow(removed) > 0) removed$type <- "remove"

  rbind(added,removed) %>%
    dplyr::mutate(step = as.integer(step)) %>%
    dplyr::arrange(step)
}
