#' A function to create the full set of \code{\link[tweenr]{tween}}'d microstep data appropriate for animation.
#'
#' @param ptv \code{list} A list of vertices and their locations at each microstep. The result of \code{\link{pretween_vertices}}
#' @param pte \code{list} A list of edges and their locations at each microstep. The result of \code{\link{pretween_edges}}
#' @param microsteps_df \code{data.frame}
#'
#'
#' @return A data frame with the following columns:
#' \itemize{
#'     \item \code{from} ego node
#'     \item \code{to} alter node
#'     \item \code{edge.weight} not important
#'     \item \code{addedge} is the edge being added
#'     \item \code{rmvedge} is the edge being removed
#'     \item \code{microstep} the microstep (0 is the starting network)
#'     \item \code{from.x} from node x coordinate
#'     \item \code{from.y} from node y coordinate
#'     \item \code{ms} animation microstep
#'     \item \code{size} node size in the frame
#'     \item \code{color} node color in the frame
#'     \item \code{.frame} frame id for animating
#'     \item \code{to.x} to node x coordinate
#'     \item \code{to.y} to node y coordinate
#'     \item \code{ecolor} edge color in the frame
#'     \item \code{esize} edge size in the frame
#' }
#'
#' @export
tween_microsteps <- function(ptv, pte, microsteps_df) {
  # browser()
  tween_steps <- NULL
  for (i in 1:nrow(microsteps_df)) {
    j <- microsteps_df$step[i]
    if (microsteps_df$type[i] == "remove")
      tween <- tween_removeEdge(ptv[[j]], ptv[[j+1]], pte[[j]], microsteps_df[i,])
    if (microsteps_df$type[i] == "add")
      tween <- tween_addEdge(ptv[[j]], ptv[[j+1]], pte[[j+1]], microsteps_df[i,])

    tween$.frame <- tween$.frame + ifelse(is.null(tween_steps),0,max(tween_steps$.frame))
    tween_steps <- as.data.frame(rbind(tween_steps, tween))
  }
  tween_steps
}
