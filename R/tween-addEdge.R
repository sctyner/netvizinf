#' A function to compute \code{\link[tweenr]{tween}}'d data for an edge addition in a microstep process animation
#'
#' @param nodes1 \code{data.frame} The node placement for the current microstep
#' @param nodes2 \code{data.frame} The node placement for the next microstep
#' @param edges \code{data.frame} The edge configuration for the next microstep
#' @param stepInfo \code{vector} The current step information as a named vector, as a subset of the result of \code{\link{getMicrostepsDF}}
#'
#' @importFrom dplyr %>% filter mutate left_join rename arrange
#' @importFrom tweenr tween_states tween_numeric_t
#' @importFrom tidyr replace_na
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
tween_addEdge <- function(nodes1, nodes2, edges, stepInfo) {
  nodes1 <- nodes1 %>% mutate(
    size=1,
    size=replace(size, id==stepInfo$from, 4),
    color = "grey20",
    color = replace(color, id==stepInfo$from, "red")
  )
  nodes2 <- nodes2 %>% mutate(
    size=1,
    color = "grey20"
  )
  nodes <- tween_states(list(nodes1, nodes2), 10, 2, "linear", 14)
  nodes <- nodes %>% mutate(id = as.character(id))

  e1 <- left_join(edges, nodes %>% select(x, y, id, ms, size, color, .frame), by=c("from"="id"))
  e1 <- rename(e1, from.x = x, from.y=y)
  e2 <- left_join(e1, nodes %>% select(x,y, id, .frame), by=c("to"="id", ".frame")) %>% rename(to.x=x, to.y=y)

  # edge data for the added edge:
  added <- e2 %>% filter(from==stepInfo$from, to==stepInfo$to) %>% arrange(.frame)
  added <- added %>% mutate(
    ecolor = "red",
    esize= 2*tween_numeric_t(c(0,1),  n = nrow(added), ease="linear")[[1]]
  )
  # exclude the edge that will be added from regular data:
  e2 <- e2 %>% filter(!(from==stepInfo$from & to==stepInfo$to))
  # and include the modified values
  e2 <- e2 %>% mutate(
    ecolor = "grey40",
    esize = 0.5
  )
  e2 <- rbind(e2, added)

  #  e2 %>% ggplot(aes(frame = .frame))  +
  #    geom_curve(aes(x=from.x, y=from.y, xend=to.x, yend=to.y, frame = .frame, colour=ecolor, size=esize), curvature=0.1) +
  #    geom_point(aes(x=from.x, y=from.y, colour=color, size=size, frame = .frame)) +
  #    scale_colour_identity() + scale_size_identity()
  e2
}
