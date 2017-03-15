#' Coloring vertices for animation
#'
#' @param vertexdata \code{data.frame}
#' @param pte \code{list} List of microsteps result from \code{\link{pretween_edges}}
#' @param colorall \code{character} Character value of color for all nodes when they're not changing
#' @param colorchange \code{character} Character value of color for the ego nodes when changing
#' @param sizeall \code{numeric} Size of all nodes when they're not changing
#' @param sizechange \code{numeric} Size of the ego node to emphasize the change
#'
#' @importFrom tweenr tween_color tween_numeric
#'
#' @export
#'
colorVertices <- function(vertexdata, pte, colorall = "grey40", colorchange = 'red', sizeall = 1, sizechange = 5){
  vertexdata$vcolor <- colorall
  vertexdata$vsize <- 1
  changess <- get_changes(pte)
  Nchange <- nrow(changess)
  for (i in 1:Nchange){
    chng <- changess[i,]
    if (chng$addedge){
      changecolors <- which(as.character(vertexdata$id) == as.character(chng$id) & floor(vertexdata$ms) == chng$ms-1)
      Nc <- length(changecolors)
      vertexdata[changecolors, "vcolor"] <- tween_color(c("red", "grey40"),n = Nc, ease = "quartic-in")
      vertexdata[changecolors, "vsize"] <- tween_numeric(c(sizechange, sizeall),n = Nc, ease = "quartic-in")
    }
    if (chng$rmvedge) {
      changecolors <- which(as.character(vertexdata$id) == as.character(chng$id) & floor(vertexdata$ms) == chng$ms)
      Nc <- length(changecolors)
      vertexdata[changecolors, "vcolor"] <- tween_color(c("red", "grey40"),n = Nc, ease = "quartic-in")
      vertexdata[changecolors, "vsize"] <- tween_numeric(c(sizechange, sizeall),n = Nc, ease = "quartic-in")
    }
  }
  return(vertexdata)
}
