#' A function to take an edglist and lay it out given a seed
#' 
#' @param edgelist \code{data.frame} Edgelist with "from" and "to" columns. Vertext names should be of the form "V1", "V2",...,"Vn".
#' @param n \code{int} Total number of edges in the network 
#' @param algorithm \code{char} Algorithm from \pkg{sna} to use. See \code{?\link[sna]{gplot.layout}} for options
#' @param layout.par \code{list} List of layout parameters. 
#' @param seed \code{int} Seed to use for layout algorithm
#' @param from \code{char} Name of the "from" column
#' @param to \code{char} Name of the "to" column
#' 
#' @export
#' 
#' 
seedLayout <- function(edgelist, n, algorithm = "spring", layout.par = NULL, seed = 9384750, from = "from", to = "to"){
  mat <- matrix(0, n, n)  
  mat[apply(edgelist[, c(from, to)], 2,readr::parse_number)] <- 1
  colnames(mat) <- paste0("V", 1:n)
  net <- as.network(mat)
  layoutFun <- getFromNamespace(paste0('gplot.layout.',algorithm), asNamespace("sna"))
  set.seed(seed)
  layout <- data.frame(layoutFun(net, layout.par = layout.par))
  names(layout) <- c("x", "y")
  # as.factor() gets rid of that odd color error in tweenr
  layout$id <- as.factor(get.vertex.attribute(net, "vertex.names"))
  return(layout)
}