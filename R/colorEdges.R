#' Function to color the edges in a microstep animation
#'
#' @param edges_unnested \code{data.frame} Edge data that has been merged with nested node data and unnested
#' @param colorall \code{character} Character value of color for all edges when they're not changing
#' @param colorchange \code{character} Character value of color for the edges when changing
#' @param sizeall \code{numeric} Size of all edges when they're not changing
#' @param sizechange \code{numeric} Size of the edge to emphasize the change
#'
#' @importFrom tweenr tween_numeric tween_color
#' @importFrom dplyr %>% group_by mutate ungroup select
#'
#' @export
#'
colorEdges <- function(edges_unnested, colorall = 'grey20', colorchange = 'red', sizeall = .5, sizechange = 2.5){
  edges_unnested_color <- edges_unnested %>%
    mutate(ecolor = ifelse(addedge + rmvedge == 0,
                           colorall, NA),
           esize = 1
    )
  rmvidx <- which(edges_unnested_color$rmvedge != 0)
  if (length(rmvidx) > 0){
      edges_unnested_color[rmvidx,"ecolor"] <- (edges_unnested_color[rmvidx,] %>%
                                              group_by(from, to) %>%
                                              mutate(count = seq_len(n())) %>%
                                              mutate(ecolor = tween_color(c(colorchange, "white"), n = n(), ease = "quartic-in")[[1]][count]) %>% ungroup() %>% select(ecolor))[[1]]
     edges_unnested_color[rmvidx,"esize"] <- (edges_unnested_color[rmvidx,] %>%
                                             group_by(from, to) %>%
                                             mutate(count = seq_len(n())) %>%
                                             mutate(esize = tween_numeric(c(sizechange, 0), n = n(), ease = "quartic-in")[[1]][count]) %>% ungroup() %>% select(esize))[[1]]
  }
  addidx <- which(edges_unnested_color$addedge != 0)
  if (length(addidx) > 0){
      edges_unnested_color[addidx,"ecolor"] <- (edges_unnested_color[addidx,] %>%
                                              group_by(from, to) %>%
                                              mutate(count = seq_len(n())) %>%
                                              mutate(ecolor = tween_color(c("white", colorchange), n = n(), ease = "quartic-out")[[1]][count]) %>% ungroup() %>% select(ecolor))[[1]]
      edges_unnested_color[addidx,"esize"] <- (edges_unnested_color[addidx,] %>%
                                             group_by(from, to) %>%
                                             mutate(count = seq_len(n())) %>%
                                             mutate(esize = tween_numeric(c(0,sizechange), n = n(), ease = "quartic-out")[[1]][count]) %>% ungroup() %>% select(esize))[[1]]
  }
  return(edges_unnested_color)
}
