#' Construct plots to be animated.
#'
#' @param dat \code{data.frame} The output of a call to \code{\link{tween_microsteps}}
#'
#' @importFrom ggplot2 ggplot geom_curve geom_point theme scale_color_identity scale_size_identity
#' @importFrom dplyr select %>% left_join
#' @importFrom tidyr replace_na
#'
#' @export
create_net_animate <- function(dat){
  froms <- dat %>% select(id = from, microstep, x = from.x, y = from.y, ms, .frame, size, color) %>% unique
  tos <- dat %>% select(id = to, microstep, x = to.x, y = to.y, ms, .frame) %>% unique
  tos <- left_join(tos, froms)
  nodedata <- rbind(froms, tos) %>% unique
  nodedata <- nodedata %>% replace_na(replace = list(size = 1, color = "#333333"))

  to_ani <- ggplot()  +
    geom_curve(data = dat, aes(x=from.x, y=from.y, xend=to.x, yend=to.y, frame = .frame, colour=ecolor, size=esize), curvature=0.1,
               arrow = arrow(angle = 20, length = unit(.1, "inches"))) +
    geom_point(data = nodedata, aes(x=x, y=y, colour=color, size=size, frame = .frame)) +
    scale_colour_identity() + scale_size_identity() +
    theme_void() +
    coord_fixed()
  return(to_ani)
}
