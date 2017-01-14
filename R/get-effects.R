#' Create effects creation function for use with purrr
#'
#' @param dat data frame of the effects to include. Columns must be named shortName, type and inter1.
#'
#' @export
get_effects <- function(dat, eff_basic){
  dat <- as.data.frame(dat)
  struct <- includeEffects(eff_basic, dat$shortName,
                 type = dat$type,
                 interaction1 = dat$inter1,
                 character = TRUE)
  return(struct)
}
