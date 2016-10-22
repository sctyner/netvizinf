#' Create a function to randomly place a data plot among null plots.
#'
#' @param sims data frame from the sims_to_df function. Only one wave at a time permitted, so it must be subset. Must have columns from, to, and sim at least.
#' @param dat the data you wish to plot among the sims randomly. Must have same column names as the sims data. The sim column must have the value "dat".
#' @param sd seed. Use for setting a random seed for reproducibility. Defaults to NULL.
#'
#' @examples
#' M1sims5df <- sims_to_df(M1sims5)
#' wave2 <- read.table("data/s50-network2.dat")
#' wave2$from <- 1:50
#' tidyr::gather(wave2, key = to, value = tie, V1:V50) %>%
#' dplyr::mutate(to = readr::parse_number(to)) %>%
#' dplyr::filter(tie == 1) -> wave2df
#' wave2df <- merge(wave2df, data.frame(id = 1:50), by.x = "from", by.y = "id", all = T)
#' wave2df$tie <- "dat"
#' names(wave2df)[3] <- "sim"
#' M1sims5df <- M1sims5df[c(1,2,5)]
#' test <- create_lineup(M1sims5df, wave2df)
#'
#' @export
create_lineup <- function(sims, dat, sd = NULL){
  if (sum(names(sims) != names(dat))>0){
    return("Error: sims and dat do not have the same column names.")
  }
  dat_for_lu <- rbind(sims, dat)
  M <- max(sims$sim) + 1
  if (!is.null(sd)){
    set.seed(sd)
  }
  dat_for_lu$plot_order <- rep(sample(M), as.vector(table(dat_for_lu$sim, useNA = 'ifany')))
  ans <- unique(subset(dat_for_lu, sim == "dat")$plot_order)
  require(geomnet)
  p <- ggplot(data = dat_for_lu) +
    geom_net(aes(from_id = from, to_id = to), color = 'black', ecolour = 'grey60', arrowgap = .02,
             size = 3, linewidth = 1, fiteach = T, directed = T, arrowsize = .5) +
    facet_wrap(~plot_order) +
    theme_net() + theme(panel.background = element_rect(fill = NA, color = 'black'))
 return(list(p = p, dat_plot = ans))
}
