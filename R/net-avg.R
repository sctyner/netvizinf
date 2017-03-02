#' Summarize networks simulated from a SAO model by counting edge occurences
#'
#' @param sims Output from saom_simulate.
#' @param wave integer. Which wave of the network would you like to average?
#' @importFrom dplyr %>% group_by summarise filter mutate
#' @export


net_avg <- function(sims, wave){
  N <- length(sims)
  sims2 <- sims_to_df(sims)
  sims2 %>% filter(!is.na(to) & wave == wave) %>%
    group_by(from, to) %>%
    summarise(count = n()) %>%
    mutate(weight = ifelse(from==to, 0, count)) -> sims2
  return(sims2)
}
