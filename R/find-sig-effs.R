#' Find significant effects added to the simple RSiena model.
#'
#' @param RSiena data object. A fully constructed RSiena data object, including covariates.
#' @param N Integer. The number of simulations to run in the RSiena fitting structure. Default to 1000.
#'
#' @examples
#' source("subset-friendship.R")
#' testfn <- netvizinf:::find_sig_effs(dat = mydataa)
#'
#' @export
find_sig_effs <- function(dat, N = 1000){
  require(RSiena)
  eff_basic <- getEffects(dat)
  require(dplyr)
  require(tidyr)
  require(rvest)
  require(purrr)
  effectsDocumentation(eff_basic)
  RSeffects <- (read_html("eff_basic.html") %>% html_nodes("table") %>% html_table())[[1]]

  RSeffects <- RSeffects %>%
    nest(shortName:inter1) %>%
    mutate(eff_struct = map(data , .f = get_effects, eff_basic = eff_basic)) %>%
    mutate(num_eff = map(eff_struct, .f = function(x) length(summary(x)[[2]])))
  min_num_eff <- min(unlist(RSeffects$num_eff))
  idx2test <- which(RSeffects$num_eff > min_num_eff) #only test the ones with one added effect
  n <- length(idx2test)
  myalgorithm2 <- sienaAlgorithmCreate( projname = Sys.time() , n3 = N)
  test_results <- data.frame(shortName = rep("",n), type = rep("",n), inter1 = rep("",n),
                             estimate = rep(0,n), se = rep(0,n), Waldpval = rep(0,n),
                             stringsAsFactors = FALSE)
  for (i in 1:n){
    ests_test <- siena07( myalgorithm2, data = dat,
                          effects = RSeffects$eff_struct[[idx2test[i]]], batch=TRUE,
                          verbose = TRUE, silent = FALSE)
    num_notrate <- length(ests_test$theta)
    test_vect <- rep(0,num_notrate)
    addlparm <- which(is.na(match(ests_test$effects$effectName, c("outdegree (density)", "reciprocity"))))
    test_vect[addlparm] <- 1
    if (is(try(RSienaTest::Wald.RSiena(test_vect, ests_test)), "try-error")){
      pval <- NA
    } else {
      wald_test <- RSienaTest::Wald.RSiena(test_vect, ests_test)
      pval <- wald_test$pvalue
    }
    test_results[i,] <- c(ests_test$effects$shortName[num_notrate],
                          ests_test$effects$type[num_notrate],
                          ests_test$effects$interaction1[num_notrate],
                          ests_test$theta[num_notrate],
                          ests_test$se[num_notrate],
                          pval)
  }

  return(test_results)

}
