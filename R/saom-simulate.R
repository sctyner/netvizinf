#' Function for simulating from a stochastic actor-oriented model.
#'
#' @param dat A Siena data object. For more information see ?RSiena::sienaDataCreate
#' @param struct A Siena effects structure object. Include all effects you wish to include in the model. See ?RSiena::getEffects for more.
#' @param parms Numeric vector of effects. The rate parameters must be listed first, followed by the effects in the objective function.
#' @param N Integer. Number of simulations you desire from the model.
#'
#' @examples
#' \dontrun{
#' # data set up from \url{https://www.stats.ox.ac.uk/~snijders/siena/basicRSiena.r}
#' library(RSiena)
#' friend.data.w1 <- as.matrix(read.table("data/s50-network1.dat"))
#' friend.data.w2 <- as.matrix(read.table("data/s50-network2.dat"))
#' friend.data.w3 <- as.matrix(read.table("data/s50-network3.dat"))
#' drink <- as.matrix(read.table("data/s50-alcohol.dat"))
#' friendshipData <- array(c(friend.data.w1, friend.data.w2, friend.data.w3), dim = c(50, 50, 3))
#' friendship <- sienaDependent(friendshipData)
#' alcohol <- varCovar(drink)
#' mydata <- sienaDataCreate(friendship, alcohol)
#' M1eff <- getEffects(mydata)
#' load("data/M1ests_bigfriends.rda")
#' M1parms <- colMeans(M1ests_bigfriends)
#' M1sims5 <- saom_simulate(dat = mydata, struct = M1eff,
#'                        parms = M1parms, N = 5)
#' }
#' @export

saom_simulate <- function(dat, struct, parms, N) {
  require(RSiena)
  struct$initialValue[struct$include] <- parms
  # create algorithm structure
  myalgorithm <- sienaAlgorithmCreate(projname = Sys.time(),
                                      useStdInits = FALSE, cond = FALSE,
                                      nsub = 0 , simOnly = TRUE, n3 = N)
  getsims <- siena07(myalgorithm, data = dat, returnDeps = TRUE, effects = struct,
          batch=TRUE, verbose = FALSE, silent = TRUE)
  return(getsims$sims)
}
