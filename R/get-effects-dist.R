#' A function to estimate the parameters of an RSiena model many times. Returns a data frame of estimates.
#'
#' @param dat A Siena data object. For more information see ?RSiena::sienaDataCreate
#' @param struct A Siena effects structure object. Include all effects you wish to include in the model. See ?RSiena::getEffects for more.
#' @param N integer. Number of times you'd like to fit the model
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
#' M2eff <- includeEffects(M1eff, jumpXTransTrip, interaction1 = "alcohol")
#' # M1 estimates
#' M1ests_bigfriends <- get_effects_dist(dat = mydata, struct = M1eff, N = 1000)
#' M2ests_bigfriends <- get_effects_dist(dat = mydata, struct = M2eff, N = 1000)
#' }
#'
#' @export
get_effects_dist <- function(dat, struct, N){
  require(RSiena)
  myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
  nparm <- sum(struct$include)
  Parmnames <- struct$shortName[struct$include]
  Nnr <- sum(Parmnames != "Rate")
  df.ests <- data.frame(matrix(0, nrow = N, ncol = nparm + Nnr + 1))

  names(df.ests) <- c(Parmnames,
                      paste("se", Parmnames[which(Parmnames!="Rate")], sep = "_"),
                      "maxConv")
  for (i in 1:N){
    fits <- siena07(myalg, data = dat, effects = struct, returnDeps = TRUE,
                        batch=TRUE, verbose = FALSE, silent = TRUE)
    df.ests[i,] <- c(fits$rate, fits$theta, sqrt(diag(fits$covtheta)), fits$tconv.max)
  }
  return(df.ests)
}
