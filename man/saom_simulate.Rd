% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saom-simulate.R
\name{saom_simulate}
\alias{saom_simulate}
\title{Function for simulating from a stochastic actor-oriented model.}
\usage{
saom_simulate(dat, struct, parms, N)
}
\arguments{
\item{dat}{A Siena data object. For more information see ?RSiena::sienaDataCreate}

\item{struct}{A Siena effects structure object. Include all effects you wish to include in the model. See ?RSiena::getEffects for more.}

\item{parms}{Numeric vector of effects. The rate parameters must be listed first, followed by the effects in the objective function.}

\item{N}{Integer. Number of simulations you desire from the model.}
}
\description{
Function for simulating from a stochastic actor-oriented model.
}
\examples{
\dontrun{
# data set up from \\url{https://www.stats.ox.ac.uk/~snijders/siena/basicRSiena.r}
library(RSiena)
friend.data.w1 <- as.matrix(read.table("data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("data/s50-network3.dat"))
drink <- as.matrix(read.table("data/s50-alcohol.dat"))
friendshipData <- array(c(friend.data.w1, friend.data.w2, friend.data.w3), dim = c(50, 50, 3))
friendship <- sienaDependent(friendshipData)
alcohol <- varCovar(drink)
mydata <- sienaDataCreate(friendship, alcohol)
M1eff <- getEffects(mydata)
load("data/M1ests_bigfriends.rda")
M1parms <- colMeans(M1ests_bigfriends)
M1sims5 <- saom_simulate(dat = mydata, struct = M1eff,
                       parms = M1parms, N = 5)
}
}
