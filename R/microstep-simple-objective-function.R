basic_obj_fn <- function(beta, x, i){
  outdegree <- sum(x[i,])
  reciprocity <- sum(x[i,]*x[,i])
  return(beta[1]*outdegree + beta[2]*reciprocity)
}

microstep <- function(alpha, beta, x0, z, obj_fn = "basic"){
 # browser()
  nact <- nrow(x0)
  actors <- 1:nact
  p <- length(beta)
  # initialize
  Jm <- rep(0, p)
  x <- x0
  z <- z[,1]
#  f0 <- eval(obj_fn(i, x, z, beta))
  h <- rep(NA, nact)
  pi <- rep(NA, nact)

  delta_t <- rexp(1, rate = nact*alpha)
  i <- sample(actors, size = 1, prob = rep(1/nact, nact))
  C <- 1:nact
  for (j in C){
    if (j == i){
      h[j] <- 0
    } else {
      x1 <- x0
      x1[i, j] <- 1 - x0[i, j]
      if (obj_fn == "basic"){
        h[j] <- basic_obj_fn(beta, x1, i) - basic_obj_fn(beta, x0, i)
      }
    }
  }
  pi <- exp(h) / sum(exp(h))
  j <- sample(C, size = 1, prob = pi)
  if (obj_fn == "basic"){
    Jm[1] <- Jm[1] +  sum(x1[i,]) - sum(x0[i,]) -
      sum(pi * (rowSums(x1) - rowSums(x0)) )
    Jm[2] <- Jm[2] + sum(x1[i,]*x1[,i]) - sum(x0[i,]*x0[,i]) -
      sum(pi * (rowSums(x1*t(x1)) - rowSums(x0*t(x0))) )
  }
  if (j != i) {
    x1 <- x0
    x1[i, j] <- 1 - x0[i, j]
  }
  return(x1)
}

set.seed(123456)
x <- matrix(c(rep(0,4), 1, rep(0,3), 1,1,0,1, 0,0,1,0), ncol = 4)

alpha <- 1
beta <- c(2,2)

testx1 <- microstep(alpha, beta, x0 = x, z = NULL)
x2 <- microstep(alpha, beta, x0 = testx1, z = NULL)
