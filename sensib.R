

LoS <- function(k, t, nsim = 0, method = weightingScheme) {
  # Loss of species, given Red List category k and generation time t
  # (this is the cumulative loss of species, not threat-wise!)
  # Note that the expected (mean) loss of species is returned is nsim <= 1,
  # whereas nsim randomised losses of species are returned otherwise.
  # In the latter case, both `k` and `t` have to be of length 1.
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  tauL <- apply(rbind(apply(rbind(t * RLcateg$lowG[w], 100), 2, min), 
                      RLcateg$lowT[w]), 2, max)
  tauU <- apply(rbind(apply(rbind(t * RLcateg$uppG[w], 100), 2, min), 
                      RLcateg$uppT[w]), 2, max)
  # applying Equation 8:
  R50L <- 1 - (1 - RLcateg$lowP[w])^(TimeFrame / tauL)
  R50U <- 1 - (1 - RLcateg$uppP[w])^(TimeFrame / tauU)
  # rounding downwards for tau > TimeFrame and upwards for tau < TimeFrame:
  R50L <- ifelse(tauL > TimeFrame, f100(R50L), c100(R50L))
  R50U <- ifelse(tauU > TimeFrame, f100(R50U), c100(R50U))
  if (nsim < 2) {
    # applying Equation 9:
    return(meanDist(RLcateg$distr[w], R50L, R50U))
  } else {
    return(randomiseDist(RLcateg$distr[w], R50L, R50U, nsim))
  }
}


LoS.eqSteps <- function(k, t, nsim = 0) {
  # Loss of species according to equal-steps weighting
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  return(rep(RLcateg$wt[w]), max(1, nsim))
}


LoS.Ev2 <- function(k, t, nsim = 0) {
  #
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  tauL <- apply(rbind(apply(rbind(t * RLcateg$lowG[w], 100), 2, min), 
                      RLcateg$lowT[w]), 2, max)
  tauU <- apply(rbind(apply(rbind(t * RLcateg$uppG[w], 100), 2, min), 
                      RLcateg$uppT[w]), 2, max)
  # applying (logit of!) Equation 8:
  R50L <- logit(1 - (1 - RLcateg$lowP[w])^(TimeFrame / tauL))
  R50U <- logit(1 - (1 - RLcateg$uppP[w])^(TimeFrame / tauU))
  # shifting extinction probabilities "outwards":
  R50L <- RL50L + ifelse(RLcateg$wt == 0, -100,
                  ifelse(RLcateg$wt == 1, -0.6,
                  ifelse(RLcateg$wt == 2, -0.3,
                  ifelse(RLcateg$wt == 3, +0.3,
                  ifelse(RLcateg$wt == 4, +0.6,
                  ifelse(RLcateg$wt == 5, +100,
                                            NA))))))
  R50U <- RL50U + ifelse(RLcateg$wt == 0, -100,
                  ifelse(RLcateg$wt == 1, -0.3,
                  ifelse(RLcateg$wt == 2, +0.3,
                  ifelse(RLcateg$wt == 3, +0.6,
                  ifelse(RLcateg$wt == 4, +100,
                  ifelse(RLcateg$wt == 5, +100,
                                            NA))))))
  # back-transforming and rounding:
  R50L <- round(invlogit(R50L), 2)
  R50U <- round(invlogit(R50U), 2)
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], R50L, R50U))
  } else {
    return(randomiseDist(RLcateg$distr[w], R50L, R50U, nsim))
  }
}


LoS.Ev2 <- function(k, t, nsim = 0) {
  #
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  tauL <- apply(rbind(apply(rbind(t * RLcateg$lowG[w], 100), 2, min), 
                      RLcateg$lowT[w]), 2, max)
  tauU <- apply(rbind(apply(rbind(t * RLcateg$uppG[w], 100), 2, min), 
                      RLcateg$uppT[w]), 2, max)
  # applying (logit of!) Equation 8:
  R50L <- logit(1 - (1 - RLcateg$lowP[w])^(TimeFrame / tauL))
  R50U <- logit(1 - (1 - RLcateg$uppP[w])^(TimeFrame / tauU))
  # reducing extinction probabilities:
  R50L <- RL50L + ifelse(RLcateg$wt == 0, -100,
                  ifelse(RLcateg$wt == 5, +100,
                                          -0.5))
  R50U <- RL50U + ifelse(RLcateg$wt == 0, -100,
                  ifelse(RLcateg$wt >= 4, +100,
                                          -0.5))
  # back-transforming and rounding:
  R50L <- round(invlogit(R50L), 2)
  R50U <- round(invlogit(R50U), 2)
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], R50L, R50U))
  } else {
    return(randomiseDist(RLcateg$distr[w], R50L, R50U, nsim))
  }
}


LoS.A1 <- function(k, t, nsim = 0) {
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  time <- sapply(sapply(t, min, 100 / 3) * 3, max, 10)
  L <- round(RLcateg$lowA1[w]^(time / 10), 2)
  U <- round(RLcateg$uppA1[w]^(time / 10), 2)
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], L, U))
  } else {
    return(randomiseDist(RLcateg$distr[w], L, U, nsim))
  }
}


LoS.A2 <- function(k, t, nsim = 0) {
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  time <- sapply(sapply(t, min, 100 / 3) * 3, max, 10)
  L <- round(RLcateg$lowA2[w]^(time / 10), 2)
  U <- round(RLcateg$uppA2[w]^(time / 10), 2)
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], L, U))
  } else {
    return(randomiseDist(RLcateg$distr[w], L, U, nsim))
  }
}


LoS.B1 <- function(k, t, nsim = 0) {
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  L <- 1 - RLcateg$lowB1[w] / RLcateg$lowB1[which(RLcateg$LC)]
  U <- 1 - RLcateg$uppB1[w] / RLcateg$uppB1[which(RLcateg$LC)]
  L <- ifelse(L > 0.5, f100(L), c100(L))
  U <- ifelse(U > 0.5, f100(U), c100(U))
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], L, U))
  } else {
    return(randomiseDist(RLcateg$distr[w], L, U, nsim))
  }
}


LoS.B2 <- function(k, t, nsim = 0) {
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  L <- 1 - RLcateg$lowB2[w] / RLcateg$lowB1[which(RLcateg$LC)]
  U <- 1 - RLcateg$uppB2[w] / RLcateg$uppB1[which(RLcateg$LC)]
  L <- ifelse(L > 0.5, f100(L), c100(L))
  U <- ifelse(U > 0.5, f100(U), c100(U))
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], L, U))
  } else {
    return(randomiseDist(RLcateg$distr[w], L, U, nsim))
  }
}


LoS.C <- function(k, t, nsim = 0) {
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  L <- 1 - RLcateg$lowC[w] / RLcateg$lowB1[which(RLcateg$LC)]
  U <- 1 - RLcateg$uppC[w] / RLcateg$uppB1[which(RLcateg$LC)]
  L <- ifelse(L > 0.5, f100(L), c100(L))
  U <- ifelse(U > 0.5, f100(U), c100(U))
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], L, U))
  } else {
    return(randomiseDist(RLcateg$distr[w], L, U, nsim))
  }
}


LoS.D <- function(k, t, nsim = 0) {
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  L <- 1 - RLcateg$lowD[w] / RLcateg$lowB1[which(RLcateg$LC)]
  U <- 1 - RLcateg$uppD[w] / RLcateg$uppB1[which(RLcateg$LC)]
  L <- ifelse(L > 0.5, f100(L), c100(L))
  U <- ifelse(U > 0.5, f100(U), c100(U))
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], L, U))
  } else {
    return(randomiseDist(RLcateg$distr[w], L, U, nsim))
  }
}



