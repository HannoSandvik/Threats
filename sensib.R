

LoS <- function(k, t, nsim = 0, method = weightingELS) {
  # Loss of species, given Red List category k and generation time t
  # (this is the cumulative loss of species, not threat-wise!)
  # Note that the expected (mean) loss of species is returned if nsim <= 1,
  # whereas nsim randomised losses of species are returned otherwise.
  # In the latter case, both `k` and `t` have to be of length 1.
  method <- tolower(method)
  loss <- NULL
  if (substr(method, 1, 2) == "eq") loss <- LoS.eqSteps(k, t, nsim)
  if (method == "a1")  loss <- LoS.A1 (k, t, nsim)
  if (method == "a2")  loss <- LoS.A2 (k, t, nsim)
  if (method == "b")   loss <- LoS.B1 (k, t, nsim)
  if (method == "b1")  loss <- LoS.B1 (k, t, nsim)
  if (method == "b2")  loss <- LoS.B2 (k, t, nsim)
  if (method == "c")   loss <- LoS.C  (k, t, nsim)
  if (method == "c1")  loss <- LoS.C  (k, t, nsim)
  if (method == "d")   loss <- LoS.D  (k, t, nsim)
  if (method == "d1")  loss <- LoS.D  (k, t, nsim)
  if (method == "e")   loss <- LoS.Ev1(k, t, nsim)
  if (method == "e1")  loss <- LoS.Ev1(k, t, nsim)
  if (method == "ev1") loss <- LoS.Ev1(k, t, nsim)
  if (method == "e2")  loss <- LoS.Ev2(k, t, nsim)
  if (method == "ev2") loss <- LoS.Ev2(k, t, nsim)
  if (method == "e3")  loss <- LoS.Ev3(k, t, nsim)
  if (method == "ev3") loss <- LoS.Ev3(k, t, nsim)
  if (is.null(loss)) stop("Unknown method")
  return(loss)
}


LoS.Ev1 <- function(k, t, nsim = 0) {
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
  return(rep(RLcateg$wt[w], max(1, nsim)) / max(RLcateg$wt, na.rm=TRUE))
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
  R50L <- R50L + ifelse(RLcateg$wt[w] == 0, -100,
                 ifelse(RLcateg$wt[w] == 1, -0.6,
                 ifelse(RLcateg$wt[w] == 2, -0.3,
                 ifelse(RLcateg$wt[w] == 3, +0.3,
                 ifelse(RLcateg$wt[w] == 4, +0.6,
                 ifelse(RLcateg$wt[w] == 5, +100,
                                              NA))))))
  R50U <- R50U + ifelse(RLcateg$wt[w] == 0, -100,
                 ifelse(RLcateg$wt[w] == 1, -0.3,
                 ifelse(RLcateg$wt[w] == 2, +0.3,
                 ifelse(RLcateg$wt[w] == 3, +0.6,
                 ifelse(RLcateg$wt[w] == 4, +100,
                 ifelse(RLcateg$wt[w] == 5, +100,
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


LoS.Ev3 <- function(k, t, nsim = 0) {
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
  R50L <- R50L + ifelse(RLcateg$wt[w] == 0, -100,
                 ifelse(RLcateg$wt[w] == 5, +100,
                                            -0.5))
  R50U <- R50U + ifelse(RLcateg$wt[w] == 0, -100,
                 ifelse(RLcateg$wt[w] >= 4, +100,
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
  L <- 1 - RLcateg$lowB2[w] / RLcateg$lowB2[which(RLcateg$LC)]
  U <- 1 - RLcateg$uppB2[w] / RLcateg$uppB2[which(RLcateg$LC)]
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
  L <- 1 - RLcateg$lowC[w] / RLcateg$lowC[which(RLcateg$LC)]
  U <- 1 - RLcateg$uppC[w] / RLcateg$uppC[which(RLcateg$LC)]
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
  L <- 1 - RLcateg$lowD[w] / RLcateg$lowD[which(RLcateg$LC)]
  U <- 1 - RLcateg$uppD[w] / RLcateg$uppD[which(RLcateg$LC)]
  L <- ifelse(L > 0.5, f100(L), c100(L))
  U <- ifelse(U > 0.5, f100(U), c100(U))
  if (nsim < 2) {
    return(meanDist(RLcateg$distr[w], L, U))
  } else {
    return(randomiseDist(RLcateg$distr[w], L, U, nsim))
  }
}


RLI <- function(x, t = 0, method = weightingRLI) {
  # Red List Index
  # Applies Equation 1
  rlw <- na.omit(RLW(x, t, method))
  rli <- 1 - sum(rlw) / length(rlw) / max(RLcateg$wt, na.rm=TRUE)
  attr(rli, "N") <- length(rlw)
  return(rli)
}


RLW <- function(x, t = 0, method = weightingRLI) {
  # Red List Weights
  # Applies Equation 2
  method <- tolower(method)
  wt <- NULL
  if (substr(method, 1, 2) == "eq") wt <- RLW.eqSteps(x, t)
  if (method == "a1")  wt <- RLW.A1 (x, t)
  if (method == "a2")  wt <- RLW.A2 (x, t)
  if (method == "b")   wt <- RLW.B1 (x, t)
  if (method == "b1")  wt <- RLW.B1 (x, t)
  if (method == "b2")  wt <- RLW.B2 (x, t)
  if (method == "c")   wt <- RLW.C  (x, t)
  if (method == "c1")  wt <- RLW.C  (x, t)
  if (method == "d")   wt <- RLW.D  (x, t)
  if (method == "d1")  wt <- RLW.D  (x, t)
  if (method == "e")   wt <- RLW.Ev1(x, t)
  if (method == "e1")  wt <- RLW.Ev1(x, t)
  if (method == "ev1") wt <- RLW.Ev1(x, t)
  if (method == "e2")  wt <- RLW.Ev2(x, t)
  if (method == "ev2") wt <- RLW.Ev2(x, t)
  if (method == "e3")  wt <- RLW.Ev3(x, t)
  if (method == "ev3") wt <- RLW.Ev3(x, t)
  if (is.null(wt)) stop("Unknown method")
  return(wt)
}


RLW.eqSteps <- function(x, t) {
  w <- sapply(x, function(y) ifelse(y %in% RLcateg$name,
                                    which(RLcateg$name == y),
                                    Inf))
  return(as.vector(RLcateg$wt[w]))
}


RLW.A1 <- function(x, t) {
  return(LoS.A1(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.A2 <- function(x, t) {
  return(LoS.A2(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.B1 <- function(x, t) {
  return(LoS.B1(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.B2 <- function(x, t) {
  return(LoS.B2(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.C <- function(x, t) {
  return(LoS.C(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.D <- function(x, t) {
  return(LoS.D(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.Ev1 <- function(x, t) {
  return(LoS.Ev1(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.Ev2 <- function(x, t) {
  return(LoS.Ev2(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.Ev3 <- function(x, t) {
  return(LoS.Ev3(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}



# Sensitivity analysis

for (meth in c("E", "Ev2", "Ev3", "equal-steps", "A1", "A2", "B", "C", "D")) {
  weightingELS <- meth
  RL. <- calcLoss(RL)
  RL. <- addThreats(RL.)
  drli <- sort(dRLI(RL.)$ELS50[, 3], decreasing = TRUE)
  drli <- cbind(drli, drli / sum(drli))
  colnames(drli) <- c("ELS" %+% TimeFrame, "fraction")
  cat("\n\nWeighting scheme " %+% meth %+% ":\n")
  print(drli)
  cat("Cumulative ELS" %+% TimeFrame %+% ": " %+% sum(drli[, 1]) %+% "\n")
}




