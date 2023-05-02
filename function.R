# These functions are needed to run the analyses of the Norwegian Red Lists
# for species described in the paper "Metrics for quantifying how much
# different threats contribute to red lists of species and ecosystems"



# ========================
# Auxiliary functions 
# ========================

# The following functions have nothing to do with Red Lists.
# I just used them to simplify some code, so they need to be defined first.

# Tests whether the arguments are equal. Robust against rounding errors!
"%=%" <- function(arg1, arg2) { 
  attributes(arg1) <- NULL
  attributes(arg2) <- NULL
  return(identical(all.equal(arg1, arg2), TRUE))
}

# Combines text strings
"%+%" <- function(string1, string2) paste0(string1, string2)

# Calculates the union of two sets (vectors)
"%A%" <- function(set1, set2)
  if (is.null(set1)) logical(0) else as.vector(na.omit(set1[set1 %in% set2]))

# Removes an element of a set
"%-%" <- function(arg1, arg2) arg1[which(!(arg1 %in% na.omit(arg2)))]

# I just find this more intuitive...
"%contains%" <- function(vector, search) grepl(search, vector, fixed = TRUE)

# Decadal logarithm should be abbreviated "lg"!
lg <- function(x) log10(x)

# Logit function
logit <- function(x) log(x / (1 - x))

# Inverse of the logit function
invlogit <- function(x) ifelse(x > 100, 1, exp(x) / (1 + exp(x)))

# Ceiling function that keeps two decimals
c100 <- function(x) ceiling(x * 100) / 100

# Floor function that keeps two decimals
f100 <- function(x)   floor(x * 100) / 100



# ========================
# RLI functions
# ========================

# Tests for data deficiency
isDD <- function(x) x %in% DD


# Tests whether a species/ecosystem is (near) threatened
isConcern <- function(x) x %in% (LC.EX %-% LC)


# Tests whether a species/ecosystem is extinct/collapsed
isExtinct <- function(x) x %in% extinct


# Extracts the threat factor from a text string
extractThreat   <- function(x) return(
  as.vector(unlist(as.data.frame(strsplit(x, ":"))[1, ]))
)


# Extracts the timing of a threat from a text string
extractTiming   <- function(x) return(
  as.vector(unlist(as.data.frame(strsplit(x, ":"))[2, ]))
)


# Extracts the scope of a threat from a text string
extractScope    <- function(x) return(
  as.vector(unlist(as.data.frame(strsplit(x, ":"))[3, ]))
)


# Extracts the severity of a threat from a text string
extractSeverity <- function(x) return(
  as.vector(unlist(as.data.frame(strsplit(x, ":"))[4, ]))
)


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
  # Cumulative loss of species using the "equal-steps" weighting scheme
  w <- sapply(x, function(y) ifelse(y %in% RLcateg$name,
                                    which(RLcateg$name == y),
                                    Inf))
  return(as.vector(RLcateg$wt[w]))
}


RLW.A1 <- function(x, t) {
  # Cumulative loss of species using the "A1" weighting scheme
  return(LoS.A1(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.A2 <- function(x, t) {
  # Cumulative loss of species using the "A2" weighting scheme
  return(LoS.A2(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.B1 <- function(x, t) {
  # Cumulative loss of species using the "B1" weighting scheme
  return(LoS.B1(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.B2 <- function(x, t) {
  # Cumulative loss of species using the "B2" weighting scheme
  return(LoS.B2(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.C <- function(x, t) {
  # Cumulative loss of species using the "C" weighting scheme
  return(LoS.C(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.D <- function(x, t) {
  # Cumulative loss of species using the "D" weighting scheme
  return(LoS.D(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.Ev1 <- function(x, t) {
  # Cumulative loss of species using version 1 of the "E" weighting scheme
  return(LoS.Ev1(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.Ev2 <- function(x, t) {
  # Cumulative loss of species using version 2 of the "E" weighting scheme
  return(LoS.Ev2(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLW.Ev3 <- function(x, t) {
  # Cumulative loss of species using version 3 of the "E" weighting scheme
  return(LoS.Ev3(x, t) *  max(RLcateg$wt, na.rm=TRUE))
}


RLI <- function(x, t = 0, method = weightingRLI) {
  # Red List Index
  # Applies Equation 1
  rlw <- na.omit(RLW(x, t, method))
  rli <- 1 - sum(rlw) / length(rlw) / max(RLcateg$wt, na.rm=TRUE)
  attr(rli, "N") <- length(rlw)
  return(rli)
}


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
  # Cumulative loss of species using the "E" weighting scheme (version 1)
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


LoS.Ev2 <- function(k, t, nsim = 0) {
  # Cumulative loss of species using version 2 of the "E" weighting scheme
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
  # Cumulative loss of species using version 3 of the "E" weighting scheme
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


LoS.eqSteps <- function(k, t, nsim = 0) {
  # Cumulative loss of species using the "equal-steps" weighting scheme
  w <- sapply(k, function(x) ifelse(x %in% RLcateg$name,
                                    which(RLcateg$name == x),
                                    Inf))
  return(rep(RLcateg$wt[w], max(1, nsim)) / max(RLcateg$wt, na.rm=TRUE))
}


LoS.A1 <- function(k, t, nsim = 0) {
  # Cumulative loss of species using the "A1" weighting scheme
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
  # Cumulative loss of species using the "A2" weighting scheme
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
  # Cumulative loss of species using the "B1" weighting scheme
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
  # Cumulative loss of species using the "B2" weighting scheme
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
  # Cumulative loss of species using the "C" weighting scheme
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
  # Cumulative loss of species using the "D" weighting scheme
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


meanDist <- function(D, L, U) return(
  # Returns the arithmetic mean of the interval between L and U,
  # given distribution D
  ifelse(D == "unif", L * 0.50 + U * 0.50,
  ifelse(D == "incr", L * 0.25 + U * 0.75,
  ifelse(D == "decr", L * 0.75 + U * 0.25,
  NA)))
)


randomiseDist <- function(D, L, U, N) {
  # Returns N random numbers within interval between L and U,
  # given distribution D
  r <- NA
  if (D == "unif") r <- runif(N, L, U)
  if (D == "incr") r <- rIncr(N,    U)
  if (D == "decr") r <- rDecr(N, L   )
  return(r)
}


meanSeverity <- function(x) {
  # Returns the arithmetic mean of severity,
  # given a severity class
  name <- extractSeverity(x)
  m <- function(x) {
    w <- which(Severity$name == x)
    f <- Severity$distr[w]
    L <- Severity$lower[w]
    U <- Severity$upper[w]
    B <- Severity$beta [w]
    mS <- switch(f,
                 unif = L * 0.50 + U * 0.50,
                 incr = L * 0.25 + U * 0.75,
                 decr = L * 0.75 + U * 0.25,
                 beta = 2 / (B + 2),
                 NA)
  }
  return(round(as.vector(sapply(name, m)), 3))
}


randomiseSeverity <- function(n, x) {
  # Returns n random numbers of severity,
  # given a severity class
  name <- extractSeverity(x)
  m <- function(x) {
    w <- which(Severity$name == x)
    f <- Severity$distr[w]
    L <- Severity$lower[w]
    U <- Severity$upper[w]
    B <- Severity$beta [w]
    mS <- switch(f,
                 unif = runif(n, L, U),
                 incr = rIncr(n,    U),
                 decr = rDecr(n, L   ),
                 beta = rbeta(n, 2, B),
                 NA)
  }
  return(as.vector(sapply(name, m)))
}


identifyYears <- function(RL) {
  # Years for which Red List are available are identified
  # from the column names which start with "Categ"
  w <- which(substr(names(RL), 1, nchar(Categ)) == Categ &
               !(names(RL) %contains% "."))
  years <- numeric(0)
  if (length(w)) {
    for (i in w) {
      y <- substr(names(RL)[i], nchar(Categ) + 1, nchar(names(RL)[i]))
      if (all(unlist(strsplit(y, "")) %in% as.character(0:9))) {
        years <- c(years, as.numeric(y))
      } else {
        years <- NA
      }
    }
  } else {
    year <- NA
  }
  if (any(is.na(years))) {
    cat("ERROR: Years could not be identified!\n")
  }
  return(years)
}


identifyThreats <- function(RL, unknown = unknownThreat) {
  # Threats reported in a Red List are identified
  # from the column names which start with "Threat"
  thr <- character()
  w <- which(substr(names(RL), 1, nchar(Threat)) == Threat)
  if (length(w)) {
    thr <- unique(unlist(strsplit(unlist(RL[,w]), ",")))
    thr <- unique(extractThreat(thr))
    thr <- c(sort(thr) %-% unknown, unknown)
  }
  return(thr)
}


checkRL <- function(RL) {
  # Checks whether the Red List data frame is compatible with the 
  # formatting requirements for the subsequent analyses.
  # * Prints error or success messages.
  # * Returns a vector of the Red List Categories used in the data frame.
  OK <- TRUE
  years <- identifyYears(RL)
  if (any(is.na(years))) {
    OK <- FALSE
  } else {
    # Check Red List Categories
    RLCat <- unique(unlist(RL[, which(substr(names(RL), 1, nchar(Categ)) ==
                                        Categ)])) %-% ""
    if (all(RLCat %in% c(RedListCat, RedListCat %+% downlistSymbol))) {
      RLCat      <-      unique(n0(RLCat))
      cat("Red List Categories are OK.\n")
    } else {
      cat("ERROR: The datafile contains unexpected Red List Categories:\n" %+%
            paste(sort(RLCat %-% RedListCat), collapse=", ") %+% "\n")
    } # RL categories
    # Check the threat columns
    if (all((Threat %+% years) %in% names(RL))) {
      if (length(unlist(strsplit(unlist(strsplit(unlist(RL[, Threat %+% years]),
                                                 ",")), ":"))) %% 4) {
        cat("ERROR: Different threats for the same species need to be " %+%
              "separated by commas;\nand each threat needs to contain name, " %+%
              "timing, scope and severity, \nseparated by colons!\n")
        OK <- FALSE
      } else {
        thr <- unique(unlist(strsplit(unlist(RL[, Threat %+% years]), ",")))
        if (any(extractThreat(thr) %in% unknownThreat)) {
          timing <- unique(extractTiming(thr))
          if (any(timing %in% unknownTiming)) {
            if (any(timing %in% inclTiming)) {
              if (any(extractScope(thr) %in% unknownScope)) {
                if (any(extractSeverity(thr) %in% unknownSeverity)) {
                  cat("Threat columns are OK.\n")
                } else {
                  cat("WARNING: The severity \"" %+% unknownSeverity %+%
                        "\" does not occur in the dataset!\n")
                  OK <- FALSE
                } # unknown severity missing?
              } else {
                cat("WARNING: The scope \"" %+% unknownScope %+%
                      "\" does not occur in the dataset!\n")
                OK <- FALSE
              } # unknown scope missing?
            } else {
              cat("ERROR: None of the following timings occur in the dataset:\n")
              cat(paste(inclTiming, collapse = ", ") %+% "\n")
              OK <- FALSE
            } # timings ok?
          } else {
            cat("WARNING: The timing \"" %+% unknownTiming %+%
                  "\" does not occur in the dataset!\n")
            OK <- FALSE
          } # unknown timing missing?
        } else {
          cat("WARNING: The threat factor \"" %+% unknownThreat %+%
                "\" does not occur in the dataset!\n")
          OK <- FALSE
        } # unknown threat missing?
      } # formatting ok
    } else {
      cat("ERROR: The following column name(s) were expected but not found:\n")
      cat(paste((Threat %+% sort(years)) %-% names(RL), collapse = ", ") %+% "\n")
      OK <- FALSE
    } # threat columns 
    # Check the change columns
    if (length(years) > 1) {
      if (all((Change %+% sort(years)[-1]) %in% names(RL))) {
        if (any(unlist(RL[, Change %+% sort(years)[-1]]) %in% realChange)) {
          cat("Change columns are OK.\n")
        } else {
          cat("ERROR: No change in Red List Category has \"" %+%
                paste(realChange, collapse = "\" or \"") %+% "\" as a reason!\n")
          OK <- FALSE
        } # real change missing
      } else {
        cat("ERROR: The following column name(s) were expected but not found:\n")
        cat(paste(Change %+% sort(years)[-1], collapse = ", ") %+% "\n")
        OK <- FALSE
      } # columns missing? 
    } # > 1 year?
  } # years ok
  # Check the generation time column
  if (GTime %in% names(RL)) {
    cat("Generation time was found.\n")
  } else {
    cat("WARNING: The dataset lacks a column containing generation times!\n")
    OK <- FALSE
  } # generation time
  # Summarise checks
  if (OK) {
    cat("\nEverything looks fine so far!\n")
  } else {
    cat("\nOne or more problem(s) were found that may preclude further analyses!\n")
  } # summary
  return(RLCat)
} # checkRL


n0 <- function(x, symbol = downlistSymbol) {
  # "Uplisting" of downlisted species,
  # i.e. downlisted species are raised by one Red List Category. 
  # Note that this function assumes:
  # * that downlisting is indicated by "symbol" after the Red List Category,
  # * that downlisting is always to the Red List Category immediately below.
  for (i in which(!RLcateg$EX)) {
    v <- which(x == (RLcateg$name[i] %+% symbol))
    W <- RLcateg$wt[i]
    j <- which(RLcateg$wt == min(RLcateg$wt[which(RLcateg$wt > W)],
                                 na.rm = TRUE))[1]
    x[v] <- RLcateg$name[j]
  }
  return(x)
}


uplist <- function(RL, symbol = downlistSymbol) {
  # "Uplists" species that have been downlisted.
  # This function works only for IUCN Red List Categories (specifically, 
  # LC, NT, VU and EN). Others are returned unmodified.
  years <- identifyYears(RL)
  for (y in years) {
    RL[, Categ %+% y] <- n0(RL[, Categ %+% y], symbol = symbol)
  }
  return(RL)
}


downlist <- function(RL, symbol = downlistSymbol) {
  # "Confirms" downlisting, i.e. removes the symbol for downlisting
  years <- identifyYears(RL)
  for (y in years) {
    RL[, Categ %+% y] <- gsub(symbol, "", RL[, Categ %+% y], fixed = TRUE)
  }
  return(RL)
}


backCast <- function(RL, real = realChange) {
  # "Back-casts" the most recent knowledge to earlier Red Lists
  years <- sort(identifyYears(RL), decreasing = TRUE)
  for (y1 in years) { # copy category columns
    RL[, Categ %+% y1 %+% "." %+% y1] <- RL[, Categ %+% y1]
  }
  if (length(years) > 1) {
    for (y1 in years[-1]) {
      # y1 is the year _to_ which knowledge is back-cast
      y1f <- min(years[years > y1])
      # y1f is the year of the Red List following y1
      for (y2 in rev(years[years > y1])) {
        # y2 is the year _from_ which knowledge is back-cast
        RL[  , Categ %+% y1  %+% "." %+% y2] <-
          RL[, Categ %+% y1f %+% "." %+% y2]
        for (i in 1:nrow(RL)) {
          if (RL[i, Change %+% y1f]  %=%  real &&
              RL[i, Categ  %+% y1 ] %in% LC.EX &&
              RL[i, Categ  %+% y2 ] %in% LC.EX)  {
            RL[  i, Categ  %+% y1    %+% "." %+% y2] <- RL[i, Categ %+% y1]
          } # if change
        } # i (rows)
      } # y2
    } # y1
  } else { # if > 1 year
    if (length(years)) { # only 1 Red List
      cat("NB: There was no earlier Red List to back-cast to!\n")
    } else { # no Red List - or wrong column names!
      cat("NB: The dataset did not contain identifiable Red List Categories!\n")
    }
  }
  return(RL)
} # backCast


calcLoss <- function(RL) {
  # Adds columns to the Red List data frame which contain the species loss
  # (or ecosystem loss), i.e. the extinction probabilities within 50 years
  # for each species (or ecosystem)
  years <- sort(identifyYears(RL))
  if (GTime %in% colnames(RL)) {
    if (!is.numeric(RL[, GTime])) {
      RL[, GTime] <- as.numeric(RL[, GTime])
    }
    w <- which(is.na(RL[, GTime]))
    if (length(w)) {
      RL[w, GTime] <- 0
      cat("NB: `NA` generation times have been set to 0.\n")
    }
  } else {
    RL[, GTime] <- 0
    cat("WARNING: All generation times were assumed to be < 1 year.\n")
  }
  for (y1 in years) {
    for (y2 in years[years >= y1]) {
      RL[, "Loss" %+% y1 %+% "." %+% y2] <-
        LoS(RL[, Categ %+% y1 %+% "." %+% y2], RL[, GTime])
      if (includeDD) { # assign probabilities of loss to DD species
        for (i in which(isDD(RL[, Categ %+% y1 %+% "." %+% y2]))) {
          RL[i, "Loss" %+% y1 %+% "." %+% y2] <- round(weighted.mean(
            LoS(sort(LC.EX), rep(RL[i, GTime], length(LC.EX))),
            table(RL[which(RL[, Categ %+% y1 %+% "." %+% y2] %in% LC.EX),
                     Categ %+% y1 %+% "." %+% y2])), 3)
        } # i
      } # if DD
    } # y2
  } # y1
  return(RL)
} # calcLoss


addThreats <- function(RL) {
  # Adds columns for each Red List and each threat to the data frame
  years <- sort(identifyYears(RL))
  threats <- identifyThreats(RL)
  unknThr <- paste(unknownThreat,
                   inclTiming[1],
                   unknownScope,
                   unknownSeverity,
                   sep = ":")
  for (y in years) {
    t <- Threat %+% y
    C <- Categ  %+% y %+% "." %+% max(years)
    RL[   threats %+% y] <- 0
    RL[  "Pop"    %+% y] <- 0
    selection <- which(isConcern(RL[,C]))
    if (includeDD) {
      selection <- which(isConcern(RL[,C]) | isDD(RL[,C]))
    }
    for (i in selection) {
      if (isExtinct(RL[i, C])) {
        # For extinct species, all timings of threats are considered,
        # i.e. they are re-coded as "ongoing"
        if (nchar(RL[i, t])) {
          P <- unlist(strsplit(RL[i, t], ","))
          for (j in 1:length(P)) {
            P[j] <- paste(extractThreat(P[j]),
                          inclTiming[1],
                          extractScope(P[j]),
                          extractSeverity(P[j]),
                          sep = ":")
          }
          RL[i, t] <- paste(P, collapse = ",")
        } else {
          RL[i, t] <- unknThr
        }
      } # if extinct
      if (nchar(RL[i, t])) {
        P <- unlist(strsplit(RL[i, t], ","))
        if (any(extractTiming(P) %in% inclTiming)) {
          # Keep only ongoing threats
          P <- P[which(extractTiming(P) %in% inclTiming)]
        } else {
          # If there is no ongoing threat for a threatened species,
          # an ongoing unknown threat is added
          P <- unknThr
        }
      } else {
        # If there is no threat at all for a threatened species,
        # an ongoing unknown threat is added
        P <- unknThr
      }
      for (j in 1:length(P)) {
        # Threat factors receive score according to their severity
        sever <- meanSeverity(P[j])
        # Severity scores are summed for each species
        RL[i,              "Pop" %+% y] <- RL[i,              "Pop" %+% y] + sever
        RL[i,extractThreat(P[j]) %+% y] <- RL[i,extractThreat(P[j]) %+% y] + sever
      } # j
    } # i
  } # y
  return(RL)
} # addThreats


summariseRL <- function(RL, exclude = notEval) {
  # Produces a summary table of the Red List(s) in the dataset
  years <- sort(identifyYears(RL))
  categ <- RedListCat %-% exclude
  rows  <- ""
  i <- 0
  if (length(years) > 0) {
    tab  <- matrix(as.numeric(NA),
                   length(years) * (length(years) + 1) / 2,
                   length(categ) + 3)
    colnames(tab) <- c("N", categ, "RLI", "Cum.ELS" %+% TimeFrame)
    for (y1 in years) {
      for (y2 in years[years >= y1]) {
        i <- i + 1
        C <- Categ %+% y1 %+% "." %+% y2
        tb <- table(RL[, C])
        tab[i, categ] <- tb[categ]
        tab[i, "N"]   <- sum(tab[i, categ], na.rm=TRUE)
        tab[i, "RLI"] <- RLI(RL[, C], RL[, GTime])
        tab[i, "Cum.ELS" %+% TimeFrame] <- 
          sum(RL[, "Loss" %+% y1 %+% "." %+% y2], na.rm=TRUE)
        if (y1 %=% y2) {
          rows[i] <- "RL" %+% y1
        } else {
          rows[i] <- "RL" %+% y1 %+% "." %+% y2
        }
      } # y2
    } # y1
    rownames(tab) <- rows
    print(tab)
  } else {
    cat("NB: There were no Red List data to summarise!\n")
    tab <- NA
  }
  invisible(tab)
} # summariseRL


DeltaRLI <- function(RL) {
  # Calculates DeltaRLI
  # This requires a dataframe `RL` with Red List Categories and threats factors
  # from more than one Red List.
  # The output is a matrix with threat-wise DeltaRLI values for each combination
  # of Red Lists, corrected for knowledge from the most recent Red List.
  years <- sort(identifyYears(RL))
  threats <- identifyThreats(RL)
  if (length(years) > 1) {
    DRLIp <- DRLIm <- matrix(0,       length(threats),  length(years),
                             dimnames = list(threats, "RL" %+% years)
    )
    W <- THR <- DW <- list()
    for (y in years) {
      categ <- Categ %+% y %+% "." %+% max(years)
      W[[y]] <- RLW(RL[, categ], RL[, GTime])
    } # y
    L <- length(which(RL[, categ] %in% LC.EX))
    for (p in threats) {
      for (y in years) {
        # applying Equation 3:
        THR[[y]] <- RL[, p %+% y] / 
          ifelse(RL[, "Pop" %+% y] == 0, 1, RL[, "Pop" %+% y])
      } # y
      for (i in 2:length(years)) {
        yr <- years[i]
        y0 <- years[i - 1]
        # applying Equation 5:
        DW[[i]]  <- W[[y0]] * THR[[y0]] - W[[yr]] * THR[[yr]]
        # applying Equation 4 (separately for positive and negative DeltaRLI):
        DRLIp[p, i] <- sum(DW[[i]][which(DW[[i]] > 0 & W[[yr]] != W[[y0]])], 
                           na.rm=T) * 0.2 / L
        DRLIm[p, i] <- sum(DW[[i]][which(DW[[i]] < 0 & W[[yr]] != W[[y0]])],
                           na.rm=T) * 0.2 / L
      } # i
    } # p
    if (inferThreats) { # extrapolation to unknown threats
      n <- length(threats) # this code requires "unknown" to be the last threat!
      for (i in 2:length(years)) {
        DRLIp[, i] <- DRLIp[, i] * (1 + DRLIp[n, i] / sum(DRLIp[1:(n - 1), i]))
        DRLIm[, i] <- DRLIm[, i] * (1 + DRLIm[n, i] / sum(DRLIm[1:(n - 1), i]))
      }
      DRLIp[n, ] <- DRLIm[n, ] <- 0
    } # extrapolation
    DRLIpm <- DRLIp + DRLIm
    DRLI   <- matrix(0, length(threats), length(years) * (length(years)- 1) / 2)
    cnames <- character()
    k <- 0
    for (i in (length(years) - 1):1) {
      y1 <- years[i]
      DR <- rep(0, length(threats))
      for (j in (i + 1):length(years)) {
        k <- k + 1
        y2 <- years[j]
        DR <- DR + DRLIpm[, j]
        cnames <- c(cnames, "RL" %+% y1 %+% "_" %+% y2)
        DRLI[, k] <- DR
      }
    }
    dimnames(DRLI) <- list(threats, cnames)
    return(DRLI)
  } else {
    cat("DeltaRLI can only be estimated if there are at least two Red Lists.\n")
    return(NA)
  }
} # DeltaRLI


dRLI <- function(RL, RLI = TRUE, ELS = TRUE) {
  # Calculates dRLI and ELS50
  # This requires a dataframe `RL` with Red List Categories and threats factors.
  # The output is a list with two elements:
  # * a matrix with threat-wise dRLI values for each Red List,
  # * a matrix with threat-wise ELS50 values for each Red List.
  years <- sort(identifyYears(RL))
  threats <- identifyThreats(RL)
  drli <- els <- matrix(0,     length(threats),  length(years), 
                        dimnames=list(threats, "RL" %+% years))
  for (y in years) {
    C <- Categ %+% y %+% "." %+% max(years)
    L <- length(which(RL[, C] %in% LC.EX))
    Gw <- RLW(RL[, C], RL[, GTime])
    if (includeDD) {
      # extrapolate to DD species
      L <- length(which(RL[, C] %in% c(LC.EX, DD)))
      Gw[which(isDD(RL[, C]))] <- mean(Gw[which(!isDD(RL[, C]))], na.rm = TRUE)
    }
    for (p in threats) {
      # applying Equation 3:
      THR <- RL[, p %+% y] / ifelse(RL[, "Pop" %+% y] == 0, 1, RL[, "Pop" %+% y])
      # applying Equation 6:
      drli[p, "RL" %+% y] <- 0.2 * sum(Gw * THR, na.rm = TRUE) / L
      # applying Equation 7:
      els [p, "RL" %+% y] <- sum(RL[, "Loss" %+% y %+% "." %+% max(years)] * THR,
                                 na.rm = TRUE)
    } # p
  } # y
  if (inferThreats) {
    # extrapolation to unknown threats
    n <- nrow(els)
    for (i in 1:length(years)) {
      els [, i] <-  els[, i] * (1 +  els[n, i] / sum( els[1:(n - 1), i]))
      drli[, i] <- drli[, i] * (1 + drli[n, i] / sum(drli[1:(n - 1), i]))
    } # i
    els[n, ] <- drli[n, ] <- 0
  } # extrapolation
  output <- list(dRLI = drli, ELS = els)
  names(output) <- c("dRLI", "ELS" %+% TimeFrame)
  if (!ELS) {
    output <- output[[1]]
  } else {
    if (!RLI) {
      output <- output[[2]]
    }
  }
  return(output)
} # dRLI


ELS <- function(RL, RLI = FALSE, ELS = TRUE) return(
  # A wrapper for dRLI which (by default) drops the dRLI values
  dRLI(RL, RLI = RLI, ELS = ELS)
)


# Generation of random numbers using Equations 13 and 14
rIncr <- function(n, x) 0 + rbeta(n, 3, 1) *      x
rDecr <- function(n, x) x + rbeta(n, 1, 3) * (1 - x)


# Transforms a vector into a matrix of the dimensions needed
m <- function(x) matrix(rep(x, each=length(threats)), length(threats), length(x))


confidenceRLI <- function(RL, nsim, column,
                          quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
                          showProgress = TRUE) {
  # Estimates confidence intervals on the RLI itself
  rlcat <- rlcat. <- n0(RL[, column])
  rli <- L <- 0
  w <- which(isDD(RL[, column]))
  for (i in 1:length(LC.EX)) { 
    # number of species in each Red List Category
    L[i] <- length(which(rlcat == LC.EX[i]))
  }
  # proportion of species in each Red List Category
  L <- L / sum(L)
  # proportion of species in each Red List Category or _lower_
  for (i in length(LC.EX):2) {
    L[i] <- sum(L[1:i])
  }
  L <- c(0, L)
  if (showProgress) cat("\n")
  for (i in 1:nsim) {
    # random assignment of DD species to other Red List Categories
    rlcat <- rlcat.
    zf <- runif(length(w))
    for (j in length(LC.EX):1) {
      rlcat[w][which(zf >= L[j] & zf <= L[j + 1])] <- LC.EX[j]
    } # j
    rli[i] <- RLI(rlcat, RL[, GTime])
    if (showProgress) cat ("\r" %+% round(100*i/nsim) %+% "% done.")
  } # i
  if (showProgress) cat("\n\n")
  return(quantile(rli, quantiles))
} # confidenceRLI


simulateDRLI <- function(RL, nsim, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
                         showProgress = TRUE) {
  # Estimates confidence intervals on DeltaRLI, dRLI and ELS50
  years <- sort(identifyYears(RL))
  C <- Categ %+% max(years) %+% "." %+% max(years)
  # remove rows that are not needed
  if (includeDD) {
    RL <- RL[which(RL[, C] %in% c(LC.EX, DD)), ]
  } else {
    RL <- RL[which(RL[, C] %in%   LC.EX), ]
  }
  threats <- identifyThreats(RL)
  # determine the lower and upper limit of possible extinction probabilities
  # for each species, depending on their Red List Category
  W <- ELS <- drli <- DRLI <- DRLIp <- DRLIm <- list()
  loss <- dr <- pop <- thr <- list()
  for (y in years) { # for each edition of the Red List
    C <- Categ %+% y %+% "." %+% max(years)
    W   [[y]] <- RLW(RL[, C], RL[, GTime])
    ELS [[y]] <- drli[[y]] <- DRLI[[y]] <- DRLIp[[y]] <- DRLIm[[y]] <-
      matrix(0, length(threats), nsim, dimnames=list(threats, NULL))
    loss[[y]] <- dr[[y]] <- rep(0, nsim)
  } # y
  if (includeDD) { # determine weights for the simulation of DD species
    DDwt <- numeric()
    for (k in LC.EX) {
      DDwt <- c(DDwt, length(which(RL[, C] == k)))
    }
    DDwt <- round(DDwt / sum(DDwt) * nsim)
    while (sum(DDwt) > nsim) {
      DDwt[1] <- DDwt[1] - 1
    }
    while (sum(DDwt) < nsim) {
      DDwt[1] <- DDwt[1] + 1
    }
  } # if DD included
  if (inferThreats) {
    #  count the occurrences of threats in order to extrapolate to unknown threats
    number <- matrix(0, length(threats), max(years), dimnames=list(threats, NULL))
    for (y in sort(years, decreasing = TRUE)) { # for each edition of the Red List
      husk <- 0
      common <- ""
      C <- Categ %+% y %+% "." %+% max(years)
      for (p in (threats %-% unknownThreat)) {
        THR <- RL[, p %+% y] / ifelse(RL[, "Pop" %+% y] == 0, 1, RL[, "Pop" %+% y])
        number[p, y] <- sum(THR[which(RL[, C]   %in%
                                        RLcateg$name[which(!RLcateg$LC)])])
        if (number[p, y] > husk) { # identify the most common threat factor
          common <- p
          husk <- number[p, y]
        } 
      } # p in threats
      number[, y] <- round(number[, y] / sum(number[, y]) * nsim)
      w <- which(threats == common)
      while (sum(number[, y]) > nsim) {
        number[w, y] <- number[w, y] - 1
      }
      while (sum(number[, y]) < nsim) {
        number[w, y] <- number[w, y] + 1
      }
    } # y
  } # if threats are inferred
  # if re.create == TRUE, the exact random numbers of the paper are re-created;
  # otherwise novel random numbers are generated
  if (re.create) {
    set.seed(25021998)
  } else {
    rm(.Random.seed)
  }
  # ------------------------
  # Start simulations
  LSS <- DDW <- 0
  unknThr <- paste(unknownThreat,
                   inclTiming[1],
                   unknownScope,
                   unknownSeverity,
                   sep = ":")
  if (showProgress) cat("\n\n\n")
  for (i in 1:nrow(RL)) {
    C <- Categ %+% years %+% "." %+% max(years)
    if (any(isConcern(RL[i, C])) | any(isDD(RL[i, C]))) {
      # ignore species that are LC in all years,
      # as they don't contribute to any of the metrics
      for (y in years) {
        t <- Threat %+% y
        if (is.na(W[[y]][i])) { # extrapolation to DD species
          if (y %=% years[1]) {
            last <- 0
            for (j in 1:length(DDwt)) {
              if (DDwt[j]) {
                w <- (last + 1):sum(DDwt[1:j])
                last   <-       sum(DDwt[1:j])
                DDW[w] <- rep(RLW(LC.EX[j], RL[i, GTime]),       DDwt[j])
                LSS[w] <-     LoS(LC.EX[j], RL[i, GTime], nsim = DDwt[j])
              } # extrapolation
            } # j
            o <- sample(1:nsim)
            DDW <- DDW[o]
            LSS <- LSS[o]
          } # DD species
        } else { # non-DD species
          DDW <- rep(W[[y]][i], nsim)
          LSS <- LoS(RL[i, Categ %+% y %+% "." %+% max(years)], RL[i, GTime], nsim)
        } # non-DD species
        # cumulative loss of species in a given year y
        loss[[y]] <- loss[[y]] + LSS
        # total population decline for species i
        pop[[y]] <- rep(0, nsim)
        # threat-wise population declines for species i
        thr[[y]] <- matrix(0, length(threats), nsim, dimnames=list(threats, NULL))
        if (nchar(RL[i, t])) {
          P <- unlist(strsplit(RL[i, t], ","))
          if (any(extractTiming(P) %in% inclTiming)) {
            # Keep only ongoing threats
            P <- P[which(extractTiming(P) %in% inclTiming)]
          } else {
            # If there is no ongoing threat for a threatened species,
            # an ongoing unknown threat is added
            P <- unknThr
          } # ongoing threats recorded?
        } else {
          # If there is no threat at all for a threatened species,
          # an ongoing unknown threat is added
          P <- unknThr
        } # any threats recorded?
        for (j in 1:length(P)) {
          Z <- nsim
          Q <- P[j]
          w <- list(1:Z)
          if (inferThreats) { # extrapolation to unknown threats
            if (extractThreat(P[j]) %=% unknownThreat) {
              for (k in 1:(length(threats) - 1)) {
                # assigning a number of iterations to each threat factor,
                # according to the distribution among known threats
                Z[k] <- number[threats[k], y]
                Q[k] <- gsub(unknownThreat, threats[k], P[j], fixed = TRUE)
                w[[k]] <- 1:Z[k]
                if (k > 1) {
                  w[[k]] <- w[[k]] + sum(number[threats[1:(k - 1)], y])
                }
              } # k
            } # if unknown threat
          } # if infer threats
          o <- sample(1:nsim)
          for (k in 1:length(Z)) {
            if (Z[k]) {
              sever <- rep(0, nsim)
              # Threat factors receive randomised scores according to their severity
              sever[w[[k]]] <- randomiseSeverity(Z[k], Q[k])
              # shuffle the values (needed when inferThreats == T)
              sever <- sever[o]
              # Severity scores are summed for each species
              pop[[y]] <- pop[[y]] + sever
              thr[[y]]  [extractThreat(Q[k]), ] <- 
                thr[[y]][extractThreat(Q[k]), ] + sever
            }
          } # k
        } # j
        # applying Equations 7 and 6:
        ELS [[y]] <-  ELS[[y]] + 
          thr[[y]] * m(LSS) / ifelse(thr[[y]] == 0, 1, m(pop[[y]]))
        drli[[y]] <- drli[[y]] +
          thr[[y]] * m(DDW) / ifelse(thr[[y]] == 0, 1, m(pop[[y]]))
        dr[[y]] <-     dr[[y]] +
          apply(thr[[y]] * m(DDW) / ifelse(thr[[y]] == 0, 1, m(pop[[y]])),
                2, sum)
        if (!is.na(W[[y]][i]) & y > years[1]) {
          # applying Equation 5 and 4:
          x <- max(years[which(years < y)]) # year of previous Red List
          if (    W[[x]][i] !=  W[[y]][i]) {
            DW <- W[[x]][i] * thr[[x]] / ifelse(thr[[x]] == 0, 1, m(pop[[x]])) -
              W[[y]][i] * thr[[y]] / ifelse(thr[[y]] == 0, 1, m(pop[[y]]))
            DRLIp  [[y]] <- DRLIp[[y]] + ifelse(DW > 0, DW, 0)
            DRLIm  [[y]] <- DRLIm[[y]] + ifelse(DW < 0, DW, 0)
          }
        } # estimate DeltaRLI
      } # y
    } # if in NT-RE
    if (showProgress) cat ("\r" %+% round(100 * i / nrow(RL)) %+% "% done.")
  } # i
  if (showProgress) cat("\n\n\n")
  # prepare results for output
  mxW <- RLW(extinct[1])
  for (y in years) {
    DRLI[[y]] <- (DRLIp[[y]] + DRLIm[[y]]) / mxW / nrow(RL)
    dr  [[y]] <-               dr   [[y]]  / mxW / nrow(RL)
    drli[[y]] <-               drli [[y]]  / mxW / nrow(RL)
  } # y
  results <- list(DeltaRLI = DRLI,
                  Cum.dRLI = dr,
                      dRLI = drli,
                  Cum.ELS  = loss,
                      ELS  = ELS)
  printResults(years, results, quantiles)
  invisible(results)
} # simulateDRLI


printResults <- function(years, results, quantiles) {
  # Prints the output of `simulateDRLI`
  years <- sort(years)
  DRLI  <- results$DeltaRLI
  dr    <- results$Cum.dRLI
  drli  <- results$dRLI
  loss  <- results$Cum.ELS
  ELS   <- results$ELS
  if (length(years) > 1) {
    for (i in (length(years) - 1):1) {
      y1 <- years[i]
      DR <- matrix(0, nrow(DRLI[[y1]]), ncol(DRLI[[y1]]),
                   dimnames = dimnames(DRLI[[y1]]))
      for (j in (i + 1):length(years)) {
        y2 <- years[j]
        DR <- DR + DRLI[[y2]]
        cat("\n\nConfidence intervals for DeltaRLI from " %+%
              y1 %+% " to " %+% y2 %+% ":\n")
        print(apply(DR, 1, quantile, quantiles))
      } # j
    } # i
  } # > 1 year?
  for (y in years) {
    cat("\n\nConfidence intervals for the cumulative dRLI in " %+%
          y %+% ":\n")
    print(quantile(dr[[y]], quantiles))
    cat("\n\nConfidence intervals for the threat-wise dRLIs in " %+%
          y %+% ":\n")
    print(apply(drli[[y]], 1, quantile, quantiles))
  } # y
  for (y in years) {
    cat("\n\nConfidence intervals for the cumulative ELS" %+% TimeFrame %+%
          " in " %+% y %+% ":\n")
    print(quantile(loss[[y]], quantiles))
    cat("\n\nConfidence intervals for the threat-wise ELS" %+% TimeFrame %+%
          " in " %+% y %+% ":\n")
    print(apply(ELS[[y]], 1, quantile, quantiles))
  } # y
  invisible(NULL)
} # printResults


disaggrMajorTypes <- function(RL, minor, type, id, categ) {
  # Disaggregates major ecosystem types into their respective minor ecosystem types
  for (i in rev(which(!is.na(RL[, minor]) & RL[, minor] != 1 & RL[, minor] != 0))) {
    # check each line of the Red List dataset in which the number of minor 
    # ecosystem types is unequal to 0 and 1
    if (RL[i, minor] < 1) {
      # cases where one minor ecosystem type is divided into more than one line
      code <- RL[i, type]
      if (!length(which(RL[, type] == code & RL[, minor] == 1))) {
        RL <- rbind(RL, RL[i, ])
        n <- nrow(RL)
        RL[n, minor] <- 1
        RL[n, id] <- n
      }
    } else {
      # cases where one line comprises more than one minor ecosystem types
      code <- RL[i, type]
      if (code %contains% ",") {
        code <- unlist(strsplit(code, ","))
        if (length(code) != RL[i, minor]) {
          code <- RL[i, type]
        }
      }
      for (j in 1:RL[i, minor]) {
        RL <- rbind(RL, RL[i, ])
        n <- nrow(RL)
        if (length(code) > 1) {
          RL[n, type] <- code[j]
        } else {
          RL[n, type] <- RL[i, type] %+% "-*" %+% j
        }
        RL[n, minor] <- 1
        RL[n, id] <- n
      } # j
    } # minor ecosystems per line
    RL[i, categ] <- "NA"
  } # i
  return(RL)
} # disaggrMajorTypes

