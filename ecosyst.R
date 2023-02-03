
# This R code can be used to run the analyses of the Norwegian
# Red Lists for ecosystems and habitat types described in the paper
# "Metrics for quantifying threats to red-listed species and ecosystems"

# ========================
# Variables
# ========================

# These variables can be used to adjust the output

# Specify the name of the data file. If you haven't saved the data file to 
# your root directory, the file path needs to be included in the file name!
file <- "ecosyst.csv"

# Number of simulations
nsim <- 100000

# Decides whether the estimation of confidence intervals
# should be re-created exactly (if TRUE) or be based on 
# different random numbers (if FALSE)
re.create <- TRUE

# Decides whether Data Deficient systems are excluded (if FALSE)
# or randomly assigned to other Red List categories (if TRUE)
includeDD <- TRUE

# Decides whether (if TRUE) or not (if FALSE) unknown threat factors
# should be inferred from the distribution of the known threat factors 
inferThreats <- FALSE

# Decides whether ecosystem major types should be disintegrated 
# into their minor types
disintegrate <- FALSE

# File names of figures. If you want to display the figures on screen,
# keep the default. If you want to create PNG file, specify the file names
# (including paths).
fig4  <- ""
figS4 <- ""


# ========================
# Constants 
# ========================

# Constants that do not normally need to be changed.
# Change only if you want to modify the underlying assumptions!

# Red List Categories used in the Red Lists analysed
RedListCat <- c("LC", "NT", "VU", "EN", "CR", "DD", "NE")
LC.CR      <- c("LC", "NT", "VU", "EN", "CR")

# Red List weights used in estimation of RLI
# (ordered from LC to CO)
# Defaults to equal-steps
RLweights <- 0:5

# Threat factors reported in the Red Lists analysed
# Note that "unknown" must always be the last threat factor!
threats <- c("alienspe", "climatec", "disturba", "landusec", "huntharv",
             "natcatas", "nativesp", "otherthr", "pollutio", "unknownf")

# Extinction probabilities used in Red List Criterion E
# (ordered from LC to CO)
Eprob <- c(0, 0.05, 0.1, 0.2, 0.5, 1)

# Time frames used in Red List Criterion E
# (ordered from LC to CO)
Etime <- c(100, 100, 100, 50, 50, 50)

# Threshold values of extinction probability
# (ordered from LC to CO)
ExtProb <- c(0, 0.02, 0.05, 0.20, 0.50, 1) # needed?¤

# Threshold values of severity of threats
SEV <- c(0, 0.02, 0.2, 1)

# Second parameter used in Equation 15
beta <- 20

# Parameter values of threat severities
rapidecl <- 0.400
slowdecl <- 0.110
negldecl <- 0.015
unknownd <- 0.091


# ========================
# Auxiliary functions 
# ========================

# The following functions have nothing to do with Red Lists.
# I just used them to simplify some code,
# so they need to be defined first

# Tests whether the arguments are equal. Robust against rounding errors!
"%=%" <- function(arg1, arg2) { 
  attributes(arg1) <- NULL
  attributes(arg2) <- NULL
  return(identical(all.equal(arg1, arg2), TRUE))
}

# Combines text strings
"%+%" <- function(string1, string2) paste(string1, string2, sep="")

# I just find this more intuitive...
"%contains%" <- function (textstring, searchtext) grepl(searchtext, textstring)

# Ceiling function that keeps two decimals
c100 <- function(x) ceiling(x * 100) / 100

# Floor function that keeps two decimals
f100 <- function(x)   floor(x * 100) / 100


# ========================
# RLI functions
# ========================

RLI <- function(x, w=RLweights) {
  # Red List Index
  # Applies Equation 1
  x <- substr(x, 1, 2)
  L <- function(x, C) length(which(x == C))
  N <- c(L(x,"LC"), L(x,"NT"), L(x,"VU"), L(x,"EN"), L(x,"CR"), L(x,"CO"))
  rli <- sum(N * (1 - w / w[6])) / sum(N)
  attr(rli, "N") <- sum(N)
  return(rli)
}

RLW <- function(x, w=RLweights) {
  # Red List Weights
  # Applies Equation 2
  as.vector(sapply(x, function(x) switch(x, LC=w[1],
                                         NT=w[2],
                                         VU=w[3],
                                         EN=w[4],
                                         CR=w[5],
                                         CO=w[6],
                                         NA)))
}

LoS <- function(k, t=NULL, p=Eprob, tau=Etime) { # ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ byggeplass!!!
  # Loss of systems, given Red List categori k
  # (this is the cumulative loss of systems, not threat-wise!)
  k <- substr(k, 1, 2)
  W1 <- RLW(k) + 1
  W2 <- W1 + (W1 > 1 & W1 < 6)
  weight <- ifelse(k == "CR", 3, 1)
  # applying Equation 8:
  Rl <- 1 - (1 - p[W1])^(50 / tau[W1])
  Ru <- 1 - (1 - p[W2])^(50 / tau[W2])
  # rounding downwards for tau > 50 a and upwards for tau < 50 a:
  Rl <- ifelse(tau[W1] > 50, f100(Rl), c100(Rl))
  Ru <- ifelse(tau[W2] > 50, f100(Ru), c100(Ru))
  # applying Equation 9:
  L <- (Rl * weight + Ru) / (weight + 1)
  return(L)
}

# Generation of random numbers using Equations 13 and 14
rIncr <- function(n, x) 0 + rbeta(n, 3, 1) *      x
rDecr <- function(n, x) x + rbeta(n, 1, 3) * (1 - x)

# Transforms a vector into a matrix of the dimensions needed
m <- function(x) matrix(rep(x, each=length(threats)), length(threats), length(x))


# ========================
# Read, prepare and summarise the data
# ========================

# Read the dataset "Norwegian Red List for ecosystems and habitet types 2018"
RL <- read.csv2(file, as.is=T, dec=".", na.strings="n/a")

if (disintegrate) { # disintegrate major types into minor types
  for (i in rev(which(!is.na(RL$MnTypes) & RL$MnTypes != 1 & RL$MnTypes != 0))) {
    if (RL$MnTypes[i] < 1) {
      code <- RL$TypeCode[i]
      if (!length(which(RL$TypeCode == code & RL$MnTypes == 1))) {
        RL <- rbind(RL, RL[i,])
        n <- nrow(RL)
        RL$MnTypes[n] <- 1
        RL$FileID[n]  <- n
      }
    } else {
      code <- RL$TypeCode[i]
      if (code %contains% ",") {
        code <- unlist(strsplit(code, ","))
        if (length(code) != RL$MnTypes[i]) {
          code <- RL$TypeCode[i]
        }
      }
      for (j in 1:RL$MnTypes[i]) {
        RL <- rbind(RL, RL[i,])
        n <- nrow(RL)
        if (length(code) > 1) {
          RL$TypeCode[n] <- code[j]
        } else {
          RL$TypeCode[n] <- RL$TypeCode[i] %+% "-*" %+% j
        }
        RL$MnTypes[n] <- 1
        RL$FileID[n]  <- n
      }
    }
    RL$Category[i]    <- "NA"
  }
}

# Add columns containing the probability of loss of each system
RL$Loss <- LoS(RL$Category)
RL$W <- RLW(RL$Category)

if (includeDD) { # extrapolation to DD systems
  RL$Loss[which(RL$Category == "DD")] <- weighted.mean(
    LoS(c("CR","EN","LC","NT","VU")),
    table(RL$Category[which(RL$Category %in% c("CR","EN","LC","NT","VU"))]))
  RL$W[which(RL$Category == "DD")] <- weighted.mean(c(4,3, 0,   1,   2),
                                                    table(RL$Category[which(RL$Category %in% c("CR","EN","LC","NT","VU"))]))
}

{ # Summary
  print(table(RL$Category)[RedListCat])
  cat("N =", sum(table(RL$Category)), "\n")
  cat("RLI: ", RLI(RL$Category), "\n")
  cat("Total loss of systems:", sum(RL$Loss, na.rm=T), "\n")
}


# ========================
# Analyse threat factors
# ========================

# Add columns for each threat to the data frame
dRLI <- LOSS <- rep(0, length(threats))
names(dRLI) <- names(LOSS) <- threats
for (y in 18) {
  RL[threats] <- 0
  RL["Pop"] <- 0
  for (i in which(RL$Category %in% c("NT", "VU", "EN", "CR", "CO", "DD"))) {
    if (nchar(RL$Threat[i])) {
      if (RL$Threat[i] %contains% "ongoingt") {
        P <- unlist(strsplit(RL$Threat[i], ","))
      } else {
        P <- "unknownf:ongoingt:unknownp:unknownd"
        # If there is no ongoing threat for a threatened system,
        # an ongoing unknown threat is added
      }
    } else {
      P <- "unknownf:ongoingt:unknownp:unknownd"
      # If there is no threat at all for a threatened species,
      # an ongoing unknown threat is added
    }
    for (j in 1:length(P)) {
      if (  P[j] %contains% "ongoingt") {
        # Threat factors receive score according to their severity
        if (P[j] %contains% "rapidecl") sever <- rapidecl
        if (P[j] %contains% "slowdecl") sever <- slowdecl
        if (P[j] %contains% "negldecl") sever <- negldecl
        if (P[j] %contains% "unknownd") sever <- unknownd
        # Severity scores are summed for each species
        RL$Pop[i]               <- RL$Pop[i]               + sever
        RL[i, substr(P[j],1,8)] <- RL[i, substr(P[j],1,8)] + sever
      } # if ongoing
    } # j
  } # i
  L <- length(which(RL$Category %in% LC.CR))
  if (includeDD) {
    L <- length(which(RL$Category %in% c(LC.CR, "DD")))
  }
  for (p in threats) {
    # applying Equation 3:
    THR <- RL[,p] / ifelse(RL$Pop == 0, 1, RL$Pop)
    # applying Equation 6:
    dRLI[p] <- 0.2 * sum(RL$W    * THR, na.rm=T) / L
    # applying Equation 7:
    LOSS [p] <-      sum(RL$Loss * THR, na.rm=T)
  } # p
} # y

if (inferThreats) {
  w <- which(names(LOSS) == "unknownf")
  LOSS  <-  LOSS * (1 +  LOSS[w] / sum( LOSS[-w]))
  dRLI <- dRLI * (1 + dRLI[w] / sum(dRLI[-w]))
  LOSS[w] <- dRLI[w] <- 0
}

print(dRLI)
print(LOSS)


# ------------------------
# Figure 4
# The following script recreates Fig 4 only if the
# above default parameterisation is left unchanged

# Simplify the data by collapsing minor threats
dRLI["otherthr"] <- sum(dRLI[c("otherthr", "unknownf", "natcatas", "alienspe", "huntharv")])
LOSS["otherthr"] <- sum(LOSS[c("otherthr", "unknownf", "natcatas", "alienspe", "huntharv")])
dRLI <-     dRLI[-which(names(dRLI) %in% c("unknownf", "natcatas", "alienspe", "huntharv"))]
LOSS <-     LOSS[-which(names(LOSS) %in% c("unknownf", "natcatas", "alienspe", "huntharv"))]
LOSS <- LOSS[order(dRLI, decreasing=T)]
dRLI <- dRLI[order(dRLI, decreasing=T)]

# Plot a graph for DeltaRLI
if (nchar(fig4)) {
  png(fig4, 1500, 1200, res=180)
}
par(mai=c(0.06, 0.96, 0.06, 0.06), family="sans")
plot(0, 0, xlim=c(7, 24), ylim=c(0.795, 1.01), xaxs="i", yaxs="i", xaxt="n", yaxt="n",
     xlab="", ylab="Red List Index", bty="n", cex.axis=1.2, cex.lab=1.8)
axis(2, seq(0.7, 1, 0.1), F, T, lwd=1.5, lend=1)
axis(2, seq(0.7, 1, 0.05), T, F, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.7, 1, 0.01), F, T, tcl=-0.25, lwd=1.5, lend=1)
rect(11, 0, 14, 1, col=grey(0.96))
for (i in 5:1) {
  rect(11, 1 - sum(dRLI[i:1]), 14, 1, lwd=1.2, col=grey(0.96 - i * 0.12))
}
rect(11, 0, 14, 1, lwd=2.4, col=NA)
x1 <- 7.4; x2 <- 10.6
rli <- RLI(RL$Category)
lines(c(x1, x2), rep(rli, 2), lwd=2.4)
lines(c(x1, x2), rep(1, 2), lwd=2.4)
polygon(x1 + c(-0.2, 0.1, 0.1), rli + c(0, 0.002, -0.002), col="black")
polygon(x2 + c(0.2, -0.1, -0.1), rli + c(0, 0.002, -0.002), col="black")
polygon(x1 + c(-0.2, 0.1, 0.1), 1 + c(0, 0.002, -0.002), col="black")
polygon(x2 + c(0.2, -0.1, -0.1), 1 + c(0, 0.002, -0.002), col="black")
text(mean(c(x1, x2)), 1, "reference value", pos=3, cex=1.2)
text(mean(c(x1, x2)), 1, "1.0000", pos=1, cex=1.2)
text(mean(c(x1, x2)), rli, "RLI 2018", pos=3, cex=1.2)
text(mean(c(x1, x2)), rli, "0.8069", pos=1, cex=1.2)
text(15, 1 - 0.5 * sum(        0) - 0.5 * sum(dRLI[1:1]),
     expression(paste("Land-use change (", bold("8.3"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:1]) - 0.5 * sum(dRLI[1:2]),
     expression(paste("Climate change (", bold("3.2"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:2]) - 0.5 * sum(dRLI[1:3]),
     expression(paste("Other threats (", bold("2.3"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:3]) - 0.5 * sum(dRLI[1:4]),
     expression(paste("Human disturbance (", bold("1.8"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:4]) - 0.5 * sum(dRLI[1:5]),
     expression(paste("Pollution (", bold("1.4"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:5]) - 0.5 * 0.205,
     expression(paste(bold("212"), " ecosystems ", italic("not"), " lost")), pos=4, cex=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(        0) - 0.5 * sum(dRLI[1:1]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:1]) - 0.5 * sum(dRLI[1:2]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:2]) - 0.5 * sum(dRLI[1:3]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:3]) - 0.5 * sum(dRLI[1:4]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:4]) - 0.5 * sum(dRLI[1:5]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:5]) - 0.5 * sum(    0.205), 2), lwd=1.2)
if (nchar(fig4)) {
  dev.off()
}


# ------------------------
# Figure S4
# The following script recreates Fig S4 only if disintegrate == TRUE
# and the remaining parameterisation is left unchanged

# Simplify the data by collapsing minor threats
dRLI["otherthr"] <- sum(dRLI[c("otherthr", "unknownf", "natcatas", "alienspe", "huntharv")])
LOSS["otherthr"] <- sum(LOSS[c("otherthr", "unknownf", "natcatas", "alienspe", "huntharv")])
dRLI <-     dRLI[-which(names(dRLI) %in% c("unknownf", "natcatas", "alienspe", "huntharv"))]
LOSS <-     LOSS[-which(names(LOSS) %in% c("unknownf", "natcatas", "alienspe", "huntharv"))]
LOSS <- LOSS[order(dRLI, decreasing=T)]
dRLI <- dRLI[order(dRLI, decreasing=T)]
LOSS <- LOSS[c(1, 2, 3, 5, 4)]
dRLI <- dRLI[c(1, 2, 3, 5, 4)]

# Plot a graph for DeltaRLI
if (nchar(figS4)) {
  png("c:\\art\\threats\\figS4.png", 1500, 1200, res=180)
}
par(mai=c(0.06, 0.96, 0.06, 0.06), family="sans")
plot(0, 0, xlim=c(7, 24), ylim=c(0.795, 1.01), xaxs="i", yaxs="i", xaxt="n", yaxt="n",
     xlab="", ylab="Red List Index", bty="n", cex.axis=1.2, cex.lab=1.8)
axis(2, seq(0.7, 1, 0.1), F, T, lwd=1.5, lend=1)
axis(2, seq(0.7, 1, 0.05), T, F, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.7, 1, 0.01), F, T, tcl=-0.25, lwd=1.5, lend=1)
rect(11, 0, 14, 1, col=grey(0.96))
for (i in 5:1) {
  rect(11, 1 - sum(dRLI[i:1]), 14, 1, lwd=1.2, col=grey(0.96 - i * 0.12))
}
rect(11, 0, 14, 1, lwd=2.4, col=NA)
x1 <- 7.4; x2 <- 10.6
rli <- RLI(RL$Category)
lines(c(x1, x2), rep(rli, 2), lwd=2.4)
lines(c(x1, x2), rep(1, 2), lwd=2.4)
polygon(x1 + c(-0.2, 0.1, 0.1), rli + c(0, 0.002, -0.002), col="black")
polygon(x2 + c(0.2, -0.1, -0.1), rli + c(0, 0.002, -0.002), col="black")
polygon(x1 + c(-0.2, 0.1, 0.1), 1 + c(0, 0.002, -0.002), col="black")
polygon(x2 + c(0.2, -0.1, -0.1), 1 + c(0, 0.002, -0.002), col="black")
text(mean(c(x1, x2)), 1, "reference value", pos=3, cex=1.2)
text(mean(c(x1, x2)), 1, "1.0000", pos=1, cex=1.2)
text(mean(c(x1, x2)), rli, "RLI 2018", pos=3, cex=1.2)
text(mean(c(x1, x2)), rli, "0.8812", pos=1, cex=1.2)
text(15, 1 - 0.5 * sum(        0) - 0.5 * sum(dRLI[1:1]),
     expression(paste("Land-use change (", bold("17"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:1]) - 0.5 * sum(dRLI[1:2]),
     expression(paste("Climate change (", bold("8"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:2]) - 0.5 * sum(dRLI[1:3]),
     expression(paste("Other threats (", bold("4"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:3]) - 0.5 * sum(dRLI[1:4]),
     expression(paste("Human disturbance (", bold("2"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:4]) - 0.5 * sum(dRLI[1:5]),
     expression(paste("Pollution (", bold("3"), " ecosystems lost)")), pos=4, cex=1.2)
text(15, 1 - 0.5 * sum(dRLI[1:5]) - 0.5 * 0.205,
     expression(paste(bold("843"), " ecosystems ", italic("not"), " lost")), pos=4, cex=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(        0) - 0.5 * sum(dRLI[1:1]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:1]) - 0.5 * sum(dRLI[1:2]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:2]) - 0.5 * sum(dRLI[1:3]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:3]) - 0.5 * sum(dRLI[1:4]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:4]) - 0.5 * sum(dRLI[1:5]), 2), lwd=1.2)
lines(c(14,15), rep(1 - 0.5 * sum(dRLI[1:5]) - 0.5 * sum(    0.205), 2), lwd=1.2)
if (nchar(figS4)) {
  dev.off()
}


# ========================
# Estimation of confidence intervals
# ========================

{ # confidence intervals on the RLI itself
  rlcat <- rlcat. <- RL$Category
  rli <- L <- 0
  w <- which(RL$Category == "DD")
  for (i in 1:5) { # number of species in each Red List Category
    L[i] <- length(which(rlcat == LC.CR[i]))
  }
  # proportion of species in each Red List Category
  L <- L / sum(L)
  # proportion of species in each Red List Category or _higher_
  for (i in 5:2) {
    L[i] <- sum(L[1:i])
  }
  L <- c(0, L)
  for (i in 1:nsim) { # random assignment of DD species to other Red List Categories
    rlcat <- rlcat.
    zf <- runif(length(w))
    rlcat[w][which(zf >= L[1] & zf < L[2])] <- "LC"
    rlcat[w][which(zf >= L[2] & zf < L[3])] <- "NT"
    rlcat[w][which(zf >= L[3] & zf < L[4])] <- "VU"
    rlcat[w][which(zf >= L[4] & zf < L[5])] <- "EN"
    rlcat[w][which(zf >= L[5] & zf <=L[6])] <- "CR"
    rli[i] <- RLI(rlcat)
  }
  cat("Confidence intervals for RLI in 2018:\n")
  print(quantile(rli, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  rm(rlcat, rlcat., rli, w, L, zf)
}

# Determine the lower and upper limit of possible extinction probabilities
# for each system, depending on their Red List Category
ELS <- dRLI <- DRLI <- matrix(0, length(threats), nsim, dimnames=list(threats, NULL))
loss <- dr <- rep(0, nsim)
W <- RLW(RL$Category)
X1 <- X2 <- rep(ExtProb[1], nrow(RL))
X1[which(W == 1)]                      <- ExtProb[2]
X1[which(W == 2)] <- X2[which(W == 1)] <- ExtProb[3]
X1[which(W == 3)] <- X2[which(W == 2)] <- ExtProb[4]
X1[which(W == 4)] <- X2[which(W == 3)] <- ExtProb[5]
X1[which(W == 5)] <- X2[which(W == 4)] <- X2[which(W == 5)] <- ExtProb[6]

if (includeDD) { # Determine weights for the simulation of DD species
  DD <- numeric()
  for (k in c(LC.CR, "CO")) {
    DD <- c(DD, length(which(RL$Category == k)))
  }
  DD <- round(DD / sum(DD) * nsim)
  while (sum(DD) > nsim) {
    DD[1] <- DD[1] - 1
  }
  while (sum(DD) < nsim) {
    DD[1] <- DD[1] + 1
  }
}

if (inferThreats) {
  # Count the occurrences of threats in order to extrapolate to unknown threats
  number <- antP <- antM <- rep(0, length(threats))
  husk <- 0
  for (p in 1:(length(threats)-1)) {
    THR <- RL[,threats[p]] / ifelse(RL[,"Pop"] == 0, 1, RL[,"Pop"])
    number[p] <- sum(THR[which(RL$Category  %in% c("CO", "CR", "EN", "VU", "NT"))])
  }
  number <- round(number / sum(number) * nsim)
  while (sum(number) > nsim) {
    number[1] <- number[1] - 1
  }
  while (sum(number) < nsim) {
    number[1] <- number[1] + 1
  }
  names(number) <- threats
}

# if re.create == TRUE, the exact random numbers of the paper are re-created;
# otherwise novel random numbers are generated
if (re.create) {
  set.seed(27092000)
} else {
  rm(.Random.seed)
}


# ------------------------
# Start simulations
LSS <- DDW <- 0
rlcat <- c("CO","CR","EN","VU","NT","LC")
if (includeDD) {
  rlcat <- c("CO","CR","EN","VU","NT","LC","DD")
}
for (i in which(RL$Category %in% rlcat)) {
  for (y in 18) {
    t <- "Threat"
    if (is.na(W[i])) { # extrapolation to DD systems
      if (DD[1]) { # LC
        w <- 1:DD[1]
        DDW[w] <- rep(RLweights[1], DD[1])
        LSS[w] <- rep(ExtProb  [1], DD[1])
      }
      if (DD[2]) { # NT
        w <- (sum(DD[1:1])+1):sum(DD[1:2])
        DDW[w] <- rep(RLweights[2], DD[2])
        LSS[w] <- runif(DD[2], ExtProb[2], ExtProb[3])
      }
      if (DD[3]) { # VU
        w <- (sum(DD[1:2])+1):sum(DD[1:3])
        DDW[w] <- rep(RLweights[3], DD[3])
        LSS[w] <- runif(DD[3], ExtProb[3], ExtProb[4])
      }
      if (DD[4]) { # EN
        w <- (sum(DD[1:3])+1):sum(DD[1:4])
        DDW[w] <- rep(RLweights[4], DD[4])
        LSS[w] <- runif(DD[4], ExtProb[4], ExtProb[5])
      }
      if (DD[5]) { # CR
        w <- (sum(DD[1:4])+1):sum(DD[1:5])
        DDW[w] <- rep(RLweights[5], DD[5])
        LSS[w] <- rDecr(DD[5], ExtProb[5])
      }
      if (DD[6]) { # CO
        w <- (sum(DD[1:5])+1):sum(DD[1:6])
        DDW[w] <- rep(RLweights[6], DD[6])
        LSS[w] <- rep(ExtProb  [6], DD[6])
      }
      o <- sample(1:nsim)
      DDW <- DDW[o]
      LSS <- LSS[o]
    } else {
      DDW <- rep(W[i], nsim)
      if (W[i] == 4) {
        LSS <- rDecr(nsim, X1[i])
      } else {
        LSS <- runif(nsim, X1[i], X2[i])
      }
    }
    # cumulative loss of systems in a given year y
    loss <- loss + LSS
    # total decline for system i
    pop <- rep(0, nsim)
    # threat-wise declines for system i
    thr <- matrix(0, length(threats), nsim, dimnames=list(threats, NULL))
    if (nchar(RL[i,t])) {
      if (RL[i,t] %contains% "ongoingt") {
        P <- unlist(strsplit(RL[i,t], ","))
      } else {
        P <- "unknownf:ongoingt:unknownp:unknownd"
        # If there is no ongoing threat for a threatened system,
        # an ongoing unknown threat is added
      }
    } else {
      P <- "unknownf:ongoingt:unknownp:unknownd"
      # If there is no threat at all for a threatened system,
      # an ongoing unknown threat is added
    }
    for (j in 1:length(P)) {
      if (P[j] %contains% "ongoingt") {
        Z <- nsim
        Q <- P[j]
        w <- list(1:Z)
        if (inferThreats) { # extrapolation to unknown threats
          if (P[j] %contains% "unknownf") {
            for (k in 1:(length(threats) - 1)) {
              # assigning a number of iterations to each threat factor,
              # according to the distribution among known threats
              Z[k] <- number[threats[k]]
              Q[k] <- threats[k] %+% substr(P[j], 9, 35)
              w[[k]] <- 1:Z[k]
              if (k > 1) {
                w[[k]] <- w[[k]] + sum(number[threats[1:(k-1)]])
              }
            } # k
          } # if unknown threat
        } # if infer threats
        o <- sample(1:nsim)
        for (k in 1:length(Z)) {
          if (Z[k]) {
            sever <- rep(0, nsim)
            # Threat factors receive randomised scores according to their severity
            if (Q[k] %contains% "rapidecl") sever[w[[k]]] <- rDecr(Z[k],         SEV[3])
            if (Q[k] %contains% "slowdecl") sever[w[[k]]] <- runif(Z[k], SEV[2], SEV[3])
            if (Q[k] %contains% "negldecl") sever[w[[k]]] <- rIncr(Z[k], SEV[2])
            if (Q[k] %contains% "unknownd") sever[w[[k]]] <- rbeta(Z[k],     2,    beta)
            # shuffle the values (needed when inferThreats == T)
            sever <- sever[o]
            # Severity scores are summed for each system
            pop                    <- pop                    + sever
            thr[substr(Q[k],1,8),] <- thr[substr(Q[k],1,8),] + sever
          }
        } # k
      } # if ongoing
    } # j
    ELS  <-  ELS + thr * m(LSS) / ifelse(thr == 0, 1, m(pop))
    dRLI <- dRLI + thr * m(DDW) / ifelse(thr == 0, 1, m(pop))
    dr <- dr+apply(thr * m(DDW) / ifelse(thr == 0, 1, m(pop)), 2, sum)
  } # y
} # i

{# Print the results
  L <- length(which(RL$Category %in% c("CO","CR","EN","VU","NT","LC","DD")))
  cat("\n\nConfidence intervals for the cumulative deltaRLI in 2018:\n")
  print(quantile(dr / 5 / L, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  cat("\n\nConfidence intervals for the threat-wise deltaRLIs in 2018:\n")
  print(apply(dRLI / 5 / L, 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  cat("\n\nConfidence intervals for the cumulative ELS50 in 2018:\n")
  print(quantile(loss, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  cat("\n\nConfidence intervals for the threat-wise ELS50 in 2018:\n")
  print(apply(ELS, 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975)))
}

