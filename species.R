
# This R code can be used to run the analyses of the Norwegian
# Red Lists for species described in the paper
# "Metrics for quantifying threats to red-listed species and ecosystems"



# ========================
# Variables
# ========================

# These variables can be used to adjust the output

# Specify the name of the data file. If you haven't saved the data file to 
# your root directory, the file path needs to be included in the file name!
file <- "species.csv"

# Number of simulations
# NB: the default takes several hours!
# For exploration purposes, nsim <- 1000 will suffice
nsim <- 100000

# Decides whether the estimation of confidence intervals
# should be re-created exactly (if TRUE) or be based on 
# different random numbers (if FALSE)
re.create <- TRUE

# Decides whether Data Deficient species are excluded (if FALSE)
# or randomly assigned to other Red List categories (if TRUE)
includeDD <- TRUE

# Decides whether (if TRUE) or not (if FALSE) unknown threat factors
# should be inferred from the distribution of the known threat factors 
inferThreats <- FALSE

# File names of figures. If you want to display the figures on screen,
# keep the default. If you want to create PNG file, specify the file names
# (including paths).
fig1  <- ""
fig2  <- ""
fig3  <- ""
figS1 <- ""
figS2 <- ""
figS3 <- ""



# ========================
# Constants 
# ========================

# Constants that do not normally need to be changed.
# Change only if you want to modify the underlying assumptions!

# Red List Categories used in the Red Lists analysed
# follow up!!!!!!!!!!!!!! ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
DD         <- "DD"
LC         <- "LC"
extinct    <- "RE" # in other contexts possibly "EW", "EX", "CO"
LC.EX      <- c(LC,   "NT", "VU", "EN", "CR", extinct)
RedListCat <- c(LC.EX, DD,  "NA", "NE")

# Red List weights used in estimation of RLI
# (ordered from LC to RE)
# Defaults to equal-steps
RLweights <- 0:5

# Threat factors reported in the Red Lists analysed
# Note that "unknown" must always be the last threat factor!
threats <- c("landusec", "climatec", "nativesp", "pollutio", "disturba", "alienspe",
  "huntharv", "bycatchc", "natcatas", "outsiden", "otherthr", "nothreat", "unknownf")

# Extinction probabilities used in Red List Criterion E
# (ordered from LC to EX)
Eprob <- c(0, 0.05, 0.1, 0.2, 0.5, 1)

# Time frames used in Red List Criterion E
# (ordered from LC to EX)
Etime <- c(100, 100, 100, 20, 10, 10)

# Threshold values of extinction probability
# (ordered from LC to EX)
ExtProb <- c(0, 0.02, 0.05, 0.43, 0.97, 1)

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
"%+%" <- function(string1, string2) paste0(string1, string2)

# Removes an element of a set
"%-%" <- function(arg1, arg2) arg1[which(!(arg1 %in% na.omit(arg2)))]

# I just find this more intuitive...
"%contains%" <- function (vector, search) grepl(search, vector, fixed=TRUE)

# Decadal logarithm should be abbreviated "lg"!
lg <- function(x) log10(x)

# Ceiling function that keeps two decimals
c100 <- function(x) ceiling(x * 100) / 100

# Floor function that keeps two decimals
f100 <- function(x)   floor(x * 100) / 100



# ========================
# RLI functions
# ========================

# Tests for data deficiency
isDD <- function(x) x == DD


# Tests for Red-List evaluation
# (i.e. FALSE for NA and NE species)
isEvaluated <- function(x) x %in% c(LC.EX) | isDD(x)


# Tests whether a species/ecosystem is (near) threatened
isConcern <- function(x) x %in% (LC.EX %-% LC)


# Tests whether a species/ecosystem is extinct/collapsed
isExtinct <- function(x) x %in% extinct


RLI <- function(x, w=RLweights) {
  # Red List Index
  # Applies Equation 1
  rlw <- na.omit(RLW(substr(x, 1, 2), w=w))
  rli <- 1 - sum(rlw) / length(rlw) / max(w)
  attr(rli, "N") <- length(rlw)
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
                                            RE=w[6], EW=w[6], EX=w[6], CO=w[6],
                                            NA)))
}


n0 <- function(x, symbol="<") {
  # "Uplisting" of downlisted species
  ifelse(x %contains% symbol,
         as.vector(sapply(substr(x, 1, 2), function(a)
           switch(a, LC="NT", NT="VU", VU="EN", EN="CR", a))),
         x)
}


uplist <- function(RL, symbol="<") {
  # "Uplists" species that have been downlisted.
  # This function works only for IUCN Red List Categories (specifically, 
  # LC, NT, VU and EN). Other are returned unmodified.
  years <- identifyYears(RL)
  for (y in years) {
    RL[, "Categ" %+% y] <- n0(RL[, "Categ" %+% y], symbol=symbol)
  }
  return(RL)
}


identifyYears <- function(RL) {
  # Years for which Red List are available are identified
  # from the column names which start with "Categ"
  w <- which(substr(names(RL), 1, 5) == "Categ" & !(names(RL) %contains% "."))
  years <- numeric(0)
  if (length(w)) {
    for (i in w) {
      y <- substr(names(RL)[i], 6, nchar(names(RL)[i]))
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


identifyThreats <- function(RL, unknown="unknownf") { #¤¤¤ define unknownf!!!
  # Threats reported in a Red List are identified
  # from the column names which start with "Threat"
  thr <- character()
  w <- which(substr(names(RL), 1, 6) == "Threat")
  if (length(w)) {
    thr <- unique(unlist(strsplit(unlist(RL[,w]), ",")))
    thr <- unique(unlist(as.data.frame(strsplit(thr, ":"))[1,]))
    thr <- c(sort(thr) %-% unknown, unknown)
  }
  return(thr)
}


backCast <- function(RL, real="realpopu") { #¤¤¤ define realpopu
  # "Back-casts" the most recent knowledge to earlier Red Lists
  years <- sort(identifyYears(RL), decreasing = TRUE)
  if (length(years) > 1) {
    for (y1 in years) { # copy category columns
      RL[, "Categ" %+% y1 %+% "." %+% y1] <- RL[, "Categ" %+% y1]
    }
    for (y1 in years[-1]) {
      # y1 is the year _to_ which knowledge is back-cast
      y1f <- min(years[years > y1])
      # y1f is the year of the Red List following y1
      for (y2 in rev(years[years > y1])) {
        # y2 is the year _from_ which knowledge is back-cast
        RL[  , "Categ" %+% y1  %+% "." %+% y2] <-
          RL[, "Categ" %+% y1f %+% "." %+% y2]
        for (i in 1:nrow(RL)) {
          if (RL[i, "Change" %+% y1f]  %=%  real &&
              RL[i, "Categ"  %+% y1 ] %in% LC.EX &&
              RL[i, "Categ"  %+% y2 ] %in% LC.EX)  {
            RL[i, "Categ" %+% y1 %+% "." %+% y2] <- RL[i, "Categ" %+% y1]
          } # if change
        } # i (rows)
      } # y2
    } # y1
    #    for (y1 in years) { # remove the original category columns #¤¤¤ or not?!
    #      RL <- RL[, -which(names(RL) == "Categ" %+% y1)]
    #    }
  } else { # if > 1 year
    if (length(years)) { # only 1 Red List
      cat("NB: There was no earlier Red List to back-cast to!\n")
    } else { # no Red List - or wrong column names!
      cat("NB: The dataset did not contain identifiable Red List Categories!\n")
    }
  }
  return(RL)
} # backCast


calcLoss <- function(RL, tau, includeDD) {
  # Adds columns to the Red List data frame which contain the species loss
  # (or ecosystem loss), i.e. the extinction probabilities within 50 years
  # for each species (or ecosystem)
  years <- sort(identifyYears(RL))
  for (y1 in years) {
    for (y2 in years[years >= y1]) {
      RL[, "Loss" %+% y1 %+% "." %+% y2] <-
        LoS(RL[, "Categ" %+% y1 %+% "." %+% y2], tau)
      if (includeDD) { # assign probabilities of loss to DD species
        for (i in which(RL[, "Categ" %+% y1 %+% "." %+% y2] == "DD")) {
          RL[i, "Loss" %+% y1 %+% "." %+% y2] <- round(weighted.mean(
            LoS(sort(LC.EX), rep(tau[i], length(LC.EX))),
            table(RL[which(RL[, "Categ" %+% y1 %+% "." %+% y2] %in% LC.EX),
                     "Categ" %+% y1 %+% "." %+% y2])), 3)
        } # i
      } # if DD
    } # y2
  } # y1
  return(RL)
}


summariseRL <- function(RL, exclude=c("NA", "NE")) {
  years <- sort(identifyYears(RL))
  categ <- RedListCat %-% exclude
  rows  <- ""
  i <- 0
  if (length(years) > 1) {
    tab  <- matrix(as.numeric(NA),
                   length(years) * (length(years) + 1) / 2,
                   length(categ) + 3)
    colnames(tab) <- c("N", categ, "RLI", "Cum.ELS50")
    wt <- rep(NA, length(categ))
    wt[1:length(LC.EX)] <- max(RLweights)
    wt[1:length(RLweights)] <- RLweights
    for (y1 in years) {
      for (y2 in years[years >= y1]) {
        i <- i + 1
        tb <- table(RL[, "Categ" %+% y1 %+% "." %+% y2])
        tab[i, categ] <- tb[categ]
        tab[i, "N"]   <-     sum(tab[i, categ]     , na.rm=TRUE)
        tab[i, "RLI"] <- 1 - sum(tab[i, categ] * wt, na.rm=TRUE) /
          sum(tab[i, LC.EX], na.rm=TRUE) / max(wt, na.rm=TRUE)
        tab[i, "Cum.ELS50"] <- sum(RL[, "Loss" %+% y1 %+% "." %+% y2], na.rm=TRUE)
        if (y1 %=% y2) {
          rows[i] <- "RL" %+% y1
        } else {
          rows[i] <- "RL" %+% y1 %+% "." %+% y2
        }
      }
    }
    rownames(tab) <- rows
#    w <- which(is.na(apply(tab > 0, 2, any)))
#    if (length(w)) {
#      tab <- tab[, -w]
#    }
    print(tab)
  } else {
    cat("NB: There were no Red List data to summarise!\n")
    tab <- NA
  }
  invisible(tab)
}


LoS <- function(k, t, p=Eprob, tau=Etime) {
  # Loss of species, given Red List categori k and generation time t
  # (this is the cumulative loss of species, not threat-wise!)
  k <- substr(k, 1, 2)
  W1 <- RLW(k) + 1
  W2 <- W1 + (W1 > 1 & W1 < 6)
  tauL <- ifelse(k == "CR", sapply(sapply(t, min, 100/3) * 3, max, 10), tau[W1])
  tauL <- ifelse(k == "EN", sapply(sapply(t, min,  20)   * 5, max, 20), tauL)
  tauU <- ifelse(k == "EN", sapply(sapply(t, min, 100/3) * 3, max, 10), tau[W2])
  tauU <- ifelse(k == "VU", sapply(sapply(t, min,  20)   * 5, max, 20), tauU)
  # weighting according to Equation 14:
  weight <- ifelse(k == "CR", 3, 1)
  # applying Equation 8:
  R50L <- 1 - (1 - p[W1])^(50 / tauL)
  R50U <- 1 - (1 - p[W2])^(50 / tauU)
  # rounding downwards for tau > 50 a and upwards for tau < 50 a:
  R50L <- ifelse(tauL > 50, f100(R50L), c100(R50L))
  R50U <- ifelse(tauU > 50, f100(R50U), c100(R50U))
  # applying Equation 9:
  L <- (R50L * weight + R50U) / (weight + 1)
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

# Read the dataset "Norwegian Red List for species 2021"
RL <- read.csv2(file, as.is=TRUE, dec=".", na.strings="n/a")

# Check whether the data are as expected
RLCat <- unique(unlist(RL[, which(substr(names(RL), 1, 5) == "Categ")])) %-% ""
if (all(nchar(RedListCat) == 2)) {
  RLCat <- unique(substr(RLCat, 1, 2))
}
if (all(RLCat %in% RedListCat)) {
  cat("Red List Categories are OK.\n")
} else {
  cat("NB: The Red List Categories specified in the \"Constants\" section\n(" %+%
      paste(sort(RedListCat), collapse=", ") %+%
      ")\nare incompatible with the Red List Categories in the data file\n("  %+%
      paste(sort(RLCat), collapse=", ") %+% ")!\n")
}
# ¤¤¤ do the same for threats!

# Create a list to summarise the Red Lists for 2010, 2015 and 2021.
Table3 <- matrix(as.numeric(NA), 9, length(RedListCat) + 3, dimnames=list(
  "RL" %+% c("2010<", "2010", "2010(15)", "2010(21)",
             "2015<", "2015", "2015(21)", "2021<", "2021"),
  c("N", RedListCat, "RLI", "Cum.ELS50")
))
alphabetic <- sort(RedListCat)
Table3[1, match(alphabetic, colnames(Table3))] <-
  c(284, 809, 890, 16762, 2580, 6528, 1310, 127, 1265)
Table3[2, match(alphabetic, colnames(Table3))] <-
  c(290, 809, 908, 16745,   NA,   NA, 1302, 127, 1266)
Table3[5, match(alphabetic, colnames(Table3))] <-
  c(247, 755, 901, 17594, 3018, 6095, 1302, 119, 1294)
Table3[6, match(alphabetic, colnames(Table3))] <- 
  c(252, 755, 916, 17579,   NA,   NA, 1297, 119, 1294)
tb <- table(substr(RL$Categ21, 1, 2))
Table3[8, match(names(tb), colnames(Table3))] <- tb
Loss21o <- LoS(substr(RL$Categ21, 1, 2), RL$GenTime)

# Prepare the data frame for analyses
years <- identifyYears(RL)
threats <- identifyThreats(RL)
RL <- uplist(RL, "<")
RL <- backCast(RL)
RL <- calcLoss(RL, tau=RL$GenTime, includeDD=includeDD)

# RLIs for the three Red Lists, corrected for knowledge in the most recent one
RLI21 <- RLI(RL$Categ21.21)
RLI15 <- RLI(RL$Categ15.21)
RLI10 <- RLI(RL$Categ10.21)

# Summarise the Red Lists
# Data for Red Lists 2010 and 2015 (prior to back-casting) have to be added
# manually, because they cannot be re-created fully from the 2021 Red List data.
# Source for these data: Artsdatabanken (2010, 2015), i.e.:
# * http://www.artsportalen.artsdatabanken.no/
# * https://www.artsdatabanken.no/Rodlista2015 
tab <- summariseRL(RL)
Table3[c(4, 7, 9), colnames(tab)] <- tab[c(3, 5, 6), ]
Table3[3, ] <- Table3[4, ] - Table3[7, ] + Table3[6, ]
Table3[, "N"] <- apply(Table3[, RedListCat], 1, sum, na.rm=T)
Table3[, "RLI"] <- 1 - apply(t(Table3[,LC.EX]) * 0:5, 2, sum, na.rm=T) / 
  apply(Table3[, LC.EX], 1, sum, na.rm=T) / max(RLweights)
Table3 <- Table3[, !is.na(apply(Table3 > 0, 2, any))]
# Means per Red List Category
# (needed to approximate species loss for data that are not based on the 2021 Red List)
mn10 <- mn15 <- rep(0, length(RedListCat))
names(mn10) <- names(mn15) <- RedListCat
for (i in RedListCat) {
  mn10[i] <- mean(RL$Loss10.21[which(RL$Categ10.21 == i)])
  mn15[i] <- mean(RL$Loss15.21[which(RL$Categ15.21 == i)])
}
Table3[1, "Cum.ELS50"] <- sum(mn10 * Table3[1, RedListCat], na.rm=T)
Table3[2, "Cum.ELS50"] <- sum(mn10 * Table3[2, RedListCat], na.rm=T)
Table3[3, "Cum.ELS50"] <- sum(mn15 * Table3[3, RedListCat], na.rm=T)
Table3[5, "Cum.ELS50"] <- sum(mn15 * Table3[5, RedListCat], na.rm=T)
Table3[6, "Cum.ELS50"] <- sum(mn15 * Table3[6, RedListCat], na.rm=T)
Loss21o[which(isDD(RL$Categ21))] <- RL$Loss21[which(isDD(RL$Categ21))]
Table3[8, "Cum.ELS50"] <- sum(Loss21o, na.rm=T)
rm(alphabetic, tb, tab, mn10, mn15, Loss21o)
print(Table3)



# ========================
# Analyse threat factors
# ========================

#¤ construction site!!!

addThreats <- function(RL) {
  # Adds columns for each Red List and each threat to the data frame
  years <- sort(identifyYears(RL))
  for (y in years) {
    t <- "Threat" %+% y
    C <- "Categ"  %+% y %+% "." %+% max(y)
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
        RL[i, t] <- gsub("onlypast", "ongoingt", RL[i, t]) #¤ generalise!!!
        RL[i, t] <- gsub("suspendd", "ongoingt", RL[i, t])
        RL[i, t] <- gsub("onlyfutu", "ongoingt", RL[i, t])
        RL[i, t] <- gsub("unknownt", "ongoingt", RL[i, t])
      }
      if (nchar(RL[i, t])) {
        if (RL[i, t] %contains% "ongoingt") {
          P <- unlist(strsplit(RL[i, t], ","))
        } else {
          # If there is no ongoing threat for a threatened species,
          # an ongoing unknown threat is added
          P <- "unknownf:ongoingt:unknownp:unknownd" #¤ generalise!!!
        }
      } else {
          # If there is no threat at all for a threatened species,
          # an ongoing unknown threat is added
        P <- "unknownf:ongoingt:unknownp:unknownd" #¤ generalise!!!
      }
      for (j in 1:length(P)) {
        if (P[j] %contains% "ongoingt") {
          # Threat factors receive score according to their severity
          if (P[j] %contains% "rapidecl") sever <- rapidecl #¤ generalise!!!
          if (P[j] %contains% "slowdecl") sever <- slowdecl
          if (P[j] %contains% "negldecl") sever <- negldecl
          if (P[j] %contains% "unknownd") sever <- unknownd
          # Severity scores are summed for each species
          RL[i,           "Pop" %+% y] <- RL[i,           "Pop" %+% y] + sever
          RL[i,substr(P[j],1,8) %+% y] <- RL[i,substr(P[j],1,8) %+% y] + sever #¤ generalise!!!
        } # if ongoing
      } # j
   } # i
  } # y
  return(RL)
} # addThreats



# ========================
# DeltaRLI (attributing RLI trends to threats)
# ========================

# Tabulate, for each threat, DeltaRLI values from 2015 to 2021,
# from 2010 to 2015 and from 2010 to 2021 
DRLIp <- DRLIm <- matrix(0, length(threats), 3,
                         dimnames=list(threats, c("RL10.15", "RL15.21", "RL10.21")))
W10 <- RLW(RL$cat10.21)
W15 <- RLW(RL$cat15.21)
W21 <- RLW(RL$cat21)
L <- length(which(RL$cat21 %in% LC.EX))
for (p in threats) {
  # applying Equation 3:
  THR10 <- RL[,p %+% 10] / ifelse(RL[,"Pop" %+% 10] == 0, 1, RL[,"Pop" %+% 10])
  THR15 <- RL[,p %+% 15] / ifelse(RL[,"Pop" %+% 15] == 0, 1, RL[,"Pop" %+% 15])
  THR21 <- RL[,p %+% 21] / ifelse(RL[,"Pop" %+% 21] == 0, 1, RL[,"Pop" %+% 21])
  # applying Equation 5:
  DW10  <- W10*THR10 - W21*THR21
  DW15  <- W10*THR10 - W15*THR15
  DW21  <- W15*THR15 - W21*THR21
  # applying Equation 4 (separately for positive and negative DeltaRLI):
  DRLIp[p,1] <- 0.2 * sum(DW15[which(DW15 > 0 & W10 != W15)], na.rm=T) / L
  DRLIm[p,1] <- 0.2 * sum(DW15[which(DW15 < 0 & W10 != W15)], na.rm=T) / L
  DRLIp[p,2] <- 0.2 * sum(DW21[which(DW21 > 0 & W15 != W21)], na.rm=T) / L
  DRLIm[p,2] <- 0.2 * sum(DW21[which(DW21 < 0 & W15 != W21)], na.rm=T) / L
}
if (inferThreats) {
  n <- length(threats)
  for (i in 1:2) { # extrapolation to unknown threats
    DRLIp[,i] <- DRLIp[,i] * (1 + DRLIp[n,i] / sum(DRLIp[1:(n-1),i]))
    DRLIm[,i] <- DRLIm[,i] * (1 + DRLIm[n,i] / sum(DRLIm[1:(n-1),i]))
  }
  DRLIp[n,] <- DRLIm[n,] <- 0
}
DRLI <- DRLIp + DRLIm
DRLI[,3] <- DRLI[,1] + DRLI[,2]
# Tidy up
rm(W10, W15, W21, THR10, THR15, THR21, DW10, DW15, DW21, DRLIm, DRLIp)
# Show the results
print(DRLI)


# ------------------------
# Figure 1
# The following script recreates Fig 1 only if the
# above default parameterisation is left unchanged

# Simplify the table by collapsing minor threats
DRLI. <- DRLI
DRLI.[11,] <- sum(DRLI.[c(6:12),])
DRLI. <- DRLI.[c(1,11,2,4,3,5),]
DRLI.[,2] <- DRLI.[,1]
DRLI.[,1] <- 0
DRLI. <- DRLI.[, c(1:3,3)]
DRLI. <- rbind(0, DRLI.)
DRLI. <- rbind(DRLI., 0)

# Plot a graph for DeltaRLI
if (nchar(fig1)) {
 png(fig1, 1500, 1200, res=180)
}
par(mai=c(0.96, 0.96, 0.24, 0.06), family="sans") # ylim=c(0.9199, 0.92015)
plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(0.9198, 0.92022),
  xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="Red List Index",
  bty="n", cex.axis=1.2, cex.lab=1.8)
axis(1, c(2009, 2021.5), F, T, tcl=0, lwd=1.5, lend=1)
axis(1, 2009:2021, F, T, lwd=1.5, lend=1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.9198, 0.9203, 0.0001), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.9198, 0.92035, 0.00001), F, T, tcl=-0.25, lwd=1.5, lend=1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex=1.8)
x <- c(2010, 2015, 2021, 2023)
DRLI.[,4] <- DRLI.[,3] + c(0, 0, 0, 0, 0, 0, -0.00000887, 0)
lines(x[3:1], rep(RLI(RL$cat10.21), 3), lwd=9.6, lty="12", col=grey(0.84))
lines(x[1:3], c(RLI10, RLI15, RLI21), lwd=9.6, col=grey(0.84))
for (i in 2:7) {
  lines(x, RLI10 + DRLI.[i,], lty=i-1, lwd=2.4)
  points(x[2:3], RLI10 + DRLI.[i,2:3], pch=c(1,4,21,24,22,25,23)[i],
    cex=1.8, bg="black", lwd=2.4, ljoin=1)
  text(2023, RLI10 + DRLI.[i,4],
    c("", "land-use change", "other/unknown", "climate change", "pollution",
      "native species", "disturbance")[i],
    pos=4, cex=1.2)
}
if (nchar(fig1)) {
  dev.off()
}


# ------------------------
# Figure S1
# The following script recreates Fig S1 only if inferThreats == TRUE
# and the remaining parameterisation is left unchanged

# Simplify the table by collapsing minor threats
DRLI. <- DRLI
DRLI.[11,] <- sum(DRLI.[c(6:12),])
DRLI. <- DRLI.[c(1,11,2,4,3,5),]
DRLI.[,2] <- DRLI.[,1]
DRLI.[,1] <- 0
DRLI. <- DRLI.[, c(1:3,3)]
DRLI. <- rbind(0, DRLI.)
DRLI. <- rbind(DRLI., 0)

# Plot a graph for DeltaRLI
if (nchar(figS1)) {
 png(figS1, 1500, 1200, res=180)
}
par(mai=c(0.96, 0.96, 0.24, 0.06), family="sans") # ylim=c(0.9199, 0.92015)
plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(0.9198, 0.92032),
  xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="Red List Index",
  bty="n", cex.axis=1.2, cex.lab=1.8)
axis(1, c(2009, 2021.5), F, T, tcl=0, lwd=1.5, lend=1)
axis(1, 2009:2021, F, T, lwd=1.5, lend=1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.9198, 0.9203, 0.0001), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.9198, 0.92035, 0.00001), F, T, tcl=-0.25, lwd=1.5, lend=1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex=1.8)
x <- c(2010, 2015, 2021, 2023)
DRLI.[,4] <- DRLI.[,3] + c(0, 0, 0, 0, 0, 0, -0.00000876, 0)
lines(x[3:1], rep(RLI(RL$cat10.21), 3), lwd=9.6, lty="12", col=grey(0.84))
lines(x[1:3], c(RLI10, RLI15, RLI21), lwd=9.6, col=grey(0.84))
for (i in 2:7) {
  lines(x, RLI10 + DRLI.[i,], lty=i-1, lwd=2.4)
  points(x[2:3], RLI10 + DRLI.[i,2:3], pch=c(1,4,21,24,22,25,23)[i],
    cex=1.8, bg="black", lwd=2.4, ljoin=1)
  text(2023, RLI10 + DRLI.[i,4],
    c("", "land-use change", "other/unknown", "climate change", "pollution",
      "native species", "disturbance")[i],
    pos=4, cex=1.2)
}
if (nchar(figS1)) {
  dev.off()
}



# ========================
# deltaRLI (attributing RLI values to threats) and Expected Loss of Species
# ========================

# Tabulate, for each threat, deltaRLI values and ELS in 2010, 2015 and 2021
dRLI <- ELS <- matrix(0, length(threats), 3, dimnames=list(threats, "RL" %+% c(21, 15, 10)))
for (y in c(21, 15, 10)) {
  C <- "cat" %+% y
  if (y != 21) C <- C %+% ".21"
  L <- length(which(RL[,C] %in% LC.EX))
  Gw <- RLW(RL[,C])
  if (includeDD) {
    L <- length(which(RL[,C] %in% c(LC.EX, "DD")))
    Gw[which(RL[,C] == "DD")] <- weighted.mean(
      c(4, 3, 0, 1, 5, 2),
      table(RL[which(RL[,C] %in% c("CR","EN","LC","NT","RE","VU")), C]))
  }
  for (p in threats) {
    # applying Equation 3:
    THR <- RL[,p %+% y] / ifelse(RL[,"Pop" %+% y] == 0, 1, RL[,"Pop" %+% y])
    # applying Equation 6:
    dRLI[p, "RL" %+% y] <- 0.2 * sum(Gw * THR, na.rm=T) / L
    # applying Equation 7:
    ELS [p, "RL" %+% y] <- sum(RL[,"Loss" %+% y] * THR, na.rm=T)
  } # p
} # y
if (inferThreats) {
  n <- nrow(ELS)
  for (i in 1:3) { # extrapolation to unknown threats
     ELS[,i] <-  ELS[,i] * (1 +  ELS[n,i] / sum( ELS[1:(n-1),i]))
    dRLI[,i] <- dRLI[,i] * (1 + dRLI[n,i] / sum(dRLI[1:(n-1),i]))
  }
  ELS[n,] <- dRLI[n,] <- 0
}
# Tidy up
rm(C, y, L, Gw, THR)
# Show the results
print(dRLI)
print(ELS)


# ------------------------
# Figure 2
# The following script recreates Fig 2 only if the
# above default parameterisation is left unchanged

# Simplify the table by collapsing minor threats
dRLI. <- dRLI
ELS. <- ELS
for (i in 1:3)
 {dRLI.["otherthr", i] <- sum(dRLI.[c("otherthr", "unknownf", "alienspe", "huntharv",
    "outsiden", "natcatas", "bycatchc", "nothreat"), i])
  ELS. ["otherthr", i] <- sum(ELS. [c("otherthr", "unknownf", "alienspe", "huntharv",
    "outsiden", "natcatas", "bycatchc", "nothreat"), i])}
dRLI. <- dRLI.[-which(rownames(dRLI.) %in%
  c("unknownf","alienspe","huntharv","outsiden","natcatas","bycatchc","nothreat")),]
ELS.  <- ELS. [-which(rownames(ELS.)  %in%
  c("unknownf","alienspe","huntharv","outsiden","natcatas","bycatchc","nothreat")),]
ELS.  <- ELS. [order(dRLI.[,1], decreasing=T),]
ELS. <- ELS.[, c(3:1, 1)]
dRLI. <- dRLI.[order(dRLI.[,1], decreasing=T),]
dRLI. <- rbind(0, dRLI.)
dRLI. <- rbind(0, dRLI.)

# Plot a graph for deltaRLI
if (nchar(fig2)) {
 png(fig2, 1500, 1200, res=180)
}
par(mai=c(0.96, 0.96, 0.24, 0.06), family="sans")
plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(0.91, 1.002),
  xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="Red List Index",
  bty="n", cex.axis=1.2, cex.lab=1.8)
axis(1, c(2009, 2021.5), F, T, tcl=0, lwd=1.5, lend=1)
axis(1, 2009:2021, F, T, lwd=1.5, lend=1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.8, 1, 0.01), F, T, lwd=1.5, lend=1)
axis(2, seq(0.8, 1, 0.02), T, T, cex.axis=1.2, lwd=1.5, lend=1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex=1.8)
x <- c(2021, 2015, 2010)
for (i in 3:nrow(dRLI.)) {
  polygon(c(x, rev(x)),
          1 - c(apply(dRLI.[1:(i-1),], 2, sum), rev(apply(dRLI.[1:i,], 2, sum))),
          border=NA, col=grey(1.32 - i * 0.12))
}
y0 <- rep(RLI21, 3)
y2 <- 1 - (sum(dRLI.[1:8,1]) + sum(dRLI.[1:7,1])) / 2
for (i in 7:3) {
  y1 <- 1 - apply(dRLI.[1:i,], 2, sum)
  lines(x, y1, lwd=2.4)
  lines(c(2021, 2023), c(mean(c(y0[1], y1[1])), y2), lwd=1.8)
  text(2023, y2, 
    c("", "land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i],
    pos=4, cex=1.2)
  y0 <- y1
  y2 <- y2 + 0.005
}
lines(c(2021, 2023), rep(y2, 2), lwd=1.8)
text(2023, y2, "land-use change", pos=4, cex=1.2)
lines(x, rep(1,3), lwd=4.8)
lines(x, c(RLI21, RLI15, RLI10), lwd=4.8)
if (nchar(fig2)) {
  dev.off()
}


# ------------------------
# Figure 3
# The following script recreates Fig 3 only if the
# above default parameterisation is left unchanged

# Plot a graph for ELS50
if (nchar(fig3)) {
 png(fig3, 1500, 1200, res=180)
}
par(mai=c(0.96, 0.96, 0.24, 0.06), family="sans")
plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(lg(8), 3), xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="Expected loss of species", bty="n", cex.axis=1.2, cex.lab=1.8)
axis(1, c(2009, 2021.5), F, T, tcl=0, lwd=1.5, lend=1)
axis(1, 2009:2021, F, T, lwd=1.5, lend=1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, 0:3, c(1, 10, 100, 1000), T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, lg(c(2:9, seq(20, 90, 10), seq(200, 900, 100))), F, T, tcl=-0.25, lwd=1.5, lend=1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex=1.8)
x <- c(2010, 2015, 2021, 2023)
ELS.[3, 4] <- 10^(1/3 * lg(ELS.[4, 4]) + 2/3 * lg(ELS.[6, 4]))
ELS.[5, 4] <- 10^(2/3 * lg(ELS.[4, 4]) + 1/3 * lg(ELS.[6, 4]))
for (i in 1:6) {
  lines (x,      lg(ELS.[i,]), lty=i, lwd=2.4)
  points(x[1:3], lg(ELS.[i,1:3]), pch=c(4,21,24,22,25,23)[i],
    cex=1.8, bg="black", lwd=2.4, ljoin=1)
  text(2023, lg(ELS.[i,4]), 
    c("land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i],
    pos=4, cex=1.2)
}
if (nchar(fig3)) {
  dev.off()
}


# ------------------------
# Figure S2
# The following script recreates Fig S2 only if inferThreats == TRUE
# and the remaining parameterisation is left unchanged

# Simplify the table by collapsing minor threats
dRLI. <- dRLI
ELS. <- ELS
for (i in 1:3)
 {dRLI.["otherthr", i] <- sum(dRLI.[c("otherthr", "unknownf", "alienspe", "huntharv",
    "outsiden", "natcatas", "bycatchc", "nothreat"), i])
  ELS. ["otherthr", i] <- sum(ELS. [c("otherthr", "unknownf", "alienspe", "huntharv",
    "outsiden", "natcatas", "bycatchc", "nothreat"), i])}
dRLI. <- dRLI.[-which(rownames(dRLI.) %in%
  c("unknownf","alienspe","huntharv","outsiden","natcatas","bycatchc","nothreat")),]
ELS.  <- ELS. [-which(rownames(ELS.)  %in%
  c("unknownf","alienspe","huntharv","outsiden","natcatas","bycatchc","nothreat")),]
ELS.  <- ELS. [c(1, 6, 2, 4, 3, 5),]
ELS.  <- ELS. [, c(3:1, 1)]
dRLI. <- dRLI.[c(1, 6, 2, 4, 3, 5),]
dRLI. <- rbind(0, dRLI.)
dRLI. <- rbind(0, dRLI.)

# Plot a graph for deltaRLI
if (nchar(figS2)) {
 png(figS2, 1500, 1200, res=180)
}
par(mai=c(0.96, 0.96, 0.24, 0.06), family="sans")
plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(0.91, 1.002),
  xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="Red List Index",
  bty="n", cex.axis=1.2, cex.lab=1.8)
axis(1, c(2009, 2021.5), F, T, tcl=0, lwd=1.5, lend=1)
axis(1, 2009:2021, F, T, lwd=1.5, lend=1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.8, 1, 0.01), F, T, lwd=1.5, lend=1)
axis(2, seq(0.8, 1, 0.02), T, T, cex.axis=1.2, lwd=1.5, lend=1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex=1.8)
x <- c(2021, 2015, 2010)
for (i in 3:nrow(dRLI.)) {
  polygon(c(x, rev(x)),
          1 - c(apply(dRLI.[1:(i-1),], 2, sum), rev(apply(dRLI.[1:i,], 2, sum))),
          border=NA, col=grey(1.32 - i * 0.12))
}
y0 <- rep(RLI21, 3)
y2 <- 1 - (sum(dRLI.[1:8,1]) + sum(dRLI.[1:7,1])) / 2
for (i in 7:3) {
  y1 <- 1 - apply(dRLI.[1:i,], 2, sum)
  lines(x, y1, lwd=2.4)
  lines(c(2021, 2023), c(mean(c(y0[1], y1[1])), y2), lwd=1.8)
  text(2023, y2, 
    c("", "land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i],
    pos=4, cex=1.2)
  y0 <- y1
  y2 <- y2 + 0.005
}
lines(c(2021, 2023), rep(y2, 2), lwd=1.8)
text(2023, y2, "land-use change", pos=4, cex=1.2)
lines(x, rep(1,3), lwd=4.8)
lines(x, c(RLI21, RLI15, RLI10), lwd=4.8)
if (nchar(figS2)) {
  dev.off()
}


# ------------------------
# Figure S3
# The following script recreates Fig S3 only if inferThreats == TRUE
# and the remaining parameterisation is left unchanged

# Plot a graph for ELS50
if (nchar(figS3)) {
 png(figS3, 1500, 1200, res=180)
}
par(mai=c(0.96, 0.96, 0.24, 0.06), family="sans")
plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(1, lg(1200)), xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="Expected loss of species", bty="n", cex.axis=1.2, cex.lab=1.8)
axis(1, c(2009, 2021.5), F, T, tcl=0, lwd=1.5, lend=1)
axis(1, 2009:2021, F, T, lwd=1.5, lend=1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, c(1, lg(1200)), F, T, tcl=0, lwd=1.5, lend=1)
axis(2, 0:3, c(1, 10, 100, 1000), T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, lg(c(2:9, seq(20, 90, 10), seq(200, 900, 100))), F, T, tcl=-0.25, lwd=1.5, lend=1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex=1.8)
x <- c(2010, 2015, 2021, 2023)
ELS.[2, 4] <- 10^(3/4 * lg(ELS.[4, 4]) + 1/4 * lg(ELS.[6, 4]))
ELS.[5, 4] <- 10^(2/4 * lg(ELS.[4, 4]) + 2/4 * lg(ELS.[6, 4]))
ELS.[3, 4] <- 10^(1/4 * lg(ELS.[4, 4]) + 3/4 * lg(ELS.[6, 4]))
for (i in 1:6) {
  lines (x,      lg(ELS.[i,]), lty=i, lwd=2.4)
  points(x[1:3], lg(ELS.[i,1:3]), pch=c(4,21,24,22,25,23)[i],
    cex=1.8, bg="black", lwd=2.4, ljoin=1)
  text(2023, lg(ELS.[i,4]), 
    c("land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i],
    pos=4, cex=1.2)
}
if (nchar(figS3)) {
  dev.off()
}



# ========================
# Estimation of confidence intervals
# ========================

{ # confidence intervals on the RLI itself
  rlcat <- rlcat. <- n0(RL$Categ21)
  rli <- L <- 0
  w <- which(RL$Categ21 == "DD")
  for (i in 1:6) { # number of species in each Red List Category
    L[i] <- length(which(rlcat == LC.EX[i]))
  }
  # proportion of species in each Red List Category
  L <- L / sum(L)
  # proportion of species in each Red List Category or _lower_
  for (i in 6:2) {
    L[i] <- sum(L[1:i])
  }
  L <- c(0, L)
  for (i in 1:nsim) { # random assignment of DD species to other Red List Categories
    rlcat <- rlcat.
    zf <- runif(length(w))
    rlcat[w][which(zf >= L[1] & zf < L[2])] <- "RE"
    rlcat[w][which(zf >= L[2] & zf < L[3])] <- "CR"
    rlcat[w][which(zf >= L[3] & zf < L[4])] <- "EN"
    rlcat[w][which(zf >= L[4] & zf < L[5])] <- "VU"
    rlcat[w][which(zf >= L[5] & zf < L[6])] <- "NT"
    rlcat[w][which(zf >= L[6] & zf <=L[7])] <- "LC"
    rli[i] <- RLI(rlcat)
  }
  cat("Confidence intervals for RLI in 2021:\n")
  print(quantile(rli, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  rm(rlcat, rlcat., rli, w, L, zf)
}

# Remove rows that are not needed
if (includeDD) {
  RL <- RL[which(RL$cat21 %in% c(LC.EX, "DD")),]
} else {
  RL <- RL[which(RL$cat21 %in% LC.EX),]
}

# Determine the lower and upper limit of possible extinction probabilities
# for each species, depending on their Red List Category
X1 <- X2 <- W <- ELS <- dRLI <- DRLI <- DRLIp <- DRLIm <- list()
loss <- dr <- pop <- thr <- list()
W[[10]] <- RLW(RL$cat10.21)
W[[15]] <- RLW(RL$cat15.21)
W[[21]] <- RLW(RL$cat21)
for (y in c(10, 15, 21)) { # for each edition of the Red List
  ELS[[y]] <- dRLI[[y]] <- DRLI[[y]] <- DRLIp[[y]] <- DRLIm[[y]] <-
    matrix(0, length(threats), nsim, dimnames=list(threats, NULL))
  loss[[y]] <- dr[[y]] <- rep(0, nsim)
  X1[[y]] <- X2[[y]] <- rep(ExtProb[1], nrow(RL))
  X1[[y]][  which(W[[y]] == 1)] <- ExtProb[2] # lower limit of NT
  X2[[y]][  which(W[[y]] == 1)] <- ExtProb[3] # upper limit of NT
  X1[[y]][  which(W[[y]] == 2)] <- ExtProb[3] # lower limit if VU
  for (i in which(W[[y]] == 2)) { # upper limit of VU
    X2[[y]][i] <- min(ExtProb[4], ((1 - (1 - 0.20)^(50 / min(100, 5 * RL$GenTime[i])))))
    X2[[y]][i] <- ifelse(RL$GenTime[i] < 10, ceiling(X2[[y]][i] * 100),
                         floor(X2[[y]][i] * 100)) / 100
  }
  for (i in which(W[[y]] == 3)) { # lower and upper limits of EN
    X1[[y]][i] <- min(ExtProb[4], ((1 - (1 - 0.20)^(50 / min(100, 5 * RL$GenTime[i])))))
    X1[[y]][i] <- ifelse(RL$GenTime[i] < 10, ceiling(X1[[y]][i] * 100),
                         floor(X1[[y]][i] * 100)) / 100
    X2[[y]][i] <- min(ExtProb[5], ((1 - (1 - 0.50)^(50 / min(100, 3 * RL$GenTime[i])))))
    X2[[y]][i] <- ifelse(RL$GenTime[i] < 50/3, ceiling(X2[[y]][i] * 100),
                         floor(X2[[y]][i] * 100)) / 100
  }
  for (i in which(W[[y]] == 4)) { # lower limit of CR
    X1[[y]][i] <- min(ExtProb[5], ((1 - (1 - 0.50)^(50 / min(100, 3 * RL$GenTime[i])))))
    X1[[y]][i] <- ifelse(RL$GenTime[i] < 50/3, ceiling(X1[[y]][i] * 100),
                         floor(X1[[y]][i] * 100)) / 100
  }
  X2[[y]][  which(W[[y]] == 4)] <- ExtProb[6] # upper limit of CR
  X1[[y]][  which(W[[y]] == 5)] <- ExtProb[6] # lower limit of RE
  X2[[y]][  which(W[[y]] == 5)] <- ExtProb[6] # upper limit of RE
} # y

if (includeDD) { # Determine weights for the simulation of DD species
  DD <- numeric()
  for (p in c("RE", "CR", "EN", "VU", "NT", "LC")) {
    DD <- c(DD, length(which(RL$cat21 == p)))
  }
  DD <- round(DD / sum(DD) * nsim)
  while (sum(DD) > nsim) {
    DD[6] <- DD[6] - 1
  }
  while (sum(DD) < nsim) {
    DD[6] <- DD[6] + 1
  }
}

if (inferThreats) {
  # Count the occurrences of threats in order to extrapolate to unknown threats
  number <- antP <- antM <- matrix(0, length(threats), 24, dimnames=list(threats, NULL))
  for (y in c(21, 15, 10)) { # for each edition of the Red List
    husk <- 0
    C <- "cat" %+% y
    if (y != 21) C <- C %+% ".21"
    for (p in (threats %-% "unknownf")) {
      THR <- RL[,p %+% y] / ifelse(RL[,"Pop" %+% y] == 0, 1, RL[,"Pop" %+% y])
      number[p,y] <- sum(THR[which(RL[,C]  %in% c("RE", "CR", "EN", "VU", "NT"))])
    }
    number[,y] <- round(number[,y] / sum(number[,y]) * nsim)
    while (sum(number[,y]) > nsim) {
      number["landusec",y] <- number["landusec",y] - 1
    }
    while (sum(number[,y]) < nsim) {
      number["landusec",y] <- number["landusec",y] + 1
    }
  }
}

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
for (i in 1:nrow(RL)) {
  for (y in c(10, 15, 21)) {
    if (!(W[[10]][i] %=% 0 & W[[15]][i] %=% 0 & W[[21]][i] %=% 0)) {
      # ignore species that are LC in all years,
      # as they don't contribute to any of the metrics
      t <- "Threat" %+% y
      if (is.na(W[[y]][i])) { # extrapolation to DD species
        if (y %=% 10) {
          if (DD[1]) { # RE
            w <- 1:DD[1]
            DDW[w] <- rep(RLweights[6], DD[1])
            LSS[w] <- rep(ExtProb[6],   DD[1])
          }
          if (DD[2]) { # CR 
            w <- (sum(DD[1:1])+1):sum(DD[1:2])
            DDW[w] <- rep(RLweights[5], DD[2])
            LSS[w] <- rDecr(DD[2],
              ifelse(RL$GenTime[i] < 10 / 3, ExtProb[5],
                ifelse(RL$GenTime[i]   <  50/3, c100(1 - 0.5^(50 / 3 / RL$GenTime[i])),
                  ifelse(RL$GenTime[i] < 100/3, f100(1 - 0.5^(50 / 3 / RL$GenTime[i])),
                    f100(1 - (1 - 0.50)^(50 / 100))
                  )
                )
              )
            )
          }
          if (DD[3]) { # EN
            w <- (sum(DD[1:2])+1):sum(DD[1:3])
            DDW[w] <- rep(RLweights[4], DD[3])
            LSS[w] <- runif(DD[3],
              ifelse(RL$GenTime[i] < 4, ExtProb[4],
                ifelse(RL$GenTime[i]   < 10, c100(1 - 0.8^(10 / RL$GenTime[i])),
                  ifelse(RL$GenTime[i] < 20, f100(1 - 0.8^(10 / RL$GenTime[i])),
                    f100(1 - (1 - 0.20)^(50 / 100))
                  )
                )
              ),
              ifelse(RL$GenTime[i] < 10 / 3, ExtProb[5],
                ifelse(RL$GenTime[i]   <  50/3, c100(1 - 0.5^(50 / 3 / RL$GenTime[i])),
                  ifelse(RL$GenTime[i] < 100/3, f100(1 - 0.5^(50 / 3 / RL$GenTime[i])),
                    f100(1 - (1 - 0.50)^(50 / 100))
                  )
                )
              )
            )
          }
          if (DD[4]) { # VU
            w <- (sum(DD[1:3])+1):sum(DD[1:4])
            DDW[w] <- rep(RLweights[3], DD[4])
            LSS[w] <- runif(DD[4], ExtProb[3],
              ifelse(RL$GenTime[i] < 4, ExtProb[4],
                ifelse(RL$GenTime[i]   < 10, c100(1 - 0.8^(10 / RL$GenTime[i])),
                  ifelse(RL$GenTime[i] < 20, f100(1 - 0.8^(10 / RL$GenTime[i])),
                    f100(1 - (1 - 0.20)^(50 / 100))
                  )
                )
              )
            )
          }
          if (DD[5]) { # NT
            w <- (sum(DD[1:4])+1):sum(DD[1:5])
            DDW[w] <- rep(RLweights[2], DD[5])
            LSS[w] <- runif(DD[5], ExtProb[2], ExtProb[3])
          }
          if (DD[6]) { # LC
            w <- (sum(DD[1:5])+1):sum(DD[1:6])
            DDW[w] <- rep(RLweights[1], DD[6])
            LSS[w] <- rep(ExtProb[1],   DD[6])
          }
          o <- sample(1:nsim)
          DDW <- DDW[o]
          LSS <- LSS[o]
        }
      } else {
        DDW <- rep(W[[y]][i], nsim)
        if (W[[y]][i] == 4) {
          LSS <- rDecr(nsim, X1[[y]][i])
        } else {
          LSS <- runif(nsim, X1[[y]][i], X2[[y]][i])
        }
      }
      # cumulative loss of species in a given year y
      loss[[y]] <- loss[[y]] + LSS
      # total tpopulation decline for species i
      pop[[y]] <- rep(0, nsim)
      # threat-wise population declines for species i
      thr[[y]] <- matrix(0, length(threats), nsim, dimnames=list(threats, NULL))
      if (nchar(RL[i,t])) {
        if (RL[i,t] %contains% "ongoingt") {
          P <- unlist(strsplit(RL[i,t], ","))
        } else {
          P <- "unknownf:ongoingt:unknownp:unknownd"
          # If there is no ongoing threat for a threatened species,
          # an ongoing unknown threat is added
        }
      } else {
        P <- "unknownf:ongoingt:unknownp:unknownd"
        # If there is no threat at all for a threatened species,
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
                Z[k] <- number[threats[k], y]
                Q[k] <- threats[k] %+% substr(P[j], 9, 35)
                w[[k]] <- 1:Z[k]
                if (k > 1) {
                  w[[k]] <- w[[k]] + sum(number[threats[1:(k-1)], y])
                }
              } # k
            } # if unknown threat
          } # if infer threats
          o <- sample(1:nsim)
          for (k in 1:length(Z)) {
            if (Z[k]) {
              sever <- rep(0, nsim)
              # Threat factors receive randomised scores according to their severity
              if (Q[k] %contains% "rapidecl") sever[w[[k]]] <- rDecr(Z[k],       	  SEV[3])
              if (Q[k] %contains% "slowdecl") sever[w[[k]]] <- runif(Z[k], SEV[2], SEV[3])
              if (Q[k] %contains% "negldecl") sever[w[[k]]] <- rIncr(Z[k], SEV[2])
              if (Q[k] %contains% "unknownd") sever[w[[k]]] <- rbeta(Z[k],     2,    beta)
              # shuffle the values (needed when inferThreats == T)
              sever <- sever[o]
              # Severity scores are summed for each species
              pop[[y]]                    <- pop[[y]]                    + sever
              thr[[y]][substr(Q[k],1,8),] <- thr[[y]][substr(Q[k],1,8),] + sever
            }
          } # k
        } # if ongoing
      } # j
      # applying Equations 7 and 6:
      ELS [[y]] <-  ELS[[y]] + thr[[y]] * m(LSS) / ifelse(thr[[y]] == 0, 1, m(pop[[y]]))
      dRLI[[y]] <- dRLI[[y]] + thr[[y]] * m(DDW) / ifelse(thr[[y]] == 0, 1, m(pop[[y]]))
      dr[[y]] <- dr[[y]]+apply(thr[[y]] * m(DDW) / ifelse(thr[[y]] == 0, 1, m(pop[[y]])),
        2, sum)
      if (!is.na(W[[y]][i])) {
        # applying Equation 5 and 4:
        if (y == 15 &  W[[10]][i] != W[[15]][i]) {
          DW <- W[[10]][i] * thr[[10]] / ifelse(thr[[10]] == 0, 1, m(pop[[10]])) -
                W[[15]][i] * thr[[15]] / ifelse(thr[[15]] == 0, 1, m(pop[[15]]))
          DRLIp[[15]] <- DRLIp[[15]] + ifelse(DW > 0, DW, 0)
          DRLIm[[15]] <- DRLIm[[15]] + ifelse(DW < 0, DW, 0)
        }
        if (y == 21 &  W[[15]][i] != W[[21]][i]) {
          DW <- W[[15]][i] * thr[[15]] / ifelse(thr[[15]]==0,1,m(pop[[15]])) -
                W[[21]][i] * thr[[21]] / ifelse(thr[[21]]==0,1,m(pop[[21]]))
          DRLIp[[21]] <- DRLIp[[21]] + ifelse(DW > 0, DW, 0)
          DRLIm[[21]] <- DRLIm[[21]] + ifelse(DW < 0, DW, 0)
        }
      }
    } # if in RE-LC
  } # y
  if  (i == 1) cat("\n\n\n")
  cat ("\r" %+% round(100*i/nrow(RL)) %+% "% done.")
  if  (i == nrow(RL)) cat("\n\n\n")
} # i


{# Print the results
  for (y in c(15, 21))
   {DRLI[[y]] <- DRLIp[[y]] + DRLIm[[y]]}
  DRLI[[10]]  <- DRLI[[15]] + DRLI[[21]]
  cat("Confidence intervals for DeltaRLI from 2010 to 2021:\n")
  print(apply(DRLI[[10]] / 5 / nrow(RL), 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  cat("\n\nConfidence intervals for the cumulative deltaRLI in 2021:\n")
  print(quantile(dr[[21]] / 5 / nrow(RL), c(0.025, 0.25, 0.5, 0.75, 0.975)))
  cat("\n\nConfidence intervals for the threat-wise deltaRLIs in 2021:\n")
  print(apply( dRLI[[21]] / 5 / nrow(RL), 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  cat("\n\nConfidence intervals for the cumulative ELS50 in 2021:\n")
  print(quantile(loss[[21]], c(0.025, 0.25, 0.5, 0.75, 0.975)))
  cat("\n\nConfidence intervals for the threat-wise ELS50 in 2021:\n")
  print(apply(ELS[[21]], 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975)))
}

