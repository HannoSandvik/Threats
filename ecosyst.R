
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

# Defines the weighting scheme for the Red List Index
# (defaults to "equal-steps"; other options are the IUCN Red List Criteria
#  "A1", "A2", "B1", "B2", "C", "D" and "E" as well as "Ev2", "Ev3")
weightingRLI <- "equal-steps"

# Defines the weighting scheme for the Expected Loss of Species
# (defaults to using the thresholds of the IUCN Red List Criterion E;
#  other options are "A1", "A2", "B1", "B2", "C", "D", "Ev2", "Ev3" and
#  "equal-steps")
weightingELS <- "E"

# What is the abbreviation used for real population changes
# (defaults to the abbreviation used in the dataset analysed in the paper)
realChange <- "realpopu"

# What are the abbreviation used for unknown threats
# (defaults to the abbreviation used in the dataset analysed in the paper)
unknownThreat   <- "unknownf"
unknownTiming   <- "unknownt"
unknownScope    <- "unknownp"
unknownSeverity <- "unknownd"

# What is (are) the abbreviation(s) of the timing categories that should be 
# considered (defaults to "ongoing")
inclTiming <- "ongoingt"
# To include _all_ threats, irrespective of timing, use:
# inclTiming <- c("onlypast", "suspendd", "ongoing", "onlyfutu", "unknownt")

# File names of figures. If you want to display the figures on screen,
# keep the default. If you want to create PNG file, specify the file names
# (including paths).
fig1  <- ""
fig2  <- ""
fig3  <- ""
figS1 <- ""
figS2 <- ""
figS3 <- ""

# Column names in the dataset which contain Red List Categories, threat factors,
# reasons for change in category, and generation time, respectively, followed by
# the year (So if the column name containing Red List Categories is _not_ named
#  something like "Categ21" or "Categ2021", this needs to be adjusted here!)
Categ  <- "Categ"
Threat <- "Threat"
Change <- "Change"
GTime  <- "GenTime"



# ========================
# Constants 
# ========================

# Constants that do not normally need to be changed.
# Change only if you want to modify the underlying assumptions!

# (1) Red List Categories and their weights, extinction probabilities etc.
# This data frame needs to contain all Red List Categories used in the 
# Red List analysed of species or ecosystems that have been evaluated.
# ...
# The column "LC" identifies the Red List Category "Least Concern".
# The column "EX" identified the Red List Categories for extinction.
# The column "wt" ...
# ...
RLcateg <- data.frame(
  name  = c(  "LC",   "NT",   "VU",   "EN",   "CR",   "RE",   "EW",   "EX"),
  LC    = c(  TRUE,  FALSE,  FALSE,  FALSE,  FALSE,  FALSE,  FALSE,  FALSE),
  EX    = c( FALSE,  FALSE,  FALSE,  FALSE,  FALSE,   TRUE,   TRUE,   TRUE),
  wt    = c(     0,      1,      2,      3,      4,      5,      5,      5),
  lowP  = c(  0.00,   0.05,   0.10,   0.20,   0.50,   1.00,   1.00,   1.00),
  uppP  = c(  0.00,   0.10,   0.20,   0.50,   1.00,   1.00,   1.00,   1.00),
  lowT  = c(   100,    100,    100,     20,     10,     10,     10,     10),
  uppT  = c(   100,    100,     20,     10,     10,     10,     10,     10),
  lowG  = c(     0,      0,      0,      5,      3,      1,      1,      1),
  uppG  = c(     0,      0,      5,      3,      1,      1,      1,      1),
  lowA1 = c(  0.00,   0.25,   0.50,   0.70,   0.90,   1.00,   1.00,   1.00),
  uppA1 = c(  0.00,   0.50,   0.70,   0.90,   1.00,   1.00,   1.00,   1.00),
  lowA2 = c(  0.00,   0.15,   0.30,   0.50,   0.80,   1.00,   1.00,   1.00),
  uppA2 = c(  0.00,   0.30,   0.50,   0.80,   1.00,   1.00,   1.00,   1.00),
  lowB1 = c( 40000,  40000,  20000,   5000,    100,      0,      0,      0),
  uppB1 = c( 40000,  20000,   5000,    100,      0,      0,      0,      0),
  lowB2 = c(  4000,   4000,   2000,    500,     10,      0,      0,      0),
  uppB2 = c(  4000,   2000,    500,     10,      0,      0,      0,      0),
  lowC  = c( 20000,  20000,  10000,   2500,    250,      0,      0,      0),
  uppC  = c( 20000,  10000,   2500,    250,      0,      0,      0,      0),
  lowD  = c(  2000,   2000,   1000,    250,     50,      0,      0,      0),
  uppD  = c(  2000,   1000,    250,     50,      0,      0,      0,      0),
  distr = c("unif", "unif", "unif", "unif", "decr", "unif", "unif", "unif"),
  beta  = c(    NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA),
  stringsAsFactors = FALSE
)

# What is the abbreviation used for the "Data Deficient" Red List Category
# (defaults to IUCN's abbreviation)
DD <- "DD"

# Which Red List Categories exist for species or ecosystems that have _not_ been
# evaluated (defaults to IUCN's abbreviations for the Red List Categories
# "Not Applicable" and "Not Evaluated")
notEval <- c("NA", "NE")

# What is added to a Red List Category to indicate downlisting
# (defaults to the degree symbol)
downlistSymbol <- "°"

# Severities of severities and their threshold values
# This data frame needs to contain all severity categories used in the 
# Red List analysed.
# The data frame defaults to the severity categories used in Norwegian Red Lists,
# where values correspond to the declines in population size over 10 years or
# 3 generations (whichever is largest) caused by a threat.
# The column "name" contains the abbreviations used for the severity categories.
# The column "lower" contains the lower limit of the respective interval.
# The column "upper" contains the upper limit of the respective interval.
# The column "distr" contains the distribution of values within the respective 
# interval (possible values: "unif", "incr", "decr", "beta").
# The column "beta" contains the beta parameter of a Beta distribution
# (a numeric values if distr == "beta", and "NA" otherwise).
Severity <- data.frame(
  name  = c("negldecl", "slowdecl", "rapidecl", "unknownd"),
  lower = c(      0.00,       0.02,       0.20,       0.00),
  upper = c(      0.02,       0.20,       1.00,       1.00),
  distr = c(    "incr",     "unif",     "decr",     "beta"),
  beta  = c(        NA,         NA,         NA,         20),
  stringsAsFactors = FALSE
)

# Time frame for the expected loss of species or ecosystems, in years
# (defaults to 50 years)
TimeFrame <- 50



# ========================
# Preparations
# ========================

# Definition of further required variables, 
# based on the variables and constants specified above

LC      <- RLcateg$name[RLcateg$LC]
extinct <- RLcateg$name[RLcateg$EX]
LC.EX   <- RLcateg$name
RedListCat <- c(LC.EX, DD, notEval)



# ========================
# Read, prepare and summarise the data
# ========================

# Read the dataset "Norwegian Red List for species 2021"
RL <- read.csv2(file, as.is=TRUE, dec=".", na.strings="n/a", encoding="latin1")

# Check whether the data are as expected
usedCategories <- checkRL(RL)

# Ensure that `RedListCat` and `LC.EX` only contain categories 
# that are actually used
RedListCat <- RedListCat %A% usedCategories
LC.EX      <-      LC.EX %A% usedCategories

# Create a list to summarise the Red Lists for 2010, 2015 and 2021.
Table3 <- matrix(as.numeric(NA), 9, length(RedListCat) + 3, dimnames=list(
  "RL" %+% c("2010" %+% downlistSymbol, "2010", "2010(15)", "2010(21)",
             "2015" %+% downlistSymbol, "2015", "2015(21)", 
             "2021" %+% downlistSymbol, "2021"),
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
RL <- uplist(RL) 
RL <- backCast(RL)
RL <- calcLoss(RL)

# RLIs for the three Red Lists, corrected for knowledge in the most recent one
RLI21 <- RLI(RL$Categ21.21, RL$GenTime)
RLI15 <- RLI(RL$Categ15.21, RL$GenTime)
RLI10 <- RLI(RL$Categ10.21, RL$GenTime)

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
Table3[, "RLI"] <- 1 - apply(t(Table3[,LC.EX]) * RLW(LC.EX), 2, sum, na.rm=T) / 
  apply(Table3[, LC.EX], 1, sum, na.rm=T) / max(RLW(LC.EX))
Table3 <- Table3[, !is.na(apply(Table3 > 0, 2, any))]
# Means per Red List Category
# (needed to approximate species loss for data 
#  that are not based on the 2021 Red List)
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
# Analysis of threat factors
# ========================

# Add columns for each threat factor
RL <- addThreats(RL)

# Estimate DeltaRLI
DRLI <- DeltaRLI(RL)
print(DRLI)

# Estimate dRLI and ELS50
drli <- dRLI(RL)
print(drli)

# Please note that the following two commands take a looong time
# if you use the default value for `nsim`!
# Replace `nsim` by 100 to just see how they work.

# Confidence intervals on RLI
print(confidenceRLI(RL, nsim, "Categ21"))

# Confidence intervals on DeltaRLI, dRLI and ELS50
results <- simulateDRLI(RL, nsim)



# ========================
# Figures
# ========================

# ------------------------
# Figure 1
# The following script recreates Fig 1 only if the
# above default parameterisation is left unchanged

# Simplify the table by collapsing minor threats
DRLI. <- DRLI
DRLI.[10, ] <- apply(DRLI.[c(1, 2, 5, 7, 9, 10, 11, 13), ], 2, sum)
DRLI. <- DRLI.[c(6, 10, 3, 12, 8, 4), ]
DRLI. <- DRLI.[, c(1:3, 3)]
DRLI. <- rbind(0, DRLI.)
DRLI. <- rbind(DRLI., 0)

# Plot a graph for DeltaRLI
if (nchar(fig1)) {
 png(fig1, 1500, 1200, res = 180)
}
par(mai = c(0.96, 0.96, 0.12, 0.06), family = "sans") # xlim = c(2009.5, 2027.5)
plot(0, 0, xlim = c(2009.5, 2029.5), ylim = c(0.9198, 0.92022),
  xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", 
  ylab = "Red List Index",
  bty = "n", cex.axis = 1.2, cex.lab = 1.8)
axis(1, c(2009, 2021.5),              F, T, tcl = 0,        lwd = 1.5, lend = 1)
axis(1, 2009:2021,                    F, T,                 lwd = 1.5, lend = 1)
axis(1, c(2010, 2015, 2021),          T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, seq(0.9198, 0.9202, 0.00010), T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, seq(0.9198, 0.9202, 0.00001), F, T, tcl = -0.25,    lwd = 1.5, lend = 1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex = 1.8)
#x <- c(2010, 2015, 2021, 2023)
x <- c(2010, 2015, 2021, 2022)
#DRLI.[, 4] <- DRLI.[, 3] + c(0, 0, 0, 0, 0, 0, -0.00000887, 0)
DRLI.[, 4] <- DRLI.[, 3] + c(0, 0, 0, 0, 0, 0, -0.00000724, 0)
lines(x[4:1], rep(RLI10, 4), lty = "12",     lwd = 9.6, col = grey(0.84))
lines(x[1:4], c(RLI10, RLI15, RLI21, RLI21), lwd = 9.6, col = grey(0.84))
for (i in 2:7) {
  lines(x, RLI10 + DRLI.[i, ], lty = i - 1, lwd = 2.4)
  points(x[2:3], RLI10 + DRLI.[i, 2:3], pch = c(1, 4, 21, 24, 22, 25, 23)[i],
    cex = 1.8, bg = "black", lwd = 2.4, ljoin = 1)
  #text(2023, RLI10 + DRLI.[i, 4],
  text(2022, RLI10 + DRLI.[i, 4],
    c("", "land-use change", "other/unknown", "climate change", "pollution",
      "native species", "disturbance")[i] %+% " (" %+%
      c(rep("+", 3), rep("", 5))[i] %+% 
      gsub("-", "−", formatC(DRLI.[i, 3], 6, format="f")) %+% ")",
    pos = 4, cex = 1.2)
}
text(2022, RLI21, "RLI (+0.000096)", pos = 4, cex = 1.2)
text(2022, RLI10, "no change (+0.000000)", pos = 4, cex = 1.2)
text(2015.5, 0.92021, expression(bold(Delta*RLI)), cex = 1.8)
if (nchar(fig1)) {
  dev.off()
}


# ------------------------
# Figure S1
# The following script recreates Fig S1 only if inferThreats == TRUE
# and the remaining parameterisation is left unchanged

# Simplify the table by collapsing minor threats
DRLI. <- DRLI
DRLI.[10, ] <- apply(DRLI.[c(1, 2, 5, 7, 9, 10, 11, 13), ], 2, sum)
DRLI. <- DRLI.[c(6, 10, 3, 12, 8, 4), ]
DRLI. <- DRLI.[, c(1:3, 3)]
DRLI. <- rbind(0, DRLI.)
DRLI. <- rbind(DRLI., 0)

# Plot a graph for DeltaRLI
if (nchar(figS1)) {
 png(figS1, 1500, 1200, res = 180)
}
par(mai = c(0.96, 0.96, 0.12, 0.06), family = "sans") # ylim=c(0.9199, 0.92015)
plot(0, 0, xlim = c(2009.5, 2027.5), ylim = c(0.9198, 0.92033),
  xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", 
  ylab = "Red List Index",
  bty = "n", cex.axis = 1.2, cex.lab = 1.8)
axis(1, c(2009, 2021.5),               F, T, tcl = 0,        lwd = 1.5, lend = 1)
axis(1, 2009:2021,                     F, T,                 lwd = 1.5, lend = 1)
axis(1, c(2010, 2015, 2021),           T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, seq(0.9198, 0.9203, 0.00010), T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, seq(0.9198, 0.9203, 0.00001), F, T, tcl = -0.25,    lwd = 1.5, lend = 1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex = 1.8)
x <- c(2010, 2015, 2021, 2023)
DRLI.[,4] <- DRLI.[,3] + c(0, 0, 0, 0, 0, 0, -0.00000876, 0)
lines(x[3:1], rep(RLI10, 3), lty = "12", lwd = 9.6, col = grey(0.84))
lines(x[1:3], c(RLI10, RLI15, RLI21),    lwd = 9.6, col = grey(0.84))
for (i in 2:7) {
  lines(x, RLI10 + DRLI.[i, ], lty = i - 1, lwd = 2.4)
  points(x[2:3], RLI10 + DRLI.[i, 2:3], pch=c(1, 4, 21, 24, 22, 25, 23)[i],
    cex = 1.8, bg = "black", lwd = 2.4, ljoin = 1)
  text(2023, RLI10 + DRLI.[i, 4],
    c("", "land-use change", "other/unknown", "climate change", "pollution",
      "native species", "disturbance")[i],
    pos = 4, cex = 1.2)
}
text(2015.5, 0.92032, expression(bold(Delta*RLI)), cex = 1.8)
if (nchar(figS1)) {
  dev.off()
}


# ------------------------
# Figure 2
# The following script recreates Fig 2 only if the
# above default parameterisation is left unchanged

# Simplify the table by collapsing minor threats
drli. <- drli$dRLI
ELS. <- drli$ELS50
for (i in 1:3) {
  drli.["otherthr", i] <- sum(drli.[c("otherthr", "unknownf", "alienspe",
    "huntgath", "outsiden", "natcatas", "bycatchc", "nothreat"), i])
  ELS. ["otherthr", i] <- sum(ELS. [c("otherthr", "unknownf", "alienspe",
    "huntgath", "outsiden", "natcatas", "bycatchc", "nothreat"), i])
}
drli. <- drli.[-which(rownames(drli.) %in%
  c("unknownf","alienspe","huntgath","outsiden","natcatas","bycatchc","nothreat")),]
ELS.  <- ELS. [-which(rownames(ELS.)  %in%
  c("unknownf","alienspe","huntgath","outsiden","natcatas","bycatchc","nothreat")),]
ELS.  <- ELS. [order(drli.[, 3], decreasing=T),]
ELS.  <- ELS. [, c(1:3, 3)]
drli. <- drli.[order(drli.[, 3], decreasing=T),]
drli. <- drli.[, 3:1]
drli. <- rbind(0, drli.)
drli. <- rbind(0, drli.)

# Plot a graph for dRLI
if (nchar(fig2)) {
 png(fig2, 1500, 1200, res=180)
}
par(mai=c(0.96, 0.96, 0.06, 0.06), family="sans")
# plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(0.91, 1.002),
plot(0, 0, xlim=c(2009.5, 2029.5), ylim=c(0.91, 1.008),
  xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="Red List Index",
  bty="n", cex.axis=1.2, cex.lab=1.8)
axis(1, c(2009, 2021.5),     F, T, tcl=0,        lwd=1.5, lend=1)
axis(1, 2009:2021,           F, T,               lwd=1.5, lend=1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis=1.2, lwd=1.5, lend=1)
axis(2, seq(0.8, 1, 0.01),   F, T,               lwd=1.5, lend=1)
axis(2, seq(0.8, 1, 0.02),   T, T, cex.axis=1.2, lwd=1.5, lend=1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex=1.8)
x <- c(2021, 2015, 2010)
for (i in 3:nrow(drli.)) {
  polygon(c(x, rev(x)),
          1 - c(apply(drli.[1:(i-1),], 2, sum), rev(apply(drli.[1:i,], 2, sum))),
          border=NA, col=grey(1.32 - i * 0.12))
}
y0 <- rep(RLI21, 3)
y2 <- 1 - (sum(drli.[1:8,1]) + sum(drli.[1:7,1])) / 2
#y2 <- RLI21 + 0.005
for (i in 7:3) {
  y1 <- 1 - apply(drli.[1:i,], 2, sum)
  lines(x, y1, lwd=2.4)
#  lines(c(2021, 2023), c(mean(c(y0[1], y1[1])), y2), lwd=1.8)
  lines(c(2021, 2022), c(mean(c(y0[1], y1[1])), y2), lwd=1.8)
#   text(2023, y2, 
  text(2022, y2, 
    c("", "land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i] %+%
      " (" %+% formatC(drli.[i + 1, 1], 4, format="f") %+% ")",
    pos=4, cex=1.2)
  y0 <- y1
  y2 <- y2 + 0.005
}
#lines(c(2021, 2023), rep(y2, 2), lwd=1.8)
lines(c(2021, 2022), rep(y2, 2), lwd=1.8)
#text(2023, y2, "land-use change", pos=4, cex=1.2)
text(2022, y2, "land-use change" %+%
               " (" %+% formatC(drli.[3, 1], 4, format="f") %+% ")",
     pos=4, cex=1.2)
lines(x, rep(1, 3), lwd=4.8)
lines(c(2021, 2022), rep(1, 2), lwd=1.8)
text(2022, 1, "reference value", font=2, pos=4, cex=1.2)
lines(x, c(RLI21, RLI15, RLI10), lwd=4.8)
lines(c(2021, 2022), c(RLI21, y2 - 0.03), lwd=1.8)
text(2022, y2 - 0.03, "RLI", font=2, pos=4, cex=1.2)
text(2015.5, 1.005, expression(bold(delta*RLI)), cex = 1.8)
if (nchar(fig2)) {
  dev.off()
}


# ------------------------
# Figure 3
# The following script recreates Fig 3 only if the
# above default parameterisation is left unchanged

# Plot a graph for ELS50
if (nchar(fig3)) {
 png(fig3, 1500, 1200, res = 180)
}
par(mai = c(0.96, 0.96, 0.06, 0.06), family = "sans")
# plot(0, 0, xlim=c(2009.5, 2027.5), ylim=c(lg(8), 3), 
plot(0, 0, xlim=c(2009.5, 2029.5), ylim=c(lg(8), 3.1), 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", 
     ylab = "Expected loss of species",
     bty = "n", cex.axis = 1.2, cex.lab = 1.8)
axis(1, c(2009, 2021.5),          F, T, tcl = 0,        lwd = 1.5, lend = 1)
axis(1, 2009:2021,                F, T,                 lwd = 1.5, lend = 1)
axis(1, c(2010, 2015, 2021),      T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, 0:3, c(1, 10, 100, 1000), T,    cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, lg(c(2:9, seq(20, 90, 10), seq(200, 900, 100))),
                                  F, T, tcl = -0.25,    lwd = 1.5, lend = 1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex = 1.8)
# x <- c(2010, 2015, 2021, 2023)
x <- c(2010, 2015, 2021, 2022)
ELS.[3, 4] <- 10^(1/3 * lg(ELS.[4, 4]) + 2/3 * lg(ELS.[6, 4]))
ELS.[5, 4] <- 10^(2/3 * lg(ELS.[4, 4]) + 1/3 * lg(ELS.[6, 4]))
for (i in 1:6) {
  lines (x,      lg(ELS.[i, ]), lty = i, lwd = 2.4)
  points(x[1:3], lg(ELS.[i, 1:3]), pch = c(4, 21, 24, 22, 25, 23)[i],
    cex = 1.8, bg = "black", lwd = 2.4, ljoin = 1)
# text(2023, lg(ELS.[i, 4]), 
  text(2022, lg(ELS.[i, 4]), 
    c("land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i] %+%
      " (" %+% round(ELS.[i, 3]) %+% ")",
    pos = 4, cex = 1.2)
}
text(2015.5, 3.01, expression(bold(ELS[50])), cex = 1.8)
if (nchar(fig3)) {
  dev.off()
}


# ------------------------
# Figure S2
# The following script recreates Fig S2 only if inferThreats == TRUE
# and the remaining parameterisation is left unchanged

# Simplify the table by collapsing minor threats
drli. <- drli$dRLI
ELS. <- drli$ELS50
for (i in 1:3) {
  drli.["otherthr", i] <- sum(drli.[c("otherthr", "unknownf", "alienspe", 
    "huntgath", "outsiden", "natcatas", "bycatchc", "nothreat"), i])
  ELS. ["otherthr", i] <- sum(ELS. [c("otherthr", "unknownf", "alienspe",
    "huntgath", "outsiden", "natcatas", "bycatchc", "nothreat"), i])
}
drli. <- drli.[-which(rownames(drli.) %in%
  c("unknownf", "alienspe", "huntgath", "outsiden", 
    "natcatas", "bycatchc", "nothreat")),]
ELS.  <- ELS. [-which(rownames(ELS.)  %in%
  c("unknownf", "alienspe", "huntgath", "outsiden",
    "natcatas", "bycatchc", "nothreat")),]
ELS. <-  ELS.[c("landusec", "otherthr", "climatec", 
                "pollutio", "nativesp", "disturba"), ]
ELS.  <- ELS. [, c(1:3, 3)]
drli. <- drli.[order(drli.[, 3], decreasing=T), ]
drli. <- drli.[, 3:1]
drli. <- rbind(0, drli.)
drli. <- rbind(0, drli.)

# Plot a graph for dRLI
if (nchar(figS2)) {
 png(figS2, 1500, 1200, res = 180)
}
par(mai = c(0.96, 0.96, 0.06, 0.06), family = "sans")
plot(0, 0, xlim = c(2009.5, 2027.5), ylim = c(0.91, 1.008),
  xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", 
  ylab = "Red List Index",
  bty = "n", cex.axis = 1.2, cex.lab = 1.8)
axis(1, c(2009, 2021.5),     F, T, tcl = 0,        lwd = 1.5, lend = 1)
axis(1, 2009:2021,           F, T,                 lwd = 1.5, lend = 1)
axis(1, c(2010, 2015, 2021), T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, seq(0.8, 1, 0.01),   F, T,                 lwd = 1.5, lend = 1)
axis(2, seq(0.8, 1, 0.02),   T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex = 1.8)
x <- c(2021, 2015, 2010)
for (i in 3:nrow(drli.)) {
  polygon(c(x, rev(x)),
          1 - c(apply(drli.[1:(i-1), ], 2, sum), rev(apply(drli.[1:i, ], 2, sum))),
          border = NA, col = grey(1.32 - i * 0.12))
}
y0 <- rep(RLI21, 3)
y2 <- 1 - (sum(drli.[1:8, 1]) + sum(drli.[1:7, 1])) / 2
for (i in 7:3) {
  y1 <- 1 - apply(drli.[1:i, ], 2, sum)
  lines(x, y1, lwd = 2.4)
  lines(c(2021, 2023), c(mean(c(y0[1], y1[1])), y2), lwd = 1.8)
  text(2023, y2, 
    c("", "land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i],
    pos = 4, cex = 1.2)
  y0 <- y1
  y2 <- y2 + 0.005
}
lines(c(2021, 2023), rep(y2, 2), lwd = 1.8)
text(2023, y2, "land-use change", pos = 4, cex = 1.2)
lines(x, rep(1,3), lwd = 4.8)
lines(x, c(RLI21, RLI15, RLI10), lwd = 4.8)
text(2015.5, 1.005, expression(bold(delta*RLI)), cex = 1.8)
if (nchar(figS2)) {
  dev.off()
}


# ------------------------
# Figure S3
# The following script recreates Fig S3 only if inferThreats == TRUE
# and the remaining parameterisation is left unchanged

# Plot a graph for ELS50
if (nchar(figS3)) {
 png(figS3, 1500, 1200, res = 180)
}
par(mai = c(0.96, 0.96, 0.06, 0.06), family = "sans")
plot(0, 0, xlim = c(2009.5, 2027.5), ylim = c(1, 3.2),
  xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "",
  ylab="Expected loss of species",
  bty = "n", cex.axis = 1.2, cex.lab = 1.8)
axis(1, c(2009, 2021.5),          F, T, tcl = 0,        lwd = 1.5, lend = 1)
axis(1, 2009:2021,                F, T,                 lwd = 1.5, lend = 1)
axis(1, c(2010, 2015, 2021),      T, T, cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, c(1, lg(1200)),           F, T, tcl = 0,        lwd = 1.5, lend = 1)
axis(2, 0:3, c(1, 10, 100, 1000), T,    cex.axis = 1.2, lwd = 1.5, lend = 1)
axis(2, lg(c(2:9, seq(20, 90, 10), seq(200, 900, 100))),
                                  F, T, tcl = -0.25,    lwd = 1.5, lend = 1)
mtext("Year of Red List assessment", 1, 3, F, 2015.5, cex = 1.8)
x <- c(2010, 2015, 2021, 2023)
ELS.[2, 4] <- 10^(3/4 * lg(ELS.[4, 4]) + 1/4 * lg(ELS.[6, 4]))
ELS.[5, 4] <- 10^(2/4 * lg(ELS.[4, 4]) + 2/4 * lg(ELS.[6, 4]))
ELS.[3, 4] <- 10^(1/4 * lg(ELS.[4, 4]) + 3/4 * lg(ELS.[6, 4]))
for (i in 1:6) {
  lines (x,      lg(ELS.[i, ]), lty = i, lwd = 2.4)
  points(x[1:3], lg(ELS.[i, 1:3]), pch = c(4, 21, 24, 22, 25, 23)[i],
    cex = 1.8, bg = "black", lwd = 2.4, ljoin = 1)
  text(2023, lg(ELS.[i, 4]), 
    c("land-use change", "other/unknown", "climate change",
      "pollution", "native species", "disturbance")[i],
    pos = 4, cex = 1.2)
}
text(2015.5, 3.12, expression(bold(ELS[50])), cex = 1.8)
if (nchar(figS3)) {
  dev.off()
}

