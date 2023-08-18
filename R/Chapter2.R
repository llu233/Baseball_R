W <- c(8, 21, 15, 21, 21, 22, 14)
L <- c(5, 10, 12, 14, 17, 14, 19)
Win.Pct <- 100 * W / (W + L)
Win.Pct
Year <- seq(1946, 1952)
Year
Year <- 1946 : 1952
Age <- Year - 1921
plot(Age, Win.Pct)
mean(Win.Pct)
100 * sum(W) / (sum(W) + sum(L))
sort(W)
cumsum(W)
summary(Win.Pct)
W[c(1, 2, 5)]
W[1 : 4]
W[-c(1, 6)]
Win.Pct > 60
(W > 20) & (Win.Pct > 60)
Win.Pct == max(Win.Pct)
Year[Win.Pct == max(Win.Pct)]
Year[W + L > 30]
NL <- c("FLA", "STL", "HOU", "STL", "COL",
        "PHI", "PHI", "SFG", "STL", "SFG")
AL <- c("NYY", "BOS", "CHW", "DET", "BOS",
        "TBR", "NYY", "TEX", "TEX", "DET")
Winner <- c("NL", "AL", "AL", "NL", "NL",
            "NL", "AL", "NL", "NL", "NL")
N.Games <- c(6, 4, 4, 5, 4, 5, 6, 5, 7, 4)
Year <- 2003 : 2012
results <- matrix(c(NL, AL), 10, 2)
results
dimnames(results)[[1]] <- Year
dimnames(results)[[2]] <- c("NL Team", "AL Team")
results
table(Winner)
barplot(table(Winner))
table(NL)
NL
NL2 <- factor(NL, levels=c("FLA", "PHI", "HOU", "STL", "COL", "SFG"))
str(NL2)
table(NL2)
World.Series <- list(Winner=Winner, Number.Games=N.Games, Seasons="2003 to 2012")
World.Series$Number.Games
World.Series[[2]] # put in vector
World.Series["Number.Games"]$Number.Games.  # put in list
N.Games <- c(6, 4, 4, 5, 4, 5, 6, 5, 7, 4)
Winner <- c("NL", "AL", "AL", "NL", "NL",
            "NL", "AL", "NL", "NL", "NL")
table(Winner)
barplot(table(Winner))
by(N.Games, Winner, summary)

HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29
hr.rates(Age, HR, AB)

HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29
HR.Rates <- hr.rates(Age, HR, AB)
Mantle <- cbind(Age, HR, AB, Rates=HR.Rates$y)

spahn[1 : 3, 1 : 10] # first 5 rows and first 4 var
spahn[1, ]
spahn[1 : 10, c("Age", "W", "L", "ERA")]
spahn$Age[spahn$ERA == min(spahn$ERA)] # Spahn lowest ERA age

spahn$FIP <- with(spahn, (13 * HR + 3 * BB - 2 * SO) / IP) #FIP
spahn1 <- subset(spahn, Tm == "BSN" | Tm == "MLN")
spahn1$Tm <- factor(spahn1$Tm, levels=c("BSN", "MLN"))
by(spahn1[, c("W.L", "ERA", "WHIP", "FIP")], spahn1$Tm, summary)
spahn1$Tm: BSN #ERA higher, FIP lower in Boston, weak defence, unlucky?
NLbatting <- read.csv("NLbatting.csv")
ALbatting <- read.csv("ALbatting.csv")
batting <- rbind(NLbatting, ALbatting)
NLpitching <- read.csv("NLpitching.csv")
NL <- merge(NLbatting, NLpitching, by="Tm")
NL.150 <- subset(NLbatting, HR > 150)
install.packages("Lahman")
library(Lahman)
??Batting
Batting <- read.csv("Batting.csv")

compute.hr <- function(pid){
  d <- subset(Batting.60, playerID == pid)
  sum(d$HR)
}
players <- unique(Batting.60$playerID)
S <- sapply(players, compute.hr)
R <- data.frame(Player=players, HR=S)
R <- R[order(R$HR, decreasing=TRUE), ]
head(R)

library(plyr)
dataframe.AB <- ddply(Batting, .(playerID), summarize, Career.AB=sum(AB, na.rm=TRUE))
Batting <- merge(Batting, dataframe.AB, by="playerID")
Batting.5000 <- subset(Batting, Career.AB >= 5000)
ab.hr.so <- function(d){
  c.AB <- sum(d$AB, na.rm=TRUE)
  c.HR <- sum(d$HR, na.rm=TRUE)
  c.SO <- sum(d$SO, na.rm=TRUE)
  data.frame(AB=c.AB, HR=c.HR, SO=c.SO)
}
aaron <- subset(Batting.5000, playerID == "aaronha01")
ab.hr.so(aaron)
d.5000 <- ddply(Batting.5000, .(playerID), ab.hr.so)
head(d.5000)
with(d.5000, plot(HR/AB, SO/AB))
with(d.5000, lines(lowess(HR/AB, SO/AB)))

?dotchart
??dotchart

###########################################
# practice
#1
SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474)
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)
SB.Attempt <- SB + CS
Success.Rate <- SB / SB.Attempt 
SB.Game <- SB / G 
plot(Success.Rate, SB.Game)

#2
outcomes = c("Single", "Out", "Out", "Single", "Double", "Out", "Walk", "Out", "Single")
table(outcomes)
outcomes
f.outcomes <- factor(outcomes, levels = c("Out", "Walk", "Single", "Double"))
table (f.outcomes)
outcomes == "Walk"
sum(outcomes == "Walk")

#3
W <- c(373, 354, 364, 417, 355, 373, 361, 363, 511)
L <- c(208, 184, 310, 279, 227, 188, 208, 245, 316)
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Spahn", "Young")
Win.PCT <- 100 * W / (W+L)
Wins.350 <- data.frame(Name, W, L, Win.PCT)
order(wins.350)
orderedwins.350 <- Wins.350[order(Wins.350$Win.PCT, decreasing = TRUE), ]
orderedwins.350

#4
SO <- c(2198, 4672, 1806, 3509, 3371, 2502, 1868, 2583, 2803)
BB <- c(951, 1580, 745, 1363, 999, 844, 1268, 1434, 1217)
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Spahn", "Young")
SO.BB.Ratio <- SO / BB
SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio)
subset(SO.BB, SO.BB.Ratio > 2.8)
SO.BB <- SO.BB [order(SO.BB$BB, decreasing=TRUE), ]

#5
Pitching <- read.csv("pitching.csv")
stats <- function(d){
  c.SO <- sum(d$SO, na.rm=TRUE)
  c.BB <- sum(d$BB, na.rm=TRUE)
  c.IPouts <- sum(d$IPouts, na.rm=TRUE)
  c.midYear <- median(d$yearID, na.rm=TRUE)
  data.frame(SO=c.SO, BB=c.BB, IPouts=c.IPouts, midYear=c.midYear)
}
career.pitching <- ddply(Pitching, "playerID", stats)
# or 
# Note the use of the '.' function to allow 
# playerID to be used without quoting
career.pitching <- ddply(Pitching, .(playerID), stats)
merge(career.pitching, Pitching)
career.10000 <- subset(career.pitching, IPouts >= 10000)
with(career.10000, plot(midYear, SO/BB))