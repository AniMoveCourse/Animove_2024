load("furseals.rda")
head(furseal)


require(sf)
require(smoove)
require(ggplot2)
require(plyr)
require(mapview)
mapview(furseal[,"Trip"])

# Fur seal data
ggplot(furseal, aes(X, Y, col = Trip)) + geom_sf() + geom_path()
t1 <- subset(furseal, Trip == "trip1")
with(t1, scan_track(x = X, y = Y, time = DateTime))

# if you haven't installed smoove: 
eval <- FALSE
if(eval){
  require(devtools)
  install_github("EliGurarie/smoove", build_vignettes = TRUE)
}

# Read the vignette!
vignette("smoove")

## Fit a single uncorrelated random walk model to the data
fit <- estimateUCVM(t1, T = t1$DateTime,
                   method = "vLike", CI = TRUE, 
                   time.units = "min")
fit

# this result gives a single estimate of means speed - about 50 m/min - 
# and the time scale of autocorrelation - about 88. 

# Compare a bunch of models

fits <- estimateRACVM(t1, T = t1$DateTime, 
                   models = "all", 
                   time.units = "min")

# This shows that - overall - the "unbiased CVM" is the best model - no overall 
# advection or rotation

# For change point analysis: 

## 1st the "sweep"

furseal_Sweep <- with(t1, sweepRACVM(Z = cbind(X,Y), 
                                T = DateTime,
                  windowsize = 12, windowstep = 1, 
                  time.unit = "hours", 
                  model = "UCVM", progress=TRUE), method = "crawl")

plotWindowSweep(furseal_Sweep)

## 2nd - get candidate change points

furSeal.cp <- findCandidateChangePoints(windowsweep = furseal_Sweep, clusterwidth = 4)
abline(v = furSeal.cp)

## 3rd - estimate models between change points

furseal.cptable1 <- getCPtable(furSeal.cp, modelset = c("UCVM", "ACVM"), criterion = "BIC")
furseal.phaselist1 <- estimatePhases(furseal.cptable1)

## 4th - plot & summarize the estimated phases

plotPhaseList(furseal.phaselist1, parameters = c("tau", "eta","mu.x", "mu.y"))
summarizePhases(furseal.phaselist1)


