
###########################################################################
##### AniMove 2024. Continuous-time movement models #######################
##### Autocorrelated home range estimation (AKDE) #########################
###########################################################################

# Authors: Inês Silva (i.simoes-silva@hzdr.de)
# R version 4.4.1 (2024-06-14)

library(ctmm)
help("akde")

# Load pre-run objects (if needed):
load(here::here("outputs", "akde.RData"))

# Data preparation: -------------------------------------------------------

# Loading tracking datasets:
data("buffalo")
projection(buffalo) <- median(buffalo)
names(buffalo)

pepper <- buffalo$Pepper # or buffalo[[4]]
head(pepper)

# Plotting locations:
plot(pepper, col = "red", lwd = 3, main = "Pepper")

pal <- color(pepper, by = "time")
plot(pepper, col = pal, main = "Pepper")

# This dataset has problems (we will address this later):
dt.plot(pepper)

# Range residency assumption: ---------------------------------------------

level <- 0.95 # we want to display 95% confidence intervals
xlim <- c(0,1 %#% "day") # to create a window of one day

# Checking for the range residency assumption:
SVF <- variogram(pepper)
par(mfrow = c(1,2))
plot(SVF, fraction = 0.5, level = level)
abline(v = 1, col = "red", lty = 2) # adding a line at 1 month
plot(SVF, xlim = xlim, level = level)
par(mfrow = c(1,1))

# Model selection ---------------------------------------------------------
# Selecting the best-fit movement model through model selection:

# Calculate an automated model guesstimate:
GUESS1 <- ctmm.guess(pepper, interactive = FALSE)
summary(GUESS1)

# Automated model selection, starting from GUESS1:
start_time <- Sys.time()
FIT1_ML <- ctmm.select(pepper, GUESS1, 
                       method = "ML", verbose = TRUE)
Sys.time() - start_time # Time difference of 2.435679 mins
summary(FIT1_ML)

plot(SVF, CTMM = FIT1_ML[[1]],
     units = TRUE, fraction = 0.5, level = c(0.95, 0.50), 
     col = "black", col.CTMM = "red")

start_time <- Sys.time()
FIT1_pHREML <- ctmm.select(pepper, GUESS1,
                           method = "pHREML", verbose = TRUE)
## reminder: it will default to pHREML if no method is specified.
Sys.time() - start_time # Time difference of 2.435679 mins
summary(FIT1_pHREML)

plot(SVF, CTMM = FIT1_pHREML[[1]],
     units = TRUE, fraction = 0.5, level = c(0.95, 0.50), 
     col = "black", col.CTMM = "red")

summary(FIT1_ML[[1]]) # best-fit model only
summary(FIT1_pHREML[[1]])

# Home range estimator ----------------------------------------------------
# Feeding a movement model into the home range estimator

# Run an area-corrected AKDE (default):
AKDE1_ML <- akde(pepper, FIT1_ML[[1]], debias = TRUE)
AKDE1_pHREML <- akde(pepper, FIT1_pHREML[[1]], debias = TRUE)

summary(AKDE1_ML, level.UD = 0.95)$CI # 95% home range area
summary(AKDE1_pHREML, level.UD = 0.95)$CI

( 1 - summary(AKDE1_ML)$CI[1,2] / summary(AKDE1_pHREML)$CI[1,2] ) * 100
# ML overestimates by ~6%

m.iid <- ctmm.fit(pepper) # IID
KDE1 <- akde(pepper, m.iid)

# Creating an extent that includes both UDs at the 95% CI level:
newEXT <- extent(list(AKDE1_pHREML, KDE1))

# Plotting KDE and AKDE side-by-side:
par(mfrow = c(1,2))
plot(pepper, UD = KDE1, ext = newEXT)
title(expression("KDEc"))
plot(pepper, UD = AKDE1_pHREML, ext = newEXT)
title(expression("AKDEc"))
par(mfrow = c(1,1))

# Mitigation measures -----------------------------------------------------
# Evaluating additional biases, applying mitigation measures

## Irregular representation in time: --------------------------------------

plot(pepper, lwd = 3)

## Sample sizes:
summary(AKDE1_pHREML)$DOF["area"] # effective sample size of animal1
nrow(pepper) # absolute sample size

# plot all sampling intervals
dt.plot(pepper) # Pepper (buffalo[[4]])
abline(h = 2 %#% "hours", col = "red")

dt.plot(buffalo$Cilla) # Cilla (buffalo[[1]])
summary(pepper)

pal <- "hr" %#% diff(pepper$t)
# minimum adjacent sampling interval
pal <- pmin(c(Inf, pal), c(pal, Inf))
# sampling intervals under 1.5 hours
pal <- (pal < 1.5)
# red (low-frequency) or yellow (high-frequency)
pal <- grDevices::rgb(1, pal, 0)
plot(pepper, col = pal, lwd = 2)

# minimum sampling interval
"minutes" %#% min(diff(pepper$t))

# Calculate wAKDE:
start_time <- Sys.time()
wAKDE1_pHREML <- akde(pepper,
                     CTMM = FIT1_pHREML[[1]],
                     weights = TRUE)
# you only need this with irregular sampling (can be slow!)
# unweighted AKDE places too much density on oversampled times
Sys.time() - start_time
summary(wAKDE1_pHREML)$CI # 95% home range area (weighted)
## Time difference of 1.66443 mins

plot(pepper, UD = wAKDE1_pHREML)

EXT <- extent(list(AKDE1_ML, AKDE1_pHREML, wAKDE1_pHREML), level = 0.95)

# Plotting pHREML (with and without weights) side-by-side:
par(mfrow = c(1,2))
plot(pepper, UD = AKDE1_pHREML, ext = EXT)
title(expression("pHREML AKDE"["C"]))
plot(pepper, UD = wAKDE1_pHREML, ext = EXT)
title(expression("pHREML wAKDE"["C"]))
par(mfrow = c(1,1))

( 1 - summary(AKDE1_pHREML)$CI[1,2] / summary(wAKDE1_pHREML)$CI[1,2] ) * 100
# Unweighted AKDE underestimates by 3%



## Low effective sample sizes: --------------------------------------------

# For a target bias of O(5%)
# ctmm.fit method="ML" requires DOF[area]>=20       (CONVENTIONAL)
# ctmm.fit method="pHREML" requires DOF[area]>=4-5  (DEFAULT)
# ctmm.boot method="pHREML" requires DOF[area]>=2-3 (SLOW)
# but in all cases DOF[area] is an estimate

data("gazelle")
gazelle <- gazelle[[11]]
head(gazelle)
plot(gazelle, col = "blue", lwd = 3)

GUESS2 <- ctmm.guess(gazelle, interactive = FALSE)
FIT2_pHREML <- ctmm.select(gazelle, GUESS2, method = "pHREML")
summary(FIT2_pHREML)
# note the effective sample sizes

# SI units converter
help("%#%")
1 %#% 'hr'

# approximate effective sample size
summary(gazelle)
(8.100378 %#% "month") / (5.881915 %#% "month")
summary(FIT2_pHREML)$DOF[["area"]]

UD2_pHREML <- akde(gazelle, FIT2_pHREML)
summary(UD2_pHREML)

summary(UD2_pHREML)$DOF["area"] # effective sample size
nrow(gazelle) # absolute sample size

# Expected order of pHREML bias:
1/summary(FIT2_pHREML)$DOF["area"]^2

help("ctmm.boot")
start_time <- Sys.time() # start recording running time
BOOT <- ctmm.boot(gazelle, FIT2_pHREML, 
                  error = 0.01, trace = 2, cores = -1)
# save(BOOT, file = here::here("data", "outputs", "bootstrap.RData"))
## note: this function incurs substantial computational cost, may take hours.
( total_time <- Sys.time() - start_time ) # output running time
# Time difference of 43.93944 mins

load(here::here("outputs", "bootstrap.RData"))
summary(BOOT)

# Expected order of bootstrap bias:
1/summary(BOOT)$DOF["area"]^3

UD2_bpHREML <- akde(gazelle, BOOT, weights = TRUE)

summary(UD2_pHREML)$CI
summary(UD2_bpHREML)$CI

( 1 - summary(UD2_pHREML)$CI[1,2] / summary(UD2_bpHREML)$CI[1,2] ) * 100

EXT <- extent(list(UD2_pHREML, UD2_bpHREML), level = 0.95)

# Plotting pHREML and bootstrapped-pHREML side-by-side:
par(mfrow = c(1,2))
plot(gazelle, UD = UD2_pHREML, ext = EXT)
title(expression("pHREML AKDE"["C"]))
plot(gazelle, UD = UD2_bpHREML, ext = EXT)
title(expression("Bootstrapped pHREML wAKDE"["C"]))
par(mfrow = c(1,1))

plot(gazelle, UD = UD2_pHREML, ext = EXT)
plot(gazelle, UD = UD2_bpHREML, ext = EXT)

# save(FIT1_ML,
#      FIT1_pHREML,
#      wAKDE1_pHREML,
#      FIT2_ML,
#      FIT2_pHREML,
#      BOOT,
#      file = here::here("outputs", "akde.RData"))
