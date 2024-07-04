
###########################################################################
##### AniMove 2024. Continuous-time movement models #######################
##### Occurrence distribution #############################################
###########################################################################

# Authors: Inês Silva & Chris Fleming
# (i.simoes-silva@hzdr.de)
# R version 4.4.1 (2024-06-14)

library(ctmm)

# Data preparation: -------------------------------------------------------

# Loading tracking datasets:
data(buffalo)
cilla <- buffalo[[1]]
plot(cilla, lwd = 3)

# Check out variogram:
cilla_svf <- variogram(cilla)
plot(cilla_svf)

# Guess model fit:
cilla_guess <- ctmm.guess(cilla, variogram = cilla_svf, 
                          interactive = FALSE)

# Fit and select best model:
start_time <- Sys.time()
cilla_fit <- ctmm.select(cilla, cilla_guess) # takes a few minutes
Sys.time() - start_time
summary(cilla_fit)
# save(cilla_fit, file = here::here("outputs", "fit_cilla.RData"))
load(here::here("outputs", "fit_cilla.RData"))
cilla_fit <- akdeList[[1]]
summary(cilla_fit) 

# Q: What is the occurrence distribution?
# A: Given a random time *in the sampling period*, where was the animal

# Q: What is the range distribution?
# A: At some time in the future/past *under the same behaviors* where will the animal be
# A: Long-term space use *for continuing behaviors*

# Create occurrence distribution (OD)
cilla_od <- occurrence(cilla, cilla_fit, level.UD = .95)
plot(cilla_od)

# Plotting OD with location data
plot(cilla, cilla_od, col.grid = NA, col.level = NA)

cilla_sim <- simulate(cilla, cilla_fit, dt = 5 %#% 'min')
plot(cilla_sim, cilla_od, col = "black", lwd = .6, col.DF = "#009da0",
     col.grid = NA, col.level = NA)

# For comparison, fitting a Brownian Motion model: ------------------------

bm <- ctmm(tau = Inf, range = FALSE)
bm$sigma <- ctmm:::scale.covm(cilla_fit$sigma,
                              1/cilla_fit$tau[1]) # match diffusion rate
cilla_fit_bm <- ctmm.fit(cilla, CTMM = bm)
summary(cilla_fit_bm)

cilla_BBMM <- occurrence(cilla, cilla_fit_bm)
plot(cilla, cilla_BBMM, col = "black", lwd = 1, 
     col.grid = NA, col.level = NA)

cilla_sim_bm <- predict(cilla, cilla_fit_bm,
                         t = seq(1, 1 %#% "year", by = 1 %#% "day"))
plot(cilla_sim_bm, col = "red", lwd = 10)
plot(cilla, col = "blue", lwd = 10, add = TRUE)

EXT <- extent(list(cilla_od, cilla_BBMM))
par(mfrow = c(1,2))
plot(cilla_od, ext = EXT)
title("Kriging")
plot(cilla_BBMM, ext = EXT)
title("BBMM")
par(mfrow = c(1,1))

# Range versus occurrence: ------------------------------------------------

# Make an AKDE
cilla_hr <- akde(cilla, cilla_fit)

# comparison plot with AKDE
EXT <- extent(list(cilla_hr,cilla_od))
par(mfrow = c(1,2))
plot(cilla, cilla_hr, ext = EXT, col.grid = NA)
title("Range UD")
plot(cilla_od, ext = EXT, col.DF = "green", col.level = NA)
title("Occurrence UD")
par(mfrow = c(1,1))

SUB <- cilla

# Manipulation 1 - coarsen the data:
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]
cilla_hr.SUB <- akde(SUB, cilla_fit)
cilla_od.SUB <- occurrence(SUB, cilla_fit)
par(mfrow = c(1,2))
# plot(cilla_hr, ext = EXT, col.grid = NA, col.level = NA)
# title("Range UD")
# plot(cilla_od, ext = EXT, col.level = NA)
# title("Occurrence UD")
plot(cilla_hr.SUB, ext = EXT, col.grid = NA, col.level = NA)
title("Range UD (Subset)")
plot(cilla_od.SUB, ext = EXT, col.level = NA)
title("Occurrence UD (Subset)")
#' Rerun this block to coarsen the data further.

# Manipulation 2 - truncate the data:
SUB2 <- cilla
SUB2 <- SUB2[1:(nrow(SUB2)/2),]

cilla_hr.SUB2 <- akde(SUB2, cilla_fit)
cilla_od.SUB2 <- occurrence(SUB2, cilla_fit)
par(mfrow = c(2,2))
plot(cilla_hr, ext = EXT, col.grid = NA, col.level = NA)
title("Range UD")
plot(cilla_od, ext = EXT, col.level = NA)
title("Occurrence UD")
plot(cilla_hr.SUB2,ext = EXT,col.grid = NA,col.level = NA)
title("Range UD (Subset 2)")
plot(cilla_od.SUB2,ext = EXT,col.level = NA)
title("Occurrence UD (Subset 2)")
par(mfrow = c(1,1))
#' Rerun this block to truncate data further.

