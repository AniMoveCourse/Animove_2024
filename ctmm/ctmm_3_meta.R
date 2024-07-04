
###########################################################################
##### AniMove 2024. Continuous-time movement models #######################
##### Meta-analyses of population-level mean parameters ###################
###########################################################################

# Authors: Inês Silva & Chris Fleming
# (i.simoes-silva@hzdr.de)
# R version 4.4.1 (2024-06-14)

library(ctmm)
help("meta")

# Load the buffalo tracking dataset:
data("buffalo")

# Load pre-run objects (if needed):
load(here::here("outputs", "meta.RData"))

# Fit movement models:
start_time <- Sys.time()
fitList <- list()
for(i in 1:length(buffalo)) {
  guess <- ctmm.guess(buffalo[[i]], interactive = FALSE)
  fitList[[i]] <- ctmm.select(buffalo[[i]], guess, trace = 2)
}
names(fitList) <- names(buffalo)
Sys.time() - start_time
## Time difference of 14.96572 mins

# calculate AKDEs on a consistent grid:
AKDES <- akde(buffalo, fitList, trace = 2)
# save(AKDES, file = here::here("outputs", "akde_buffalos.RData"))
load(here::here("outputs", "akde_buffalos.RData"))

pal <- color(AKDES, by = "individual")
# set color to be spatially distinct (by individual)

# Plot AKDEs:
plot(AKDES,
     col.DF = pal,
     col.level = pal,
     col.grid = NA,
     level = NA)

# Mean buffalo home range "the old way":
AREA <- vector("numeric", length = length(AKDES))
for(i in 1:length(AKDES))
{ AREA[i] <- summary(AKDES[[i]])$CI[2] }
AREA
mean(AREA) # mean
sqrt(var(AREA)/length(AREA)) # SE

# Meta-analysis of buffalo home-range areas:
## What is the mean home range area of an average individual:

meta(AKDES,
     col = c(pal,"black"), 
     verbose = TRUE, # verbose output with CIs
     sort = TRUE) 
## model selection: Dirac-delta > inverse-Gaussian

# Compare groups:
meta(list(south = AKDES[1:3],
          north = AKDES[4:6]),
     plot = TRUE, 
     verbose = TRUE)

# save(fitList = fitList,
#      AKDES = AKDES,
#      file = here::here("outputs", "meta.RData"))


# Population density: -----------------------------------------------------

# this is a straight mean of the individual densities that doesn't model population variance
help("mean.UD")
# note the 'sample' argument for correct CIs

# straight mean - for a population of 6 buffalo
MEAN <- mean(AKDES,sample=FALSE)

plot(buffalo,MEAN,col=COL,main="Mean African buffalo AKDE")

# this is a population kernel density estimate (paper coming)
help("pkde")

PKDE <- pkde(buffalo,AKDES)

plot(buffalo,PKDE,col=COL,main="African buffalo PKDE")


