## ----echo = -1---------------------------------------------------------------------------------

require(sf)
require(plyr)


load(file="elk_regdata.rda")

myelk_utm <- elk_regdata |> subset(id == "YL96") |> 
  st_as_sf(coords = c("lon","lat"), crs = 4326) |>
  st_transform(32611)


## ----------------------------------------------------------------------------------------------
myelk_utm <- cbind(myelk_utm, st_coordinates(myelk_utm)) |> data.frame()
head(myelk_utm)


## ----------------------------------------------------------------------------------------------
library(momentuHMM)


## ----------------------------------------------------------------------------------------------
myelk_hmm_prep <- prepData(myelk_utm |> data.frame(), type = "UTM",
                                    coordNames = c("X","Y")) 

head(myelk_hmm_prep)

## ----------------------------------------------------------------------------------------------
stepMean0 <- c(m1 = 100, m2 = 4000)
stepSD0 <- c(sd1 = 50, sd2 = 1000)
angleCon0 <- c(rho1  = 0.1, rho2 = 0.8)

## ----------------------------------------------------------------------------------------------
stateNames <- c("resident","transit")


## ----------------------------------------------------------------------------------------------
dist <- list(step = "gamma", angle = "wrpcauchy")
Par0 <- list(step=c(stepMean0, stepSD0), angle = c(angleCon0))

## ----------------------------------------------------------------------------------------------
myelk_hmm_fit <- fitHMM(data = myelk_hmm_prep, nbStates = 2, dist = dist, 
                      Par0 = Par0, stateNames = stateNames)

print(myelk_hmm_fit)


## ----------------------------------------------------------------------------------------------
hmm_states <- viterbi(myelk_hmm_fit)
str(hmm_states)


## ----------------------------------------------------------------------------------------------
myelk_utm$state <- hmm_states


## ----DefaultTwoStatePlot, echo  = 2, eval = 1--------------------------------------------------
plot(myelk_hmm_fit)


## ----TwoState_ScanTtrack-----------------------------------------------------------------------
layout(cbind(c(1,1),2:3))
par(bty = "l", mar = c(2,2,2,2))

with(myelk_utm, {
  plot(X, Y, asp =1, col = c("orange","blue")[state], pch = 19, cex = 0.7)
  segments(X[-length(X)], Y[-length(Y)], 
           X[-1], Y[-1], col = c("orange","blue")[state[-length(state)]])
  plot(date, X, col = c("orange","blue")[state], pch = 19, cex = 0.7)
  segments(date[-length(X)], Y[-length(Y)], 
           date[-1], Y[-1], col = c("orange","blue")[state[-length(state)]])
  plot(date, Y, col = c("orange","blue")[state], pch = 19, cex = 0.7)
  segments(date[-length(X)], Y[-length(Y)], 
           date[-1], Y[-1], col = c("orange","blue")[state[-length(state)]])
})


## ----------------------------------------------------------------------------------------------
library(mapview)

## ----------------------------------------------------------------------------------------------
myelk_sf <- myelk_utm |>
  st_as_sf(coords=c("X","Y"), crs= 32611) |>
  st_transform(4326) |>
  mutate(state = as.character(state))

myelk_track <- myelk_sf |>
  dplyr::summarize(do_union=FALSE) |> 
  st_cast("LINESTRING")

mapview(myelk_track, color="darkgrey") +
  mapview(myelk_sf, zcol="state", col.regions=c("orange","blue"))


## ----Priors_ThreeState-------------------------------------------------------------------------
stepMean0 <- c(m1 = 50, m2 = 2000, m3 = 200)
stepSD0 <- c(sd1 = 50, sd2 = 1000, sd3 = 100)
angleCon0 <- c(rho1  = 0.1, rho2 = 0.8, rho3 = 0.2)


## ----------------------------------------------------------------------------------------------
stateNames <- c("resident-slow","transit", "resident-faster")


## ----------------------------------------------------------------------------------------------
dist <- list(step = "gamma", angle = "wrpcauchy")
Par0 <- list(step=c(stepMean0, stepSD0), angle = c(angleCon0))


## ----Fit_ThreeState----------------------------------------------------------------------------
myelk_hmm_threestate <- fitHMM(data = myelk_hmm_prep, nbStates = 3, 
                         dist = dist, Par0 = Par0, 
                         stateNames = stateNames, 
                         formula = ~1)


## ----------------------------------------------------------------------------------------------
myelk_hmm_threestate


## ----------------------------------------------------------------------------------------------
hmm_3states <- viterbi(myelk_hmm_threestate)
myelk_utm$state <- hmm_3states


## ----ThreeStateDefaultPlot, echo = 2, eval = 1-------------------------------------------------
plot(myelk_hmm_threestate, ask = FALSE)
plot(myelk_hmm_threestate)


## scan track 3-state

layout(cbind(c(1,1),2:3))
par(bty = "l", mar = c(2,2,2,2))

cols <- c("orange","blue", "red")

with(myelk_utm, {
    plot(X, Y, asp =1, col = cols[state], pch = 19, cex = 0.7)
    segments(X[-length(X)], Y[-length(Y)], 
             X[-1], Y[-1], col = cols[state[-length(state)]])
    plot(date, X, col = cols[state], pch = 19, cex = 0.7)
    segments(date[-length(X)], Y[-length(Y)], 
             date[-1], Y[-1], col =cols[state[-length(state)]])
    plot(date, Y, col = cols[state], pch = 19, cex = 0.7)
    segments(date[-length(X)], Y[-length(Y)], 
             date[-1], Y[-1], col = cols[state[-length(state)]])
})


with(myelk_utm |> subset(lubridate::month(myelk_utm$date) == 4), {
    plot(X, Y, asp =1, col = cols[state], pch = 19, cex = 0.7)
    segments(X[-length(X)], Y[-length(Y)], 
             X[-1], Y[-1], col = cols[state[-length(state)]])
    plot(date, X, col = cols[state], pch = 19, cex = 0.7)
    segments(date[-length(X)], Y[-length(Y)], 
             date[-1], Y[-1], col =cols[state[-length(state)]])
    plot(date, Y, col = cols[state], pch = 19, cex = 0.7)
    segments(date[-length(X)], Y[-length(Y)], 
             date[-1], Y[-1], col = cols[state[-length(state)]])
})

# How much time did it spend in each state?

table(myelk_utm$state)

# when was it in each state?
require(lubridate)
require(ggplot2)
with(myelk_utm, table(state, hour(date)))  |> barplot(col = cols)
     
