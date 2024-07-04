#########################################################
###                   AniMove 2024                    ###
### Script by Kami Safi, Martina Scacco & Anne Scharf ###
#########################################################
###               Trajectory Analysis                 ###
#########################################################

library(move2)
library(move)
library(adehabitatLT)
library(sf)
library(lubridate)
library(circular)
library(ggplot2)
library(rnaturalearth)
library(units)
library(dplyr)
library(purrr)
library(tidyr)
library(scales)

# set wd to the data folder in your computer
setwd("/home/mscacco/ownCloud - mscacco@ab.mpg.de@owncloud.gwdg.de/Animove_2024_prep/prep_scripts/data")

# We will look at simple geometric properties of the tracks and 
# how movement processes are affected by small changes in these properties

parDefaults <- par()

#________________________________________________________________
# VARIANCE OF dBBMM (dynamic Brownian Bridge Movement Model) ####
#________________________________________________________________

# dBB allows to assigns a probability of where the animal could have been when we did not observe it (between two known locations).
# It assumes brownian motion between them and gives the probability of being anywhere between point 1 and 2 at any given time.
# Probability is based on the movement variance of the trajectory: the faster and more direct, the less the variance between the two points
# Variance calculated leaving out the 2nd of three locations and calculating its distance from the straight line connecting location 1 and 3

# we need to reproject the data to metres. Here we can use center=T: the center of the coordinate system is the center of the track
Leroy <- move(system.file("extdata","leroy.csv.gz",package="move"))
Leroy <- spTransform(Leroy, center=T) 

# we choose a window size of odd n. of locs; within a window, one global variance is estimated using all locations and local variances are estimated based on the margin.
# the window slides through the entire trajectory and uses the margin to find breakpoints and estimate variances. Size of the window and margins define the expected scale of change in behaviour, but the calculation is very robust against changes in window and margins.
LeroyVar <- brownian.motion.variance.dyn(Leroy, location.error=25, window.size=71, margin=21)

# This method allows the variance to change along the track, that's why the brownian bridge is "dynamic" 
# so we can use boxplots to show the variance of the movement variance at different times of the day:
VarDat <- data.frame(var=getMotionVariance(LeroyVar), 
                     hour=hour(LeroyVar$study.local.timestamp)) # important to use local time for interpretation
boxplot(VarDat$var~VarDat$hour, xlab="Hour of the day", ylab="mean Brownian variance", pch="*")
# Note to plot:
# - Leroy moves around during the night, and sleeps during the day, it probably avoids people 
# - Low variance in the motion variance means the location I left out is easy to predict (either stationary, or moving with constant speed)

#__________________________________________________________________
# VARIANCE of dBGB (dynamic bi-Gaussian Bridge Movement Model) ####
#__________________________________________________________________

# The variance in dBGB decomposes the variance in two:
# forward-backward (parallel to movement) and side-ways component (perpendicular to movement)
# helps distinguishing stationary from movement (which resulted in similar dBB variance above)

LeroyBGB <- dynBGBvariance(Leroy, locErr=25, windowSize=31, margin=15)
VarDat <- data.frame(var=getMotionVariance(LeroyBGB), hour=hour(LeroyVar$study.local.timestamp))
VarDat$I_d <- ((VarDat$var.para-VarDat$var.orth)/(VarDat$var.para+VarDat$var.orth))
boxplot(I_d~hour, xlab="Hour of the day", ylab="dBGB variance index", data=VarDat, pch="*")
abline(h=0, lty=2)
# Note to plot:
# - zero: true brownian motion, diffusive in all directions as likely to go sideways than front or back.
# (almost nothing in nature is brownian, but error is, if data fall on 0 it could be error e.g. in the den)
# - positive values: directional movement, more front or back than side, less changes in direction and more in velocity
# - negative values: more likely to go sideways, there are more changes in direction and less in velocity



