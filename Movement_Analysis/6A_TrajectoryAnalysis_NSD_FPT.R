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


#________________________________
# AUTO-CORRELATION STRUCTURE ####
#________________________________

# Trajectories, empirical or simulated, are often generated based on a correlation of their geometric properties.
# In almost all natural movements we can find some degree of persistence in maintaining the direction and often also speed. 
# (in fact correlated random walks, look more "natural" than the Brownian random walks 
# The correlation structure of a trajectory can not only inform about the underlying movement processes, but also be used to investigate changes in these processes, 
# which can be used to find breaks in the behaviour of the animal from the movement, defined as segmentation
# Autocorrelation is calculated as the correlation of values in a vector with a certain lag


### load data from Leroy the fisher
fishers <- mt_read(mt_example())
Leroy <- filter_track_data(fishers, .track_id = "M1")
Leroy <- Leroy[! sf::st_is_empty(Leroy),]
speed_leroy <- drop_units(mt_speed(Leroy, "m/s"))
speed_leroy <- speed_leroy[!is.na(speed_leroy)]

### Correlation between any 2 subsequent speeds (autocorrelation of lag 1)
cor(speed_leroy[-length(speed_leroy)], speed_leroy[-1])

### Correlation of all speeds separated by one segment (autocorrelation of lag 2)
lag <- 2
cor(speed_leroy[seq(1, length(speed_leroy)-lag, 1)],
    speed_leroy[seq(1+lag, length(speed_leroy), 1)])
# autocorrelation decreases with increasing lag, let's see for 100

### Autocorrelation of speeds between lag 1 to lag 100
r <- NULL
p <- NULL
for(lag in 1:100){
  r <- c(r, cor.test(speed_leroy[seq(1, length(speed_leroy)-lag, 1)],
                     speed_leroy[seq(1+lag, length(speed_leroy), 1)], method="spearman")$estimate)
  p <- c(p, cor.test(speed_leroy[seq(1, length(speed_leroy)-lag, 1)],
                     speed_leroy[seq(1+lag, length(speed_leroy), 1)], method="spearman")$p.value)
}
plot(r, type="n", xlab="Lag", ylab="Correlation coefficient")
points(r[p<=0.05]~seq(1:100)[p<=0.05], pch=16, col="grey40")
points(r, type="b")
points(40,0.5, pch=16, col="grey40")
points(40,0.5)
text(41,0.49, expression(p<=0.05), pos=4)
# Note to plot:
# - significant autocorrelation up to lag ~7-10, which in leroy corresponds to ~6h
# - oscillation correspond to periodicity in activity, Leroy is active for a while, and than inactive for a while

# different plot, same information
acf(speed_leroy)

### Autocorrelation of azimuth and turning angle
azimuth <- as.circular(drop_units(mt_azimuth(Leroy)), type="direction",
                   units="radians", template="geographic",
                   zero="0", rotation="clock", modulo="asis")
turnAngle <- as.circular(drop_units(mt_turnangle(Leroy)), type="angle",
                     units="radians", template="geographic",
                     zero="0", rotation="clock", modulo="asis")
par(mfcol=c(1,2))
acf(azimuth, na.action = na.omit, main="Autocorrelation in azimuth")
acf(turnAngle, na.action = na.omit, main="Autocorrelation in turning angle")
# Note to plot:
# - there is little autocorrelation, Leroy moves randomly in all directions

#__________________________________________________
## CORRELATION IN DIRECTION & MOVEMENT PROCESS ####
#__________________________________________________
### Simulate 2 tracks with 1000 steps 
### and different levels of correlation of azimuth (0.2 and 0.99)
set.seed(1512)
library(adehabitatLT)
crw1 <- as(simm.crw(date=1:1000, r=0.2), "Move") 
crw2 <- as(simm.crw(date=1:1000, r=0.99), "Move")
par(mfcol=c(1,2))
plot(crw1)
plot(crw2)

# We saw from simulated trajectories how the correlation in direction directly affects 
# how long an animal takes to leave a certain area (its movement process/diffusive process from an area).
# To directly quantify this same parameter from an empirical trajectory we can use:
# - "reference" point -> NSD Net Squared Displacement
# - NO "reference" point -> FPT First Passage time

#___________________________________
# NET SQUARE DISPLACEMENT (NSD) ####
#___________________________________

# NSD = distance of each point in the trajectory from a given reference point (nest, colony..)
# NSD ~ time = how fast an animal moves away from a point of reference
# In this case the point of reference is the first point of the track, that is Leo's nest

# restore plotting device
dev.off()
par(parDefaults)

# load and plot Leo the vulture with associated seasons 
# (we saved it yesterday in the data folder)
Leo <- readRDS("Leo-65545_seasons.rds")

# calculate and plot NSD
LeoNSD <- (st_distance(Leo, Leo[1,]))^2
LeoNSD <- units::set_units(LeoNSD, "km^2")
layout(matrix(c(1,1,2,3), ncol=2, byrow=T))
plot(mt_time(Leo), LeoNSD, type="l",
     xlab="Time", ylab="Net square distance (Km²)", main="All data")

leoBreed08 <- Leo[which(Leo$season=="Breeding" & year(mt_time(Leo))==2008),]
leoBreed08NSD <- (st_distance(leoBreed08,leoBreed08[1,]))^2
units(leoBreed08NSD) <- units::make_units(km^2)

plot(mt_time(leoBreed08),leoBreed08NSD, type="l",
     xlab="Time", ylab="Net square distance (Km²)", main="Breeding 2008")

leoWinter <- Leo[which(Leo$season=="Wintering"),]
leoWinter <- leoWinter[c(which(year(mt_time(leoWinter))==2008 & month(mt_time(leoWinter)) %in% c(11,12)),
                         which(year(mt_time(leoWinter))==2009 & month(mt_time(leoWinter)) %in% c(1,2,3))),]
leoWinterNSD <- (st_distance(leoWinter,leoWinter[1,]))^2
units(leoWinterNSD) <- units::make_units(km^2)
plot(mt_time(leoWinter),leoWinterNSD, type="l",
     xlab="Time", ylab="Net square distance (Km²)",main="Winter 2008/2009")
layout(matrix(c(1), ncol=1, byrow=T))
# Note to plot:
# - "All data": shows nicely migration and winter/summer range, high nest fidelity and winter site fidelity
# - "Breeding": Leo moves very locally (it's squared, so < 20 km from nest)
# - "Winter08/09": shows that data is not classified correctly, data should be cut off at both ends


#__________________________________________________
# FIRST PASSAGE TIME (FPT) AND VARIANCE IN FPT ####
#__________________________________________________

# If you don't have a point of reference, every location of your trajectory can represent a "reference point"
# FPT calculates how long it takes to leave a circle with a radius r, centered on each location
# what is the mean FPT and the variation of FPT along the track?

library(adehabitatLT)
# functions in the adehabitatLT family assume planar coordinate systems where distances and angles are preserved
# therefore it cannot deal with spherical (geographic) coordinate system and we need to reproject
# the best is to use a local projection, centered in the middle of the track, to minimize the effects of spherical distortion on distances. 
# but this is a problem in large scale datasets, long distance migration in this case
# here we take the start and end point of its migration for a 2points equidistant projection
# you will learn a much better/automated way to do this on Friday!
plot(mt_segments(Leo), col="red")
st_bbox(Leo) #get xy min and xy max to paste below.. Pay attention to the order looking at the plot!!
AEQ_2p <- "+proj=tpeqd +lon_1=-108.82617 +lat_1=53.82783 +lon_2=-68.58033 +lat_2=7.16283 +datum=WGS84"
Leoprj <- st_transform(Leo, crs=AEQ_2p)

# create "as.ltraj" class object
Leo_ltraj <- adehabitatLT::as.ltraj(xy=st_coordinates(Leoprj), 
                                    date=mt_time(Leoprj), 
                                    id=mt_track_id(Leoprj), typeII=T)
# calculate FPT, for different radii, in this case between 1 km and 1000 km in 150 exponential steps (samples more smaller radii than bigger)
fptLeo <- fpt(Leo_ltraj, radii=10^seq(3, 6, length.out=150), units="days")
# fpt object: n. columns = n.radii, n.rows = n.locations, values = fpt in days
str(fptLeo)
dim(fptLeo[[1]])


### Mean nb of days to leave each radii
meanFPT <- colMeans(fptLeo[[1]], na.rm=T)
radiiFPT <- attributes(fptLeo)$radii
plot(meanFPT ~ radiiFPT,
     type="l", lwd=2, xlab="Radii in meters",
     ylab="First passage time in days", log="xy")
# Note to plot:
# - with increasing radii size, on average it takes the animal longer to leave the circle
# - but the slope of this increase is what is interesting, as it suggests changes in the movement process

### Variance of the log(FPT)
vars <- varlogfpt(fptLeo, graph=F)
plot(as.numeric(vars)~radiiFPT,
     type="l", lwd=1, lty=2,
     log="x", ylab="Variance of log first passage time",
     xlab="Radius in meters")
# Note to plot:
# - minima and maxima can indicate change in the movement process


### fitting LM to min/max peaks of variance of log(fpt)
plot(log10(meanFPT)~log10(radiiFPT),
     type="l", lwd=2, xlab="Log radii in meters",
     ylab="Log first passage time in days")
# fit a model to the largest valley, and largest peak of variance
lm1 <- lm(log10(meanFPT[1:which.min(vars[1:which.max(vars)])])~
            log10(radiiFPT[1:which.min(vars[1:which.max(vars)])]))
lm2 <- lm(log10(meanFPT[which.min(vars[1:which.max(vars)]):which.max(vars)])~
            log10(radiiFPT[which.min(vars[1:which.max(vars)]):which.max(vars)]))
abline(lm1, lty=2)
abline(lm2, lty=3)
text(4, 0.1, paste(signif(summary(lm1)$coefficients[2,1], 2),
                   "±",
                   signif(summary(lm1)$coefficients[2,2], 2)), pos=4, cex=0.75)
text(4, 1, paste(signif(summary(lm2)$coefficients[2,1], 2),
                 "±",
                 signif(summary(lm2)$coefficients[2,2], 2)), pos=4, cex=0.75)
# Note to plot:
# - flat slope (< 2): directional movement
# - steep slope (around 2): brownian movement


### breaks in the trend of the variance of log(fpt)
plot(as.numeric(vars)~radiiFPT,
     type="l", lwd=1, lty=2,
     ylab="Variance of log first passage time",
     xlab="Radius in meters", log="x")
breaks1 <- which(diff(floor(diff(as.numeric(vars))))==1)+1
breaks2 <- which(diff(floor(diff(as.numeric(vars))))==-1)+1
abline(v=radiiFPT[breaks1[2]], col="blue")
abline(v=radiiFPT[breaks2[3:5]], col="red")
# Note to plot:
# - besides the local minimum and the global maximum, there is a post maximum hump in the variance


### fitting LM to all segments identified by the breaks (changes in slope of variance of log(fpt))
plot(log10(meanFPT)~log10(radiiFPT),
     type="n", lwd=4, xlab="Log radii in meters",
     ylab="Log first passage time in days")
breaks <- c(breaks1[1], breaks2) # breaks at: 22 65 93 96
lm1 <- lm(log10(meanFPT[1:breaks[1]])~log10(radiiFPT[1:breaks[1]]))
lm2 <- lm(log10(meanFPT[breaks[1]:breaks[2]])~log10(radiiFPT[breaks[1]:breaks[2]]))
lm3 <- lm(log10(meanFPT[breaks[2]:breaks[3]])~log10(radiiFPT[breaks[2]:breaks[3]]))
lm4 <- lm(log10(meanFPT[breaks[3]:breaks[4]])~log10(radiiFPT[breaks[3]:breaks[4]]))
lm5 <- lm(log10(meanFPT[breaks[4]:length(as.numeric(vars))])~log10(radiiFPT[breaks[4]:length(as.numeric(vars))]))

abline(lm1, lty=2, lwd=1 + summary(lm1)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm2, lty=2, lwd=1 + summary(lm2)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm3, lty=2, lwd=1 + summary(lm3)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm4, lty=2, lwd=1 + summary(lm4)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm5, lty=2, lwd=1 + summary(lm5)$coefficient[2,1], col=alpha("black", 0.8))

lines(log10(meanFPT)~log10(radiiFPT),type="l", lwd=4, col=alpha("grey40", 0.8))
legend("bottomright",title="Radii (m)", lty=c(2,2,2,2,2),
       lwd=signif(c(1+summary(lm1)$coefficient[2,1],
                    1+summary(lm2)$coefficient[2,1],
                    1+summary(lm3)$coefficient[2,1],
                    1+summary(lm4)$coefficient[2,1],
                    1+summary(lm5)$coefficient[2,1]),2),
       c(paste(c(1000, round(radiiFPT[breaks],0))[1:2], collapse=" - "),
         paste(c(1000, round(radiiFPT[breaks],0))[2:3], collapse=" - "),
         paste(c(1000, round(radiiFPT[breaks],0))[3:4], collapse=" - "),
         paste(c(1000, round(radiiFPT[breaks],0))[4:5], collapse=" - "),
         paste(c(round(radiiFPT[breaks],0)[4], 100000), collapse=" - ")),
       bty="n", cex=0.75)
points(log10(meanFPT)[breaks]~log10(radiiFPT)[breaks], col=c("blue","red","red","red"), pch=19)
# Note to plot:
# - the different radii represent the different scales at which Leo is operating


### We can better understand these scales by looking at FPT in relation to time
par(mfrow=c(2,2))
breaks <- c(22, 65, 93, 96)
for(i in c(1,2,3)){
  plot(fptLeo[[1]][,breaks[i]]~ mt_time(Leo), type="n",
       xlab="Time", ylab="FPT (days)",
       main=paste("Radius ", round(radiiFPT[breaks[i]],0), "meters"),
       bty="n")
  points(fptLeo[[1]][,breaks[i]]~ mt_time(Leo), pch=16, col=alpha("grey", 0.1))
  lines(fptLeo[[1]][,breaks[i]]~ mt_time(Leo))
}
par(mfrow=c(1,1))
# Note to plot:
# - 1st: ~3Km seems to be Leo's day range size, it never takes her more than 5-10 days to leave a 3Km circle.
# - 2nd: ~20Km is probably about the home range. The longest amount of days could represent breeding (up to 100 days), the shortest spikes wintering, when Leo is a bit more mobile (up to 25 days).
# # - 3rd: ~70Km seems to be highlighting the migration times, when Leo migrates it moves in steps of about 80 km every couple of days before stopping.
# - The points at the bottom of plot 1,2, and 3 (shortest fpt) should represent migration.


