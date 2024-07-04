#____________________
# TRACK ANALYSIS ####
#____________________

# Compare movement metrics between individuals/groups
# Ways to incorporate autocorrelation structure

library(move2)
library(units)
library(dplyr)
library(sf)
library(MASS)

bats <-  movebank_download_study(study_id = 1918503,  #"Parti-colored bat Safi Switzerland",
                                 sensor_type_id = "radio-transmitter")

# Annotate speed, azimuth and turning angle to the trajectory
bats <- mutate(bats, 
               speed = mt_speed(bats,"m/s"), 
               timelag = mt_time_lags(bats,"day"), 
               distance = mt_distance(bats,"m"))

# summarize values per id
names(mt_track_data(bats))
indData <- group_by_track_data(bats, individual_local_identifier, sex) %>% 
           summarize(sex=unique(sex),
                    medianSpeed=median(speed,na.rm=T),
                    timeTracked=sum(timelag,na.rm=T),
                    distanceTracked=sum(distance,na.rm=T)) %>% #cumulative distance tracked
           st_drop_geometry()
head(indData, 4)


### Test for differences in distance traveled per day between sexes
indData <- droplevels(indData)
boxplot(log10(I(distanceTracked/timeTracked)) ~ sex, 
        data=indData, names=c("Males", "Females"),
        ylab=expression(paste(Log_10, " of cumulative distance in m per day", sep="")))
# Note to plot: there may be a small difference
indDataNoUnits <- drop_units(indData)
t.test(log(I((distanceTracked/1000))/timeTracked) ~ sex, data=indDataNoUnits)
# but this difference does not seem to be significant
# with a glm we can add co-variates
mod <- glm(sqrt(distanceTracked) ~ as.factor(sex) + timeTracked, data=indDataNoUnits)
# we check that assumptions are met
par(mfrow=c(2,2))
plot(mod, ask=F)
# and we see that the longer we track them, the higher the cumulative distance (of course)
summary(mod)


### Test for differences in speed between sexes
par(mfrow=c(1,1))
boxplot(medianSpeed ~ sex, data=indDataNoUnits, names=c("Males", "Females"), ylab="Median speed in m/s")
# Note to plot: males seem to travel faster than females.. one obs with interesting negative speed, but we leave it for now
wilcox.test(medianSpeed ~ sex, data=indDataNoUnits)
# this time the difference seems to be significant

# Speeds and distances have naturally very long tails, and to run linear models we often have to transform our data
# Usually log or sqrt, but if we are not sure, boxcox is a nice function to find a right transformation of the data
# If we minimise the bias in the residuals the estimated slope is correct
bc <- boxcox(medianSpeed ~ as.factor(sex) + timeTracked, data=indDataNoUnits[-15,])
modII <- glm(I(medianSpeed^bc$x[which.max(bc$y)]) ~ as.factor(sex)+timeTracked, data=indDataNoUnits[c(-15),])
# plot(modII)
summary(modII)
# there is a difference between sexes in speed
# time tracking is not significant anymore, because is integrated in the speed calculation
# given what we said about the effect of sampling frequency we could check that too
bats$timelag_min <- set_units(bats$timelag, min) 
group_by_track_data(bats, sex) %>% 
  summarize(medianTimelag=median(timelag_min, na.rm=T)) %>% 
  st_drop_geometry(batsPerTrack)


### Model using lme, glmm and gamm
# - can include complex correlation structures
# - can include random factors
# - can include different families (glm and gam)
# - can include smooth terms (gam)
# Interesting link: https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html

library(mgcv)
library(MASS)
library(nlme)
bats <- mt_as_event_attribute(bats, sex)
testDat <- data.frame(v=drop_units(bats$speed),
                      id=mt_track_id(bats),
                      time=mt_time(bats),
                      sex=bats$sex)
testDat <- testDat[which(testDat$v < 20 & testDat$v > 2),]
testDat <- arrange(testDat, id, time)
acf(testDat$v)
cs1 <- corAR1(0.2, form = ~ time|id) # correlation structure, value between -1 and 1
# we start with every subsequent observation being correlated with each other)

# using linear models
mod1 <- nlme::lme(log(v) ~ as.factor(sex), 
                  random=~1|id, #random factor
                  correlation=cs1, #corr structure
                  data=testDat)
summary(mod1)
acf(residuals(mod1))

# using generalized linear models
mod2 <- MASS::glmmPQL(log(v) ~ as.factor(sex), 
                      random=~1|id, #random factor
                      correlation=cs1, #corr structure
                      family=gaussian, #could choose diff family
                      data=testDat, verbose=FALSE)
summary(mod2)
acf(residuals(mod2))

# using generalized additive models
mod3 <- mgcv::gamm(log(v) ~ as.factor(sex), 
                   random=list(id=~1), #random factor
                   correlation=cs1, #corr structure
                   data=testDat, family=gaussian)
summary(mod3$gam)
summary(mod3$lme)
# significant but very low Rsq
acf(residuals(mod3$lme))
par(mfrow=c(2,2))
gam.check(mod3$gam)

# We tried to do the best we could do with only looking at the geometry of the track, 
# We used space and time, and comparisons between sex, all without looking at the environmental context yet, we'll do that later


