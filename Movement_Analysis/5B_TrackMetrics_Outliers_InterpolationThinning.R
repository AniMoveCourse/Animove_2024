#########################################################
###                  AniMove 2024                     ###    
### Script by Kami Safi, Martina Scacco & Anne Scharf ###
#########################################################
###         Track  metrics, outliers, thinning        ###
#########################################################

library(move2)
library(dplyr)
library(lubridate)
library(units)
library(classInt)
library(ggplot2) 
library(sf)
library(mapview)

## set wd to the data folder in your computer
setwd("/home/mscacco/ownCloud - mscacco@ab.mpg.de@owncloud.gwdg.de/Animove_2024_prep/prep_scripts/data")
setwd("/home/ascharf/ownCloud/Animove_2024_prep/prep_scripts/data")

#_______________________
# DATA EXPLORATION ####
#_______________________

# We will now look at a series of very useful functions in move2 that 
# allow us to calculate the movement metrics we talked about earlier.
# We will look into:
# ?mt_time_lags()
# ?mt_distance()
# ?mt_speed()
# ?mt_azimuth()
# ?mt_turnangle()
# And explore the relationship between these properties.


# Load four move2 objects for our examples
bats <- mt_read("Parti-colored bat Safi Switzerland.csv") 
buff <- mt_read("Kruger African Buffalo, GPS tracking, South Africa.csv.gz")
Leo <- mt_read("Leo-65545.csv")
fishers <- mt_read(mt_example())


#_______________________________________________
# TEMPORAL ORGANIZATION OF THE TRAJECTORIES ####
#_______________________________________________

### Number of locations
nrow(bats)

### Number of tracks
mt_n_tracks(bats)

### Number of locations per track
table(mt_track_id(bats))

#__________________________________
### Time lag between locations ####

timeLags <- mt_time_lags(bats)
head(timeLags)
#by default it uses the most convenient unit but we can change it or set it from the beginning
timeLags <- units::set_units(timeLags, hours)
timeLags <- mt_time_lags(bats, units = "hour")
head(timeLags)

### Timelag, distance, speed, and direction are properties of the segment (2 locations, not one)
# therefore the number of valid values will be nrow() - 1 for each track, here we have 17 tracks and therefore 17 NAs
mt_n_tracks(bats)
table(is.na(timeLags))
tail(timeLags)

### Distribution of time lags
hist(timeLags, breaks=50, main=NA, xlab="Time lag") 
arrows(24.5,587.5,20.7,189.7, length=0.1)
arrows(49.5,587.5,45.7,189.7, length=0.1)

### Distribution of timelags shorter than 1h
tl_short <- timeLags[timeLags < set_units(1, "hour")]
hist(tl_short, breaks=50, main=NA, xlab="Time lag")

#___________________________________
### Number of locations in time ####

# N. of locations in a certain time window (first transform time to local time zone of the study for better interpretation)
ts <- mt_time(bats)
head(ts)
tsLocal <- lubridate::with_tz(ts, tzone="Europe/Zurich") #OlsonNames()
head(tsLocal)
# N. of location per hour (geometry gets inherited)
bats %>% group_by(hour(tsLocal)) %>% 
  summarize(n())
# N. of locations per month and hour, rename columns and drop geometry
bats %>% group_by(Month = month(tsLocal), Hour = hour(tsLocal)) %>% 
  summarize(N.locations = n()) %>% 
  sf::st_drop_geometry()

#______________________________________________
# SPATIAL ORGANIZATION OF THE TRAJECTORIES ####
#______________________________________________

#______________________________________________
### Distance/step length between locations ####

dist <- mt_distance(bats, units = "m") # if units is not set, it will use the most convenient
summary(dist)
hist(dist)

#_______________________________
### Speed between locations ####

speeds <- mt_speed(bats, units = "m/s") # if units is not set, it will use the most convenient
summary(speeds)
hist(drop_units(speeds), breaks="FD")

### Have a look at the realistic speeds (e.g. < 20m/s)
speedsRealistic <- speeds[speeds < set_units(20, m/s)] # remember to set the unit before the operation
hist(drop_units(speedsRealistic), xlab = "Speed [m/s]", breaks = "FD") 
# this is the common shape for speeds: animals mostly move slowly, sometimes they move fast.

### Speed vs timelags
speedVsTimeLag <- data.frame(timeLag = timeLags, speeds = speeds)
speedVsTimeLag <- speedVsTimeLag[speedVsTimeLag$timeLag < set_units(10, hour) & speedVsTimeLag$speeds < set_units(20, m/s),]
plot(speedVsTimeLag$timeLag, speedVsTimeLag$speeds, xlab='Time lag', ylab='Speed', pch=19) # when "units" are recognised, they are automatically added to the plot
# with longer timelags the speeds is lower... you will learn a lot more about this in the following days.

### Plot highlighting segments with high and low speeds
# select bat 191
unique(mt_track_id(bats))
bat191 <- filter_track_data(bats, .track_id = "191")
# store speed
v <- set_units(mt_speed(bat191), m/s)
# find 9 colors for 10 breaks in the speed vector, the myPal vector has the same length as the number of segments
myBreaks <- classIntervals(v[!is.na(v)], n=10, style="equal")$brks
#myPal <- as.character(cut(v, breaks = myBreaks, labels = grey.colors(10)))
ggplot() +
  #geom_sf(data = mt_segments(bat191), linewidth = 1.5, color = myPal) +
  #two alternative ways to assign colors, either with myPal and the line above, or with the line below and scale_color_gradient2
  geom_sf(data = mt_segments(bat191), aes(color = drop_units(v)), linewidth = 1.5) +
  xlab("Longitude") + ylab("Latitude") +
  scale_color_gradient2(low = "black", mid = "#434343", high = "white", 
                        midpoint = drop_units(median(v, na.rm=T)),
                        breaks = drop_units(myBreaks),
                        name = "Speed [m/s]") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # get rid of panel grids
        panel.background = element_rect(fill = '#434343')) # Change panel background

#_____________________________________________________________
### Direction of movement / azimuth / heading of movement ####

# NOTE: the words heading or bearing are mostly referred to the direction of body axis
# When analysing tracking data we are not observing the animal but only its movement, so we can only now the direction of movement (not body orientation).
# BUT: many devices record "heading" which is the orientation of the tag (not the direction of movement). SO if the tag does not shift its position it is an indication of body orientation.
direction <- mt_azimuth(bats) # Angles in radians relative to the N
head(direction)
summary(direction)
hist(direction,  breaks = 18, xlab="Direction of movement", main = NA)
# they seem to go in all directions

#______________________
### Turning angles ####

### Turning angles are a property of 2 segments (3 locations)
turnAngles <- mt_turnangle(bats) 
# angles in radians relative to the previous step, with 2 NAs per track (first and last value of each track)
turnAngles[bats$`individual-local-identifier` == 191]
summary(turnAngles)

hist(turnAngles, breaks = 18, xlab="Turning Angle", main = NA)
# this shape can indicate movement along a linear structure

# Let's look at turning angle in a different animal, we use buffaloes
turnAnglesBuf <- mt_turnangle(buff)
hist(turnAnglesBuf)
# this is the common shape for turning angles: mostly moving straight, sometimes turning.


#__________________________________________________
# CASE STUDY: AZIMUTH, TURNING ANGLE and SPEED ####
#__________________________________________________

### For this case study we use Leo the Turkey vulture

# Plot Leo
ggplot() + theme_bw() +
  geom_sf(data = rnaturalearth::ne_coastline(returnclass = "sf", 50)) +
  geom_sf(data = mt_track_lines(Leo), color="firebrick")+
  coord_sf(xlim=c(min(sf::st_coordinates(Leo)[,1]),max(sf::st_coordinates(Leo)[,1])), ylim=c(min(sf::st_coordinates(Leo)[,2]),max(sf::st_coordinates(Leo)[,2])))

# get locations per year and month
table(year(mt_time(Leo)), month(mt_time(Leo)))

# removing years 2012-13 because there is a large gap
Leo <- Leo[year(mt_time(Leo)) < 2012,]

# Annotate speed, azimuth and turning angle to the trajectory.
Leo <- Leo %>% mutate(azimuth = mt_azimuth(Leo), 
                      speed = mt_speed(Leo), 
                      turnangle = mt_turnangle(Leo))

###________________________________________________
### Azimuth distribution with speed and season ####

### Categorize seasons. Numbers refer to month.
# assign the categories to a new variable based on the timestamp
# assign season = NA to segments where subsequent locations fall in different seasons
Leo  <- mutate(Leo,
               month = month(mt_time(Leo)),
               season = recode_factor(month,
                                      "1"="Wintering", "2"="Wintering",
                                      "3"="Wintering", "4"="North migration",
                                      "5"="North migration", "6"="Breeding",
                                      "7"="Breeding", "8"="Breeding",
                                      "9"="South migration", "10"="South migration",
                                      "11"="Wintering", "12"="Wintering"),
               season = if_else(season == lead(season, 1), #lead looks at the next location, with lag 1
                                season, NA)
)

saveRDS(Leo, "Leo-65545_seasons.rds")

### Look at direction of movement when movement is happening, that is:
# select segments above 2 m/s, we are only interested in segments when Leo is moving, and not the stationary error
# remove missing values in season and group data by season
Leo_moving <- filter(Leo, speed > set_units(2, "m/s") & !is.na(season)) %>% group_by(season)

### Speed ~ azimuth scatter plot
# speed here is ground speed (how fast is the animal in making way)
ggplot(Leo_moving, aes(x = azimuth, y = speed)) +
  geom_point(color=alpha("black", 0.3)) +
  scale_x_units(unit = "degrees", breaks = c(-2:2) * 90, expand = c(0L, 0L)) +
  scale_y_units(unit = "m/s") +
  theme_linedraw()

# same per season
ggplot(Leo_moving, aes(x = azimuth, y = speed)) +
geom_point(color=alpha("black", 0.3)) +
  scale_x_units(unit = "degrees", breaks = c(-2:2) * 90, expand = c(0L, 0L)) +
  scale_y_units(unit = "m/s") +
  facet_wrap(~season) +
  theme_linedraw()
# Note to plot:
# -high speeds predominantly occur when azimuths are around roughly -180-180∘ and 0∘ (N and S), which are the result of Leo’s migratory behaviour.

### "Wind rose"-type plot, showing azimuth AND speed, again per season
Leo_gg <- mutate(Leo_moving, speed_categorical = cut(set_units(speed, m/s), 
                                                     breaks = c(2, 5, 10, 15, 35)))
Leo_gg <- filter(Leo_gg, !is.na(season))

ggplot(Leo_gg) +
  coord_polar(start = pi) +
  geom_histogram(
    aes(x = set_units(azimuth, "degrees"),fill = speed_categorical),
    breaks = set_units(seq(-180L, 180L, by = 10L), "degrees"),
    position = position_stack(reverse = TRUE)) +
  scale_x_units(
    name = NULL,
    limits = set_units(c(-180L, 180L), "degrees"),
    breaks = (-2L:1L) * 90L,
    labels = c("S", "W", "N", "E")) +
  scale_y_continuous(name = NULL, limits = c(-1, 120), expand = c(0L, 0L)) +
  facet_wrap(~season) +
  scale_fill_ordinal("Speed [m/s]") +
  theme_linedraw()
# Note to plot:
# - migration has directionality with high speeds
# - breeding has only low speeds
# - wintering seems to contain some migration. But we just took "month": to categorize.


###______________________________________________________
### Turning angle distribution with speed and season ####

### Speed ~ turning angle scatter plot, per season
ggplot(filter(Leo_moving,!is.na(season)),aes(x = turnangle, y = speed)) +
  geom_point(color=alpha("black", 0.3))+
  scale_x_units("Turning angle", unit = "degrees", breaks = c(-2:2) * 90, expand = c(0L, 0L)) +
  scale_y_units(unit = "m/s")+
  facet_wrap(~season)+
  theme_linedraw()
# Note to plot:
# - during migration movement is directional and speeds are higher
# - in breeding speeds are low and movement is in all directions
# - here again we see that wintering contains some migration

### "Wind rose"-type plot, showing turning angles AND speed, again per season
pi_r <- set_units(pi, "rad")

ggplot(Leo_gg) +
  geom_histogram(
    aes(x = turnangle, fill = speed_categorical),
    position = position_stack(reverse = TRUE)) +
  scale_fill_ordinal("Speed") +
  coord_polar(start = pi) +
  scale_x_units(
    name = NULL,
    limits = c(-pi_r, pi_r),
    breaks = (-2L:1L) * (drop_units(pi_r/2)),
    labels = c("180", "270", "0", "90")) +
  scale_y_continuous(limits = c(-500L, 650L), breaks = c(0L, 250L, 500L)) +
  facet_wrap(~season) +
  theme_linedraw()
# Note to plot:
# - probability of turning around is dependent on speed: the faster, the harder to turn
# - high speeds only have low turning angles
# - low speeds have all turning angles


#_______________________
# REMOVING OUTLIERS ####
#_______________________

#__________________________________
### based on Movebank features ####
bts <- movebank_download_study(study_id = 1918503, 
                        sensor_type_id = "radio-transmitter", 
                        remove_movebank_outliers = F)
# When downloading data using movebank_download_study(), the argument "remove_movebank_outliers" is TRUE by default
mt_movebank_visible(bts) %>% table()
# but if false, we could then remove them by:
bts <- mt_filter_movebank_visible(bts)

#________________________
### based on mapping ####

# Back to the buffaloes' example
mt_n_tracks(buff)

# quick map to detect outlier
mapview::mapView(buff$geometry)

# get the position of the coordinate that has the max longitude
which.max(st_coordinates(buff)[,1])
# drop the point with the largest coordinate values
buff_noOut <- buff[-which.max(st_coordinates(buff)[,1]), ]

# The outlier has been removed and we save the cleaned object
mapview::mapView(buff_noOut$geometry)

#________________________  
### based on speed ####

## arrange track
buff <- dplyr::arrange(buff, mt_track_id(buff), mt_time(buff)) 
## remove duplicated timestamps
buff <- buff %>% 
  mutate(n_na = rowSums(is.na(pick(everything())))) %>%
  dplyr::arrange(n_na) %>%
  mt_filter_unique(criterion='first') %>% 
  dplyr::arrange(mt_track_id()) %>% 
  dplyr::arrange(mt_track_id(),mt_time())

## get speeds and look at max
buff_speed <- mt_speed(buff, "m/s") 
hist(buff_speed)
(q <- quantile(buff_speed, seq(0.99,1,0.001), na.rm=T))
plot(drop_units(q))

max_speed <- set_units(15, m/s) 
while(any(mt_speed(buff) > max_speed, na.rm = TRUE)){
  buff <- buff %>% filter(mt_speed(.)<=max_speed | is.na(mt_speed(.)))
}
hist(mt_speed(buff, "m/s"))

saveRDS(buff, file="buffalo_cleaned.rds")

#__________________
# MISSED FIXES ####
#__________________

## Missed fixes show in the data like entries with timestamp but without location
## we tend to always just remove them, but sometimes these can also be informative!

# load data from Leroy the fisher
fishers <- mt_read(mt_example())
leroy <- filter_track_data(fishers, .track_id = "M1")
#leroy <- mt_read(list.files(system.file("extdata", package = "move2"), full.names = T))

# pick one year
leroy <- leroy[year(mt_time(leroy))==2009,]
# get the time stamps of entries without location, using sf::st_is_empty(leroy)
missedFixes <- data.frame(time = mt_time(leroy[sf::st_is_empty(leroy),]), status = "Not Successful")
# add the time stamps of positions obtained
obtainedFixes <- data.frame(time = mt_time(leroy[! sf::st_is_empty(leroy),]), status = "Successful")

# rbind them and change time to local time zone of the study
df <- rbind(missedFixes, obtainedFixes)
df$time <- lubridate::with_tz(df$time, tz="America/New_York")

# Plot histogram that is filled out with proportions binned per hours
ggplot(df, aes(x=hour(time), fill=status)) +
  geom_histogram(binwidth=1, position='fill') +
  xlab("Hour of the day") + ylab("Proportion") +
  scale_fill_grey()
# Fishers are nocturnal animals, during the day they are in the den

## histogram filled out with proportions binned per day
ggplot(df, aes(x=time, fill=status)) +
  geom_histogram(binwidth=24*60*60, position='fill') +
  xlab("Date") + ylab("Proportion") +
  scale_fill_grey()



#________________________________________________
# IRREGULAR DATA: INTERPOLATION AND THINNING ####
#________________________________________________

# Most empirical trajectories have gaps/irregularity in the sampling, 
# which can be a problem with some analytical methods assuming regular sampling.
# To regularize movement data there are technically two solutions: 
# 1. reduce the data to the largest gap in time or 
# 2. interpolate positions in the gappy sections. 
# Both come with their problems, but especially the second one makes strong assumptions with the following consequences:

#__________________________________________
### Consequence of interpolating point ####

# in move2 empty locations are kept with their timestamps, so interpolation is as easy as:
leroy <- mutate(leroy, locationType=ifelse(st_is_empty(leroy), "interpolated","true"))
leroyInt <- mt_interpolate(leroy)

ggplot()+
  geom_sf(data = mt_track_lines(leroyInt), color="grey50")+
  geom_sf(data = leroyInt,  aes(color=locationType))+
  scale_color_manual("",values = c(true = alpha("blue", 0.3), interpolated = alpha("firebrick", 0.5)))+
  theme_void()
# As we saw above, the "gaps" (missed fixes) can be biologically meaningful: in this case Leroy is denning.

# What happens to the autocorrelation structure?
leroyInt <- filter(leroyInt, !st_is_empty(leroyInt)) # first and last locations are empty and cannot be interpolated
acf(drop_units(mt_speed(leroyInt)[-nrow(leroyInt)]), lag.max=100, main="speeds")
# The pattern in autocorrelation is the result of introducing these linearly interpolated positions

# Final notes on interpolation:
# rather work with gaps and errors then introducing points (unless we are sure they were in a static location (e.g. den)
# or for smooth visualizations


#___________________________________________
### Consequence of subsampling/thinning ####

# Difference in speed due to differences in sampling

library(move)
library(adehabitatLT)
set.seed(7478)
# simulate a track
r.track <- as(simm.crw(1:1000, 1, 0.99), "Move")
# thin it to 1 location every 3
r.trackThin <- r.track[seq(1,nrow(coordinates(r.track)),3),]
# compare the distribution of their speeds (non-parametric test to compare distributions)
ks.test(sqrt(speed(r.track)), sqrt(speed(r.trackThin)))

hist(sqrt(speed(r.trackThin)), freq=F, xlim=c(0,2), breaks="FD", col=alpha("grey", 0.5), xlab="Square root transformed speed", main=NA)
hist(sqrt(speed(r.track)), freq=F, add=T, breaks="FD", col=alpha("white", 0.5))
legend("topleft", c("all positions", "every 3rd position"), fill=c("white", "grey"), bty="n")
# high segment values are only possible with high sampl freq, in the thinned track we underestimate distance covered in the same time lag
# ground speed should always come together with the rate at which it was sampled

# Their distribution is different but their means quite similar
# non-parametric test to compare ranks
wilcox.test(sqrt(speed(r.track)), sqrt(speed(r.trackThin)))
# parametric test to compare means
t.test(sqrt(speed(r.track)), sqrt(speed(r.trackThin)))

# Final notes on thinning, especially when comparing groups:
# Always important to compare the "operational sampling rate" how many fixes I get per unit time, 
# irrespective of the sampling schedule I choose
# (e.g. if we are comparing sexes, and one is always between leaves battery is lower and they get less fixes)


########
### Thinning/subsampling example with real tracking data (added after the session)
########

## in move2 there is a function 'mt_filter_per_interval()' to subsample/thin the data. 
## The function selects one event per defined time interval (time window). 
## The time lag between the selected events does not necessarily correspond to the defined interval. 
## For example, if the defined time interval is "1 hour" with the criterion "first", the function will 
## select the event that is closest to every full hour (within each window), so if the first events of a 
## track are at 10:45, 10:55, 11:05, 11:30, 12:01, 12:33, 13:00, the selected timestamps will be 10:45, 
## 11:05, 12:01, 13:00, each of them falling into different time windows (in this case hours), but 
## resulting in timelags of 20mins, 56min, 59mins.
## When down sampling a track, the time lags mostly correspond to the chosen time interval, except the first
## time lag (and when there are gaps in the data).

## Depending why one needs to thin the data, having here and there smaller timelags is no problem.
## A function to thin the data and only retain locations with a set minimum timelag 
## (i.e. to obtain somewhat regular data) is in the doings. Up to then, have a look at
## ?amt::track_resample (it does a great job at regularizing the data the best possible way, 
## with what is available. It only uses actual locations, no interpolation is done)

fishers <- mt_read(mt_example())
fishers <- dplyr::filter(fishers, !sf::st_is_empty(fishers))
## first have a look at the distribution of timelags of your data. 
summary(mt_time_lags(fishers, units="min"))
## thin the data to 1 location per hour. Here the first location per 1h window is picked
fishers_1h <- mt_filter_per_interval(fishers,criterion="first",unit="1 hour")
tl <- mt_time_lags(fishers_1h, units="min")
summary(tl)
## there are still some small timelags, due to gaps in the data
length(which(tl<set_units(55,"min")))/nrow(fishers_1h)
## but only 9% of the locations have a timelag smaller than 55min
