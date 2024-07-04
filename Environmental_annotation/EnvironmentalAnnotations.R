#########################################################
###                  AniMove 2024                     ###    
###            Script by Elham Nourani                ###
#########################################################
###     Environmental annotation and plotting         ###
#########################################################


library(tidyverse)
library(terra)
library(sf)
library(mapview)
library(rnaturalearth)
library(cowplot)
library(viridis)

# set working directory
setwd("Environmental_annotation/")

#For this session, we will work with one sample track form Nourani et al. 2023. 
#See paper and data here: https://doi.org/10.1016/j.cub.2023.01.068 and original code here: https://github.com/mahle68/seabirds_storms_public


# Open data. The data has already been thinned to roughly an hourly resolution
albi <-  readRDS("data/albatross_track.rds")

#check the object class
class(albi)

#Let's have a look. Convert to sf for plotting
albi_sf <- albi %>% 
  st_as_sf(coords = c("location_long", "location_lat"), crs = "EPSG:4326")

#let's have a look
mapview(albi_sf)


###_________________________________
### Download environmental data ####
###_________________________________

library(ecmwfr)

### THIS CHUNKC OF CODE IS NOT REPRODUCIBLE UNLESS YOU SET UP YOUR CDS (Copernicus Climate Data Store) CREDENTIALS
#### Download data only for the duration and geographic extent of the track. 

yr <- unique(year(albi$timestamp)) %>% 
  as.character()

mnth <- unique(month(albi$timestamp)) %>% 
  as.character()

days <- unique(day(albi$timestamp)) %>% 
  as.character()

hours <- c(0:23) %>% str_pad(2, "left", "0") %>% paste0(":00")

#Decide on the geographic extent based on the extent of the data
#bbox<- st_bbox(albi_sf)

#path for the output data
output_path <- "wind_for_albi"

#setup your local keychain (see for details: https://github.com/bluegreen-labs/ecmwfr)
wf_set_key(user = "your_user_id",
           key = "your_API_key",
           service = "cds")

#request data for each hour of each day (you can also submit one request for all hours and all days at once)

lapply(days, function(dy){
  lapply(hours, function(hr){
    
    #prepare the request
    request <- list(
      "dataset_short_name" = "reanalysis-era5-single-levels",
      "product_type"   = "reanalysis",
      "variable"       = c("10m_u_component_of_wind", "10m_v_component_of_wind"),
      "year"           = yr,
      "month"          = mnth,
      "day"            = dy,
      "time"           = hr,
      "area"           = c(-25, -62, -51, 11),
      "format"         = "netcdf",
      "target"         = paste0("albi_wind_", yr, "_", mnth, "_", dy, "_", hr, ".nc"))
    
    #submit the download request
    wf_request(user = "your_user_id",
               request = request,
               transfer = TRUE,
               path = output_path,
               verbose = TRUE)
  })
})

###__________________________________________
### Extract wind data at tracking points ####
###__________________________________________

#list all netcdf files that were downloaded
wind_ls <- list.files("data/wind_for_albi", pattern = ".nc", full.names = T)

#for each timestamp of the albatross tracking data, open the corresponding netcdf file and extract the wind values

#create a new column for each unique hour
albi_sf <- albi_sf %>% 
  mutate(unique_hr = paste0(year(timestamp), "_", month(timestamp), "_", day(timestamp), "_", hour(timestamp) %>%  str_pad(2, "left", "0") %>% paste0(":00")))


albi_annotated <- lapply(albi_sf$unique_hr, function(x){
  
  #open the corresponding netcdf file. 
  wind <- rast(wind_ls[[grep(x, wind_ls)]]) #class(wind) #this is a spatRaster. You can plot it using plot(wind)
  
  row_with_wind <- albi_sf %>% 
    filter(unique_hr == x) %>% #filter for this unique hour
    extract(x = wind, y = ., method = "simple", bind = T) #In this example, the crs of tracking data and environmental data match. If they don't, make sure to use st_transform to change one to match the other.
  
  #return the annotated row
  row_with_wind
  
}) %>% 
  reduce(rbind) #rebound all the rows together


#let's have a look
mapview(albi_annotated, zcol = "u10")
mapview(albi_annotated, zcol = "v10")

#as it was mentioned in the course lecture, u (horizontal) and v (vertical) are the two components of the wind vector. 
#Let's calculate wind speed from these for easier interpretation of what's happening.

albi_annotated <- albi_annotated %>% 
  st_as_sf() %>%  #convert this back to an sf object from a spatVector
  dplyr::mutate(wind_speed = sqrt(u10^2 + v10 ^2))

#now let's see the track colored with wind speed
mapview(albi_annotated, zcol = "wind_speed")

#write to file. This is already included in the data folder
#saveRDS(data/albi_annotated, file = "albatross_annotated.rds")

#now we know what the wind speed that the bird encountered was. But, we don't know what the conditions around it looked like. Let's do some more exploration and plotting.

###__________________________________________
### Explore the environmental conditions ####
###__________________________________________

### convert the wind speed netcdf files to a dataframe for easier summarizing and plotting

#list all netcdf files that were downloaded
wind_ls <- list.files("wind_for_albi", pattern = ".nc", full.names = T) 

wind_df <- lapply(wind_ls, function(x){
  
  #extract timestamp from the file name
  dt <- str_sub(x, -19, -4)
  
  rst <- rast(x)
  df <- rst %>% 
    as.data.frame(xy = T) %>% 
    mutate(timestamp = dt, #add a timestamp column
           wind_speed = sqrt(u10^2 + v10 ^2)) %>%  #claculate wind speed
    rename(longitude = x,
           latitude = y) #rename lat and lon columns
  
  #return the dataframe for this timestamp
  df
}) %>% 
  reduce(rbind) #bind all rows together to have one dataframe for all the wind data

#write to file. This is already included in the data folder
#saveRDS(data/wind_df, file = "wind_df.rds")

### summarize the wind conditions: density distributions

#how did the wind conditions available and used vary?
summary(albi_annotated$wind_speed)
summary(wind_df$wind_speed)

#quick plotting to compare the distributions
ggplot() +
  geom_boxplot(data = wind_df, aes(x = factor(0), y = wind_speed, fill = "available")) +
  geom_boxplot(data = albi_annotated, aes(x = factor(1), y = wind_speed, fill = "used")) +
  scale_fill_manual(values = c("available" = "cornflowerblue", "used" = "lightcoral"), name = "") +
  theme_minimal() +
  scale_x_discrete(labels = c("available", "used")) +
  labs(x = "")

#It looks like albi really liked strong winds, but it never flew in the strongest wind.

#___________________________________________________#
# PLOT the track and colored it based on wind speed #
#___________________________________________________#

#get the world map as an sf object to use as the background
world <- ne_countries(scale = "medium", returnclass = "sf")

#plot the tracking points, color them based on values of wind speed
ws <- ggplot(data = world) +
  geom_sf(col = "white", fill = "black") +
  coord_sf(xlim = c(-60, -30), ylim = c(-43, -32), expand = FALSE) +
  geom_path(data = albi_annotated, aes(x = st_coordinates(albi_annotated)[,1], y = st_coordinates(albi_annotated)[,2], 
                                       col = wind_speed), linewidth = 3.5, lineend = "round") +
  scale_colour_viridis(option = "magma", na.value = "white", name = "m/s", alpha = 0.7) +
  theme_linedraw() +
  #scale_x_continuous(breaks = c(0,30)) +
  #scale_y_continuous(breaks = c(10,30,50)) +
  theme(axis.text = element_text(size = 10, colour = 1),
        legend.text = element_text(size = 10, colour = 1), 
        legend.title = element_text(size = 10, colour = 1),
        legend.position = "right",
        legend.background = element_rect(colour = NULL, fill = "white"))+
  labs(x = NULL, y = NULL, title = "Wind speed")

X11(height = 6, width = 11)
ws

#Let's do some plotting to see if albi interacted with the strongest wind or not. 

#_________________________________________________#
# PLOT the track over top of the wind speed data  #
#_________________________________________________#

### some prep work

#get the world map as an sf object to use as the background
world <- ne_countries(scale = "medium", returnclass = "sf")


#create a dataframe with spatial data of the world polygons for the plot inset. More details in a bit.
world_df <- cbind(data.frame(st_coordinates(st_centroid(world))), world) %>% 
  select(c("X", "Y", "featurecla", "name")) %>% 
  rename( long = X, lat = Y, group = featurecla)


#define an extent for the plot 
ext_df <- data.frame(x = c(-58, 8, 8, -58),
                     y = c(-50, -50, -26, -26))


#reduce the spatial resolution of the wind data...for the sake of plotting
wind_lres <- wind_df %>% 
  mutate(lat_lres = round(latitude),
         lon_lres = round(longitude)) %>% 
  group_by(timestamp, lat_lres, lon_lres) %>% 
  summarise(u10 = mean(u10, na.rm = T),
            v10 = mean(v10, na.rm = T),
            wind_speed = mean(wind_speed)) %>% 
  ungroup() %>% 
  rename(latitude = lat_lres,
         longitude = lon_lres)

#open the tracking data. Let's use the original data frame, not the sf that we created earlier
#albi <-  readRDS("data/albatross_track.rds")

albi <- albi %>% 
  mutate(unique_hr = paste0(year(timestamp), "_", month(timestamp), "_", day(timestamp), "_", hour(timestamp) %>%  str_pad(2, "left", "0") %>% paste0(":00"))) #add a unique_hr column

### create the map for the first hour of the foraging trip
one_hour <- split(albi, albi$unique_hr)[[1]]

unique_hr <- one_hour$unique_hr

#extract the wind data for this unique hour
wind <- wind_lres[wind_lres$timestamp == unique_hr,]

#plot!
main_map <- ggplot() +
  geom_tile(data = wind, aes(x = longitude, y = latitude, fill = wind_speed))+
  geom_segment(data = wind, 
               aes(x = longitude, xend = longitude+u10/10, y = latitude, 
                   yend = latitude+v10/10), arrow = arrow(length = unit(0.12, "cm")), size = 0.3)+
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "grey85") +
  geom_point(data = one_hour, aes(x = location_long, y = location_lat), color = "white", fill = "black", shape = 21, size = 3, stroke = 1.5, show.legend = FALSE) + #add the tracking location
  coord_sf(xlim = range(ext_df$x), ylim = range(ext_df$y))+
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                       na.value = "white", name = expression(paste("Wind speed (m s"^-1*")"))) +
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour = 1),
        legend.key.width=unit(0.27,"cm"),
        legend.text = element_text(size = 8, colour = 1), 
        legend.title = element_text(size = 9, colour = 1),
        legend.position = c(0.06, 0.78),
        legend.spacing.y = unit(0, 'cm'),
        legend.background = element_blank(),
        plot.title = element_text(face = "italic"))+
  labs(x = NULL, y = NULL, title = paste("Atlantic Yellow-nosed Albatross", 
                                         paste(one_hour$timestamp, "UTC", sep = " "), sep = "   "))

X11(height = 6, width = 11)
main_map

# the map looks nice, but I want to know what the entire foraging trip looks like 
#create inset map showing the foraging trajectory on the world map
inset <- ggplot() + 
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_polygon(data = ext_df, aes(x = x, y = y), fill = NA, color = "black", linewidth = 0.3) + 
  coord_map("ortho", orientation = c(albi$location_lat[1]+37, albi$location_long[1], 0)) +
  geom_path(data = albi, aes(x = location_long, y = location_lat), size = 0.17) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.background = element_rect(fill = "white")) +
  labs(x = NULL, y = NULL) 

inset


#Combine the main map and the inset together
map_with_inset <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset, x = 0.80, y = 0.565, width = 0.25, height = 0.25) 

X11(height = 6, width = 11)
map_with_inset


#Now, let's make one map for each unique hour in the foraging trip
#________________________________________________________________#

#create a directory to store the output files
dir.create("seabird_frames/") #the contents of this folder after running the following code will be 70 MB

#loop over unique hours
lapply(split(albi, albi$unique_hr), function(hr_point){
  
  unique_hr <- hr_point$unique_hr
  wind <-  wind_lres[wind_lres$timestamp == unique_hr,]
  
  main_map <- ggplot() +
    geom_tile(data = wind, aes(x = longitude, y = latitude, fill = wind_speed))+
    geom_segment(data = wind, 
                 aes(x = longitude, xend = longitude+u10/10, y = latitude, 
                     yend = latitude+v10/10), arrow = arrow(length = unit(0.12, "cm")), size = 0.3)+
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "grey85") +
    geom_point(data = hr_point, aes(x = location_long, y = location_lat), color = "white", fill = "black", shape = 21, size = 3, stroke = 1.5, show.legend = FALSE) + #add the tracking location
    coord_sf(xlim = range(ext_df$x), ylim = range(ext_df$y))+
    scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                         na.value = "white", name = expression(paste("Wind speed (m s"^-1*")"))) +
    theme_bw()+
    theme(axis.text = element_text(size = 11, colour = 1),
          legend.key.width=unit(0.27,"cm"),
          legend.text = element_text(size = 8, colour = 1), 
          legend.title = element_text(size = 9, colour = 1),
          legend.position = c(0.06, 0.78),
          legend.spacing.y = unit(0, 'cm'),
          legend.background = element_blank(),
          plot.title = element_text(face = "italic"))+
    labs(x = NULL, y = NULL, title = paste("Atlantic Yellow-nosed Albatross", 
                                           paste(one_hour$timestamp, "UTC", sep = " "), sep = "   "))
  
  #X11(height = 6, width = 11)
  final_plot <- ggdraw() +
    draw_plot(main_map) +
    draw_plot(inset, x = 0.80, y = 0.565, width = 0.25, height = 0.25) 
  
  #write the plot to file
  png(paste0("seabird_frames/", unique_hr, ".png"), 
      height = 6, width = 11, units = "in", res = 300)
  print(final_plot)
  dev.off()
  
})

#You can create a gif or an mp4 file using these frames. For example, I use ffmpeg in linux, but some websites allow you to upload your images and get a gif/video back.
#Animate from the linux terminal:
#ffmpeg -framerate 10 -pattern_type glob -i "*.png" output.mp4

#So, we can see that albi avoided the strongest wind speeds by flying in the eye of the storm! 
#Keep in mind that in this session we haven't done any formal analyses of the interaction between the moving animal and its environment. We will learn about these methods tomorrow :) 







