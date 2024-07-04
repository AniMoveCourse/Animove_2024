#########################################################
###                  AniMove 2024                     ###    
### Script by Kami Safi, Martina Scacco & Anne Scharf ###
#########################################################
###                   Visualization                   ###
#########################################################

library(move2)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(gganimate)
library(plotly)
library(mapview)
library(gganimate)
library(units)
library(lubridate)
library(moveVis)

# set wd to the data folder in your computer
setwd("/home/ascharf/ownCloud/Animove_2024_prep/prep_scripts/data")

#___________________________
# MAPPING MOVEMENT DATA ####
#___________________________

bats <- mt_read("Parti-colored bat Safi Switzerland.csv")

###_____________________
### Basic plotting ####

#### basic plots colored by individual (with graphics)
plot(bats["individual-local-identifier"], 
     max.plot=1, pch=16, cex=0.5, axes=T)

#### basic plots colored by individual (with ggplot2)
ggplot() + theme_void() +
  geom_sf(data = bats) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`))

###________________________
### Plotting on a map ####

#### plot on the world boundaries
worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")

ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  geom_sf(data = bats) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`))
  
# zoom in by subsetting the country
ggplot() + theme_void() +
  geom_sf(data = worldMap[worldMap$name == 'Switzerland',]) +
  geom_sf(data = bats) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`)) 

# or by cropping the map with a bounding box
bb <- st_bbox(bats)
exp <- 2

ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  geom_sf(data = bats) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`)) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)

#### plot on different background maps
## with "ggspatial" 
## only "osm" background 
ggplot() +
  ggspatial::annotation_map_tile(zoom = 9) +
  ggspatial::annotation_scale(aes(location="br")) +
  theme_linedraw() +
  geom_sf(data = bats, color = "darkgrey", size = 1) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`)) +
  guides(color = "none")


## with "ggmap"
## currently for stadia and google and API Key is needed and osm is unavailable
## for stadia: 
##     - sign up here: https://client.stadiamaps.com/signup/
##     - go to profile - manage properties - API Key
## register api key:
register_stadiamaps("YOUR-API-KEY-HERE", write = FALSE)
# request map data and then plot
names(bb) <- c("left","bottom","right","top") #the bounding box needs renaming
m <- ggmap::get_map(location = bb, zoom=9, source="stadia", maptype = "stamen_terrain")
m <- ggmap::get_stadiamap(bbox=bb, zoom=9, maptype = "stamen_terrain")

ggmap(m) +
  geom_sf(data = bats, inherit.aes = FALSE) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`), inherit.aes = FALSE) +
  guides(color = "none")

# other possibilities
m_wc <- ggmap::get_stadiamap(bbox = bb, zoom=9, maptype = "stamen_watercolor")
m_bw <- ggmap::get_stadiamap(bbox = bb, zoom=9, maptype = "stamen_toner")

gg_wc <- 
  ggmap(m_wc) +
  geom_sf(data = bats, inherit.aes = FALSE) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`), inherit.aes = FALSE) +
  guides(color = "none")+
  annotation_scale(aes(location="br"))
gg_bw <- 
  ggmap(m_bw) +
  geom_sf(data = bats, inherit.aes = FALSE) +
  geom_sf(data = mt_track_lines(bats), aes(color = `individual-local-identifier`), inherit.aes = FALSE) +
  guides(color = "none")+
  annotation_scale(aes(location="br"))

library(patchwork)
(gg_wc + gg_bw) #+ plot_layout(guides = "collect") & theme(legend.position = "bottom") #to use if one of the plots has legend

#_______________________
# INTERACTIVE PLOTS ####
#_______________________

###____________
### Plotly ####

# by wrapping a ggplot in plotly
plotly::ggplotly(gg_wc,
                 tooltip = c("individual-local-identifier"))


###____________
### Mapview ####

# by using mapview, after changing the class so that it is recognised as an SF object
batsSF <- bats
class(batsSF) <- class(bats) %>% setdiff("move2") # remove class "move2" from object
mapview::mapView(batsSF, zcol="individual-local-identifier", legend=F) #as points
mapview::mapView(mt_track_lines(bats), zcol="individual-local-identifier", legend=F) #as lines

# see this book "Making Maps with R" for more details on making maps in R: 
# https://bookdown.org/nicohahn/making_maps_with_r5/docs/introduction.html

#_________________________________
# ANIMATE TRACKS WITH gganimate ####
#_________________________________
mv2 <- mt_read(mt_example())

data_interpolated <- mv2[!sf::st_is_empty(mv2), ] |>
  mt_interpolate(
    seq(
      as.POSIXct("2011-02-05"),
      as.POSIXct("2011-02-15"), "1 hour" 
    ),
    max_time_lag = units::as_units(3, "hour"),
    omit = TRUE
  )
animation <- ggplot() +
  annotation_map_tile(zoom = 9, progress = "none") +
  annotation_scale() +
  theme_linedraw() +
  geom_sf(
    data = data_interpolated, size = 3,
    aes(color = `individual-local-identifier`)
  ) +
  transition_manual(timestamp) +
  labs(
    title = "Albany-NY fishers",
    subtitle = "Time: {current_frame}",
    color = "Individual"
  )
gganimate::animate(animation,
                   nframes = length(unique(data_interpolated$timestamp))
)

## see more gganimate examples here: https://bartk.gitlab.io/move2/articles/albatross.html

#_________________________________
# ANIMATE TRACKS WITH moveVis ####
#_________________________________

# Not available on CRAN at the moment, but from github repository: 
# devtools::install_github("16EAGLE/moveVis")

library(moveVis)
library(move)

# we will use the example dataset in the package, of storks in germany
data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion
# if they are in move2 use to_move() to convert them

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "mins")
# reproject m to web mercator projection (default for base maps)
m <- sp::spTransform(m, "EPSG:3857") #spTransform is a sp based function that was used with move

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "carto", map_type = "dark", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames and save a gif in your home directory
animate_frames(frames, out_file = "moveVis.gif")

# Link to the Animove 2022 recording from Jakob Schwalb-Willmann:
# https://animove.org/elearning/ -> moveVis
# Link to the website:
# https://movevis.org/

