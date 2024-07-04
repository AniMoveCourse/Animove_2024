# Date: June 2024
## this script contains translators (mostly both directions) between the move2 object and the:
# - telemetry class of ctmm package: move2_TO_telemetry() (mt_as_move2() reads in telemetry & telemetry lists)
# - track_xyt from amt: move2_TO_track.xyt() (mt_as_move2() reads in track.xyt objects)
# - binClstPath and binClstStck from EMbC: move2_TO_embctrk() & embctrk_TO_move2()
# - ltraj from adehabitatLT: move2_TO_ltraj() & ltraj_TO_move2()
# - data.frame for recurse functions: move2_TO_recurseDF()
# - data.frame containing all event and track attributes: move2_TO_df() (mt_as_move2() reads in data.frame)
# - to_move() & mt_as_move2() - between move and move2 objects
## Note: in the future some of the packages above will probably allow a move2 object as an input, and the move2 package will have functions to transform some of the classes above into a move2 object
## Note: most functions print a warning message: this is no error, this is to communicate important information and assumptions

############################
### telemetry from ctmm ###
############################
library(move2)
library(ctmm)
library(sf)
move2_TO_telemetry <- function(mv2) {
  # needed columns: individual.local.identifier (or tag.local.identifier), timestamp, location.long and location.lat
  mv2 <- mt_as_event_attribute(mv2, names(mt_track_data(mv2)))
  mv2 <- dplyr::mutate(mv2, location.long=sf::st_coordinates(mv2)[,1],
                        location.lat=sf::st_coordinates(mv2)[,2])
  
  mv2df <- data.frame(mv2)
  ## as.telemetry expects the track id to be called "individual.local.identifier" this is a quick fix, it might need some more thought to it to make it nicer. HOPE THIS IS FIXED ONCE ctmm INTEGRATES READING IN move2
  # fix: idtrack colum gets the prefix "track_id:", individual.local.identifier gets the sufix "_original" to maintain this original information
  colnames(mv2df)[colnames(mv2df)%in%make.names(mt_track_id_column(mv2))] <- paste0("track_id:",make.names(mt_track_id_column(mv2)))
  colnames(mv2df)[colnames(mv2df)%in%c("individual.local.identifier","individual_local_identifier","individual-local-identifier")] <- paste0(colnames(mv2df)[colnames(mv2df)%in%c("individual.local.identifier","individual_local_identifier","individual-local-identifier")],"_original")
  mv2df$individual_local_identifier <-mt_track_id(mv2)
  mv2df$timestamp <- mt_time(mv2) # ensuring used timestamps are in the column "timestamp" as expected by as.telemetry()
  telem <- as.telemetry(mv2df,
                        timezone=tz(mt_time(mv2)),
                        projection= if(st_is_longlat(mv2)){NULL}else{projection(mv2)},
                        na.rm= "col",
                        keep=T)
  return(telem)
}

# telemetry_TO_move2 => mt_as_move2 reads in a telemetry /list telemetry object


############################
#### track_xyt from amt ####
############################
library(move2)
library(amt)
library(sf)
move2_TO_track.xyt <- function(mv2){
  if(mt_is_move2(mv2)){
    warning("!!INFO!!: only coordinates, timestamps and track IDs are retained")
    track(
      x=sf::st_coordinates(mv2)[,1],
      y=sf::st_coordinates(mv2)[,2],
      t=mt_time(mv2),
      id=mt_track_id(mv2),
      crs = sf::st_crs(mv2)
    )
  }
}

# track.xyt_TO_move2 => mt_as_move2 reads in a track.xyt object

###########################################
## binClstPath and binClstStck from EMbC ##
###########################################
library(move2)
library(EMbC)
library(sf)
move2_TO_embctrk <- function(mv2){
  if(mt_is_move2(mv2)){
    warning("this function converts the move2 object into the data.frame or list of data.frames (if multiple tracks) needed for the stbc() of the embc package")
    warning("!!INFO!! make sure you have removed all empty locations on the move2 object, if not stbc() will give an error.")
    if(mt_n_tracks(mv2)==1){
      embctrk <- data.frame(timeStamp=mt_time(mv2),
                            lon=sf::st_coordinates(mv2)[,1],
                            lat=sf::st_coordinates(mv2)[,2],
                            track=mt_track_id(mv2))
    }
    if(mt_n_tracks(mv2)>1){
      df <-  data.frame(timeStamp=mt_time(mv2),
                        lon=sf::st_coordinates(mv2)[,1],
                        lat=sf::st_coordinates(mv2)[,2],
                        track=mt_track_id(mv2))
      embctrk <- split(df,f=df$track)
    }
    return(embctrk)
  }
}


embctrk_TO_move2 <- function(embctrk){
  warning("!!INFO!!: embc classes do not include track ids in their objects, when transforming back and forth these get lost")
  if (is(embctrk, "binClstPath")){
    embctrk2df <- data.frame(
      lon = embctrk@pth$lon,
      lat = embctrk@pth$lat,
      time = embctrk@pth$dTm,
      velocity.m.s = embctrk@X[, 1],
      turn.rad = embctrk@X[, 2],
      W = embctrk@W,
      A = embctrk@A,
      track = "unnamed"
    )
    mv2 <- mt_as_move2(embctrk2df,
                       coords = c("lon", "lat"),
                       time_column="time",
                       track_id_column="track",
                       crs= embctrk@tracks@proj4string@projargs)
  }
  if (is(embctrk, "binClstStck")){
    mv2_L <- lapply(1:length(embctrk@bCS), function(i) {
      embctrk2df <- data.frame(
        lon = embctrk@bCS[[i]]@pth$lon,
        lat = embctrk@bCS[[i]]@pth$lat,
        time = embctrk@bCS[[i]]@pth$dTm,
        velocity.m.s = embctrk@bCS[[i]]@X[, 1],
        turn.rad = embctrk@bCS[[i]]@X[, 2],
        W = embctrk@bCS[[i]]@W,
        A = embctrk@bCS[[i]]@A,
        track = paste0("unnamed", i)
      )
      mt_as_move2(embctrk2df,
                  coords = c("lon", "lat"),
                  time_column="time",
                  track_id_column="track",
                  crs= embctrk@bCS[[i]]@tracks@proj4string@projargs)
    })
    mv2 <- mt_stack(mv2_L,.track_combine="rename")
  }
  return(mv2)
}

###############################
### ltraj from adehabitatLT ###
###############################
library(move2)
library(adehabitatLT)
library(sf)
move2_TO_ltraj <- function(mv2){
  if(mt_is_move2(mv2)){
    warning("!!INFO!!: only coordinates, timestamps and track IDs are retained")
    warning("!!INFO!!: projection information is lost as the ltraj function still only accepts proj4 strings")
    if(sf::st_is_longlat(mv2)){
      warning("Converting a object in geographic coordinate system (lat/long) while the ltraj assums projected data")
    }
    adehabitatLT::as.ltraj(as.data.frame(sf::st_coordinates(mv2)), date = mt_time(mv2), id = as.character(mt_track_id(mv2)), typeII = T)#, proj4string=sf::st_crs(mv2))
  }
}

ltraj_TO_move2 <- function(ltj){
  if(is(ltj,"ltraj")){
    if (!attr(ltj, "typeII")) {
      stop("Can only work on typeII objects")
    }
    mv2_L <- lapply(1:length(ltj), function(i) {
      ltj2df <- data.frame(ltj[[i]])
      ltj2df <- data.frame(ltj2df,attr(ltj[[i]], "infolocs"))
      ltj2df$id <- attr(ltj[[i]], "id")
      ltj2df$burst <- attr(ltj[[i]], "burst")

      mt_as_move2(ltj2df,
                  coords = c("x", "y"),
                  time_column="date",
                  track_id_column="id",
                  crs= projection(ltj[[i]]))
    })

    mv2 <- mt_stack(mv2_L,.track_combine="rename")
  }
}


#######################################
### data.frame for recurse functions ##
#######################################
library(move2)
library(recurse)
library(sf)
move2_TO_recurseDF <- function(mv2){
  if(mt_is_move2(mv2)){
    warning("this function converts the move2 object into the data.frame needed for most functions in the recurse package")
    data.frame(
      x=sf::st_coordinates(mv2)[,1],
      y=sf::st_coordinates(mv2)[,2],
      t=mt_time(mv2),
      id=mt_track_id(mv2)
    )
  }
}


######################################################
### data.frame with all event and track attributes ###
######################################################
library(sf)
move2_TO_df <- function(mv2){
  if(mt_is_move2(mv2)){
    ## 1. move all track associated attributes to the event table
    mv2 <- mt_as_event_attribute(mv2, names(mt_track_data(mv2)))
    ## 2. put coordinates in 2 columns
    mv2 <- dplyr::mutate(mv2, location_long=sf::st_coordinates(mv2)[,1],
                         location_lat=sf::st_coordinates(mv2)[,2])
    ## 3. remove the sf geometry column from the table    
    df <- data.frame(sf::st_drop_geometry(mv2))
    return(df)
  }
}
