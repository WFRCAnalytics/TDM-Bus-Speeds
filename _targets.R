library(targets)
library(tarchetypes)
library(tidyverse)
library(readr)
library(foreign)
library(sf)
library(leaflet)
library(mapview)
library(RColorBrewer)
library(rgdal)

source("R/bus_speeds.R")


data_targets <- tar_plan(
  #Read in Data
  tar_target(tdm_uta_conversion, read_csv("data/TDMtoUTARoute.csv") %>% 
               mutate(LabelNum = row_number())
             ),
  tar_target(tdm_data, read.dbf("data/TDM/_v832_SE19_Net19_2_OD_Station_Detail.dbf") %>%
               left_join( tdm_uta_conversion, by = c("NAME" = "TDMRoute"))
             ),
  tar_target(uta_data, read.csv("data/UTA/UTASep2019Tue-ThuBusSpeeds.csv") %>%
               left_join(tdm_uta_conversion, by = c("ROUTE" = "UTARoute"))
             ),
  tar_target(tdm_segments, get_tdm_segments("data/TDM/Master_Link.shp")),
  
  #Spatial Orientation
  tar_target(uta_points, get_uta_points(uta_data)),
  tar_target(tdm_transit_lines, get_transit_lines(tdm_data, tdm_segments)),
  tar_target(tdm_transit_lines_fixed, fix_transit_lines(tdm_transit_lines)),
  tar_target(tdm_transit_nodes, get_transit_nodes(tdm_transit_lines_fixed)),
  tar_target(tdm_centroids, make_centroids(tdm_transit_lines_fixed)),
  
  #Data Cleaning
  tar_target(uta_points_clean, clean_uta_points(uta_points)),
  tar_target(uta_points_clean_fixed, fix_clean_uta_points(uta_points_clean)),
  tar_target(tdm_centroids_clean, clean_centroids(tdm_centroids)),
  tar_target(uta_map_points, mapable_uta_points(uta_points_clean_fixed))
)


analysis_targets <- tar_plan(
  # Filter UTA Stops based on distance from TDM
  tar_target(close_uta_points, filter_far_uta_stops(uta_points_clean_fixed,tdm_transit_lines_fixed,150,109)),
  
  # Merge UTA Data onto Centroid Data by period and directionality
  #' (109 - total number of transit routes,
  #'  1/0 - UTA directionality,
  #'  pk/ok - Peak / Off-Peak)
  tar_target(pk_0_uta_on_tdm, merge_uta_tdm("pk",0,109,close_uta_points,tdm_centroids_clean)),
  tar_target(pk_1_uta_on_tdm, merge_uta_tdm("pk",1,109,close_uta_points,tdm_centroids_clean)),
  tar_target(ok_0_uta_on_tdm, merge_uta_tdm("ok",0,109,close_uta_points,tdm_centroids_clean)),
  tar_target(ok_1_uta_on_tdm, merge_uta_tdm("ok",1,109,close_uta_points,tdm_centroids_clean)),
  
  #' Take the average of centroid speeds with multiple uta speeds
  tar_target(pk_0_centroid_speeds, calc_centroid_speeds(pk_0_uta_on_tdm)),
  tar_target(pk_1_centroid_speeds, calc_centroid_speeds(pk_1_uta_on_tdm)),
  tar_target(ok_0_centroid_speeds, calc_centroid_speeds(ok_0_uta_on_tdm)),
  tar_target(ok_1_centroid_speeds, calc_centroid_speeds(ok_1_uta_on_tdm)),
  
  #' Determine start and end uta point per tdm centroid value
  tar_target(pk_0_centroid_speed_summary, calc_centroid_speed_summary(pk_0_centroid_speeds, tdm_centroids_clean)),
  tar_target(pk_1_centroid_speeds_summary, calc_centroid_speed_summary(pk_1_centroid_speeds, tdm_centroids_clean)),
  tar_target(ok_0_centroid_speeds_summary, calc_centroid_speed_summary(ok_0_centroid_speeds, tdm_centroids_clean)),
  tar_target(ok_1_centroid_speeds_summary, calc_centroid_speed_summary(ok_1_centroid_speeds, tdm_centroids_clean)),
  
  #' Use start and end uta points to fill in all in-between tdm link
  #' speed values
  tar_target(pk_0_segment_speeds, calc_segment_speeds(tdm_centroids_clean, pk_0_centroid_speed_summary)),
  tar_target(pk_1_segment_speeds, calc_segment_speeds(tdm_centroids_clean, pk_1_centroid_speeds_summary)),
  tar_target(ok_0_segment_speeds, calc_segment_speeds(tdm_centroids_clean, ok_0_centroid_speeds_summary)),
  tar_target(ok_1_segment_speeds, calc_segment_speeds(tdm_centroids_clean, ok_1_centroid_speeds_summary)),
  
  #' Clean data and calculate final link speeds
  tar_target(pk_0_estimated_speeds, estimated_segment_speeds(pk_0_segment_speeds, tdm_transit_lines_fixed)),
  tar_target(pk_1_estimated_speeds, estimated_segment_speeds(pk_1_segment_speeds, tdm_transit_lines_fixed)),
  tar_target(ok_0_estimated_speeds, estimated_segment_speeds(ok_0_segment_speeds, tdm_transit_lines_fixed)),
  tar_target(ok_1_estimated_speeds, estimated_segment_speeds(ok_1_segment_speeds, tdm_transit_lines_fixed)),
  
  tar_target(pk_estimated_speeds, average_estimated_speeds(pk_0_estimated_speeds,pk_1_estimated_speeds)),
  tar_target(ok_estimated_speeds, average_estimated_speeds(ok_0_estimated_speeds,ok_1_estimated_speeds))
  
)

# Still To Do
#' Filter out or identify those stops that don't connect between centroids
#' determine which speed matches with which directionality
#' why are Avgmphdwell speeds 0 sometimes?
#' Maybe filter out really slow uta speeds?


tar_plan(
  data_targets,
  analysis_targets
)


#' route 18 is a good example of the flaws in my methodology. Try to fix it!! (UTA speeds aren't lining up with corresponding tdm links
#' ex. freeway links are like 20mph when the uta stops indicate above 40mph)
