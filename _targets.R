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
  tar_target(tdm_transit_nodes, get_transit_nodes(tdm_transit_lines)),
  tar_target(tdm_centroids, make_centroids(tdm_transit_lines)),
  
  #Data Cleaning
  tar_target(uta_points_clean, clean_uta_points(uta_points)),
  tar_target(tdm_centroids_clean, clean_centroids(tdm_centroids))
)


analysis_targets <- tar_plan(
  tar_target(close_uta_points, filter_far_uta_stops(uta_points_clean,tdm_transit_lines,150,109)),
  
  tar_target(pk_0_uta_on_tdm, merge_uta_tdm("pk",0,109,close_uta_points,tdm_centroids_clean)),
  tar_target(pk_1_uta_on_tdm, merge_uta_tdm("pk",1,109,close_uta_points,tdm_centroids_clean)),
  tar_target(ok_0_uta_on_tdm, merge_uta_tdm("ok",0,109,close_uta_points,tdm_centroids_clean)),
  tar_target(ok_1_uta_on_tdm, merge_uta_tdm("ok",1,109,close_uta_points,tdm_centroids_clean)),
  
  tar_target(pk_0_centroid_speeds, calc_centroid_speeds(pk_0_uta_on_tdm)),
  tar_target(pk_1_centroid_speeds, calc_centroid_speeds(pk_1_uta_on_tdm)),
  tar_target(ok_0_centroid_speeds, calc_centroid_speeds(ok_0_uta_on_tdm)),
  tar_target(ok_1_centroid_speeds, calc_centroid_speeds(ok_1_uta_on_tdm)),
  
  tar_target(pk_0_segment_speeds, calc_segment_speeds(tdm_centroids_clean, pk_0_centroid_speeds)),
  tar_target(pk_1_segment_speeds, calc_segment_speeds(tdm_centroids_clean, pk_1_centroid_speeds)),
  tar_target(ok_0_segment_speeds, calc_segment_speeds(tdm_centroids_clean, ok_0_centroid_speeds)),
  tar_target(ok_1_segment_speeds, calc_segment_speeds(tdm_centroids_clean, ok_1_centroid_speeds))
  
)



tar_plan(
  data_targets,
  analysis_targets
)

#test <- tar_read(ok_1_segment_speeds)
