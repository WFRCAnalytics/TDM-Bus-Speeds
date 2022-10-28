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
library(gtfstools)
library(gtfs2gps)
library(hms)
library(geotidy)
library(sfheaders)


# Source Scripts for All Analyses
source("R/bus_speeds.R")
source("R/bus_speeds_visuals.R")
source("R/gtfs_functions.R")

#####################################################################################################################################################################################################
# Primary Analysis # UTA Bus Stop Points #
#####################################################################################################################################################################################################

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
  tar_target(tdm_segments, get_tdm_segments("data/TDM/Master_Link.shp", "data/TDM/loadednet/v832_SE19_Net19__Assigned.dbf")),
  
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

visual_targets<- tar_plan(
  #' create visual plots to understand how UTA observed speeds compare with TDM modeled speeds
  tar_target(joint_estimated_speeds, join_estimated_speeds(pk_0_estimated_speeds, pk_1_estimated_speeds, ok_0_estimated_speeds, ok_1_estimated_speeds)),
  tar_target(mutated_joint_speeds, mutate_joint_speeds(joint_estimated_speeds,FTG_2019)), # FT_2019 or FTG_2019
  tar_target(descLineGraphs, mapPlots(mutated_joint_speeds, "FTG", descLinePlotter)),
  tar_target(descScatterPlots, mapPlots(mutated_joint_speeds, "FTG", descScatterPlotter)),
  tar_target(aveScatterPlots, mapPlots(mutated_joint_speeds, "FTG", aveScatterPlotter)),
  tar_target(errScatterPlots, mapPlots(mutated_joint_speeds, "FTG", errorScatterPlotter))
)

#descLineGraphs <- tar_read(descLineGraphs)
#makePNGs(descLineGraphs, "outputs/DescendingSpeeds_Exact")
#descScatterPlots <- tar_read(descScatterPlots)
#makePNGs(descScatterPlots, "outputs/DescendingSpeeds_Average")
#aveScatterPlots <- tar_read(aveScatterPlots)
#makePNGs(aveScatterPlots, "outputs/ScatterPlotSpeeds_Average")
#errScatterPlots <- tar_read(errScatterPlots)
#makePNGs(errScatterPlots, "outputs/ScatterPlotSpeeds_Error")


#####################################################################################################################################################################################################
# Secondary Analysis # GTFS Stop Schedule Data #
#####################################################################################################################################################################################################

gtfs_targets <- tar_plan(
  tar_target(gtfs_path, "C:/Users/cday/Documents/projects/TDM-Bus-Speeds/data/UTA/gtfs.zip"),
  tar_target(uta_gtfs, gtfstools::read_gtfs(gtfs_path)),
  tar_target(gtfs_gps, gtfs2gps(uta_gtfs, spatial_resolution = 100)),
  tar_target(gtfs_gps_lines, gps_as_sflinestring(gtfs_gps)),
  tar_target(gtfs_gps_points, gps_as_sfpoints(gtfs_gps)),
  
  tar_target(trips, uta_gtfs$trips),
  tar_target(routes, uta_gtfs$routes),
  tar_target(triproutes, merge_trip_routes(gtfs_gps_lines,trips,routes)),
  tar_target(timepeaks, calculate_time_peak(triproutes)),
  tar_target(mflines, c("1004","12704","13304","210704","24904","128004","142204","340904","504","150804","4", "17")),
  tar_target(stop_locations, get_stop_locations(uta_gtfs, gtfs_gps_points,mflines)),
  tar_target(gps_lines_mf, filter_lines(timepeaks,mflines)),
  tar_target(speed_sums, summarize_speeds(gps_lines_mf)),
  tar_target(segments, st_make_segments(speed_sums)),
  tar_target(geosphere, calculate_gtfs_compass(segments)),
  
  tar_target(gtfs_tdm_routes, left_join(geosphere,tdm_uta_conversion, by = c("route_short_name" = "GTFSRoute"))),
  tar_target(tdm_lines_compass, calculate_tdm_compass(tdm_transit_lines)),
  tar_target(tdm_centroids_compass, make_centroids(tdm_lines_compass)),
  tar_target(tdm_centroids_clean2, clean_centroids2(tdm_centroids_compass)),
  
  tar_target(join_tdm_gtfs, join_gtfs(gtfs_tdm_routes,tdm_centroids_clean2)),
  tar_target(merge_tdm_lines, merge_tdm_links(join_tdm_gtfs,tdm_lines_compass))
)

gtfs_visual_targets <- tar_plan(
  tar_target(gtfs_histo_df, make_histo_df(merge_tdm_lines)),
  tar_target(routeplots, plot_routes(gtfs_histo_df)),
  tar_target(joint_gtfs_speeds, join_gtfs_speeds(merge_tdm_lines)),
  tar_target(mutated_gtfs_speeds, mutate_joint_speeds(joint_gtfs_speeds,FTG_2019)), # FT_2019 or FTG_2019
  
  tar_target(g_descLineGraphs, mapPlots(mutated_gtfs_speeds, "FTG", descLinePlotter)),
  tar_target(g_descScatterPlots, mapPlots(mutated_gtfs_speeds, "FTG", descScatterPlotter)),
  tar_target(g_aveScatterPlots, mapPlots(mutated_gtfs_speeds, "FTG", aveScatterPlotter)),
  tar_target(g_errScatterPlots, mapPlots(mutated_gtfs_speeds, "FTG", errorScatterPlotter))
)


tar_plan(
  
  data_targets,
  analysis_targets,
  visual_targets,
  
  gtfs_targets,
  gtfs_visual_targets
  
)


