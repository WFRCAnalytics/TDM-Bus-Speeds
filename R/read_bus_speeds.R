
#' needed libraries
library(tidyverse)
library(readr)
library(foreign)
library(sf)
library(leaflet)
library(mapview)
library(RColorBrewer)
library(rgdal)

# Inputs ------------------------------------------------------------------------#
tdm_uta_conversion <- read_csv("data/TDMtoUTARoute.csv") %>%
  mutate(LabelNum = row_number())
tdm_segment_data <- read.dbf("data/TDM/_v832_SE19_Net19_2_OD_Station_Detail.dbf") %>%
  left_join(tdm_uta_conversion, by = c("NAME" = "TDMRoute"))
uta_segment_data <- read.csv("data/UTA/UTASep2019Tue-ThuBusSpeeds.csv") %>%
  left_join(tdm_uta_conversion, by = c("ROUTE" = "UTARoute"))
tdm_segments <- st_read("data/TDM/Master_Link.shp") %>%
  select(A,B) %>%
  mutate(link_id = paste0(A,"_",B)) %>%
  st_as_sf()


# UTA Points
uta_points <- uta_segment_data %>%
  st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>% # %>% mutate(geometry2 = lead(geometry))
  st_transform(26912)
  
uta_points_clean <- uta_points %>%
  group_by(ROUTE,DIR,period) %>%
  mutate(STOP2 = lead(STOP)) %>%
  filter(STOP != STOP2, !is.na(STOP2)) %>%
  mutate(PkOk = ifelse(grepl("m peak",period),"pk","ok")) %>%
  select(LabelNum,Label,DIR,period,PkOk,STOP,STOP2,Avgmph,Avgmphdwell,geometry) %>%
  ungroup()

# TDM Lines
tdm_segment_gis <- tdm_segment_data %>%
  filter(MODE %in% c(4,5,6,9)) %>%
  mutate(B = lead(A)) %>%
  mutate(link_id = paste0(A,"_",B)) %>%
  select("link_id",1:8,"B",9:24,"Label","LabelNum") %>%
  left_join(tdm_segments,by = c("link_id")) %>%
  filter(!is.na(A.y)) %>%
  st_as_sf()

# TDM Centroids
tdm_centroids_full <- tdm_segment_gis %>%
  mutate(midlinep = st_centroid(geometry)) %>%
  as.tibble() %>% select(-geometry) %>% st_as_sf() %>%
  mutate(centroid_id = row_number()) 

tdm_centroids <- tdm_centroids_full %>%
  select(centroid_id,LabelNum,Label,ONEWAY,LINKSEQ1,LINKSEQ2,midlinep)

# TDM Points
tdm_point_gis <- tdm_segment_gis %>%
  st_cast("POINT")


# Snapped UTA Points
#uta_snapped <- st_snap_points(uta_points,tdm_centroids,tolerance = 10) 
#joined_sf <- uta_points_clean %>% 
#  cbind(tdm_centroids[st_nearest_feature(uta_points_clean, tdm_centroids),]) %>%
#  mutate(dist = st_distance(midlinep, geometry, by_element = T))###

#uta_snapped <- joined_sf %>%
#  as.tibble() %>% select(-geometry) %>% st_as_sf()

routes <- list()
periodType = "pk"
last_route <- 109
for(i in 1:last_route){
  tdm_centroids_route <- tdm_centroids %>% filter(LabelNum == i)
  uta_route <- uta_points_clean %>% filter(LabelNum == i, PkOk == periodType)
  joined_sf <- uta_route %>% 
    cbind(tdm_centroids_route[st_nearest_feature(uta_route, tdm_centroids_route),]) %>%
    mutate(dist = ifelse(is.na(LabelNum.1),NA,st_distance(midlinep, geometry, by_element = T))) %>%
    as.tibble() %>% select(-geometry) %>% st_as_sf()
  routes[[i]] <- joined_sf
}

uta_on_tdm <- bind_rows(routes) %>%
  select(-LabelNum.1,-Label.1)

centroid_speeds <- uta_on_tdm %>% as.tibble() %>%
  group_by(Label,DIR,centroid_id) %>%
  arrange(Label,DIR,centroid_id) %>%
  #before averaging, filter out stops far from tdm network (buffer?)
  mutate(Avgmph_C = mean(Avgmph),
         Avgmphdwell_C = mean(Avgmphdwell)
         ) %>%
  filter(!is.na(centroid_id)) %>%
  select(centroid_id,LabelNum,Label,DIR,PkOk,STOP,STOP2,Avgmph_C,Avgmphdwell_C) %>%
  unique()
  
segment_speeds <- tdm_centroids_full %>%
  left_join((centroid_speeds %>% filter(DIR == 0)), by = c("centroid_id","LabelNum","Label"))



# MAPVIEW ----------------------------------------------------------------------#
labels <- c("919","M806_EglMtn","O616","S002X","M821_Psn", "BRT3500S", "FD605")

selected_tdm_lines <- tdm_segment_gis %>% filter(Label %in% labels)
selected_tdm_nodes <- tdm_point_gis %>% filter(Label %in% labels)
selected_tdm_centroids <- tdm_centroids %>% filter(Label %in% labels)
selected_uta_stops <- uta_points %>% filter(Label %in% labels)
selected_snaps <- uta_on_tdm %>% filter(Label %in% labels)



mapview(selected_tdm_lines, crs = 26912, zcol = "Label",
        col.regions = brewer.pal(8, "Dark2"),
        col = brewer.pal(8, "Dark2")) +
  mapview(selected_tdm_nodes,crs = 26912, zcol = "Label",
          col.regions = brewer.pal(8, "Dark2"),
          col = brewer.pal(8, "Dark2")) +
  mapview(selected_tdm_centroids, crs = 26912, zcol = "Label",
          col.regions = brewer.pal(8, "Set1"),
          col = brewer.pal(8, "Set1")) +
  mapview(selected_uta_stops, crs = 26912, zcol = "Label",
          col.regions = brewer.pal(8, "Set1"),
          col = brewer.pal(8, "Set1")) +
  mapview(selected_snaps, crs = 26912, zcol = "Label",
          col.regions = brewer.pal(8, "Set1"),
          col = brewer.pal(8, "Set1"))

tdm_segment_gis$geometry





ogrListLayers("data/UTA/BusRoutes_UTA.gdb")
bus_routes_uta <- st_read("data/UTA/BusRoutes_UTA.gdb") %>%
  st_as_sf()

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolylines(data = bus_routes_uta$SHAPE)










# To Connect Points with Lines: ------------------------------------------------#
uta_test <- uta_segment_data %>%
  mutate(Lon2 = lead(Lon), Lat2 = lead(Lat)) %>%
  filter(is.na(Lat2) == FALSE) %>%
  sf_pts_to_lines() %>%
  st_set_crs("+init=epsg:4326")

names(uta_test2$geometry) <- NULL
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolylines(data = uta_test$geometry)





# Functions --------------------------------------------------------------------#
make_line <- function(xy2){
  st_linestring(matrix(xy2, nrow=2, byrow=TRUE))
}

make_lines <- function(df, names=c("Lon","Lat","Lon2","Lat2")){
  m = as.matrix(df[,names])
  lines = apply(m, 1, make_line, simplify=FALSE)
  st_sfc(lines)
}

sf_pts_to_lines <- function(df, names=c("Lon","Lat","Lon2","Lat2")){
  geom = make_lines(df, names)
  df = st_sf(df, geometry=geom)
  df
}
