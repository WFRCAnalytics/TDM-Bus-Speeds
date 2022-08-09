# Create Spatial Objects-------------------------------------------------------------#
# Read in TDM Network Shapefile
get_tdm_segments <- function(shp){
  st_read(shp) %>%
    select(A,B) %>%
    mutate(link_id = paste0(A,"_",B)) %>%
    st_as_sf()
}

# UTA Bus Stop Point Spatial Object Creation
get_uta_points <- function(uta_data){
  uta_data %>%
    st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>% 
    st_transform(26912)
}

# TDM Transit Lines Spatial Object
get_transit_lines <- function(tdm_segment_data,tdm_segments){
  tdm_segment_data %>%
    filter(MODE %in% c(4,5,6,9)) %>%
    mutate(B = lead(A)) %>%
    mutate(link_id = paste0(A,"_",B)) %>%
    select("link_id",1:8,"B",9:24,"Label","LabelNum") %>%
    left_join(tdm_segments,by = c("link_id")) %>%
    filter(!is.na(A.y)) %>%
    st_as_sf()
}

# TDM Transit Nodes Spatial Object
get_transit_nodes <- function(tdm_transit_lines){
  tdm_transit_lines %>%
    st_cast("POINT")
}
  
# TDM Centroids Spatial Object
make_centroids <- function(tdm_transit_lines) {
  tdm_transit_lines %>%
    mutate(midlinep = st_centroid(geometry)) %>%
    as.tibble() %>% select(-geometry) %>% st_as_sf() %>%
    mutate(centroid_id = row_number()) 
}


# Clean Data ------------------------------------------------------------------#
clean_uta_points <- function(uta_points){
  uta_points %>%
    group_by(ROUTE,DIR,period) %>%
    mutate(STOP2 = lead(STOP)) %>%
    filter(STOP != STOP2, !is.na(STOP2)) %>%
    mutate(PkOk = ifelse(grepl("m peak",period),"pk","ok")) %>%
    select(LabelNum,Label,DIR,period,PkOk,STOP,STOP2,Avgmph,Avgmphdwell,geometry) %>%
    ungroup()
}
  
clean_centroids <- function(tdm_centroids){
  tdm_centroids %>%
    select(centroid_id,LabelNum,Label,ONEWAY,LINKSEQ1,LINKSEQ2,midlinep)
} 




#DATA ANALYSIS ------------------------------------------------------------------------------------#
filter_far_uta_stops <- function(uta_points_clean,tdm_segment_gis,distance,last_route){
  betterstops <- list()
  for(i in 1:last_route){
    good_stops <- uta_points_clean %>% filter(LabelNum == i)
    route_buffer_zone <- tdm_segment_gis %>% filter(LabelNum == i) %>%
      st_segmentize(50) %>% st_buffer(dist = distance, enCapStyle = "ROUND")
    close_stops <- good_stops %>% st_filter(route_buffer_zone)
    betterstops[[i]] <- close_stops
  }
  
  close_uta_stops <- bind_rows(betterstops)
  close_uta_stops
}


merge_uta_tdm <- function(periodType,direction,last_route,uta_points_clean,tdm_centroids){
  routes <- list()
  for(i in 1:last_route){
    tdm_centroids_route <- tdm_centroids %>% filter(LabelNum == i)
    uta_route <- uta_points_clean %>% filter(LabelNum == i, PkOk == periodType, DIR == direction)
    joined_sf <- uta_route %>% 
      cbind(tdm_centroids_route[st_nearest_feature(uta_route, tdm_centroids_route),]) %>%
      mutate(dist = ifelse(is.na(LabelNum.1),NA,st_distance(midlinep, geometry, by_element = T))) %>%
      as.tibble() %>% select(-geometry) %>% st_as_sf()
    routes[[i]] <- joined_sf
  }
  
  uta_tdm <- bind_rows(routes) %>%
    select(-LabelNum.1,-Label.1)
  uta_tdm
}

calc_centroid_speeds <- function(uta_on_tdm){
  centroid_speeds <- uta_on_tdm %>% as.tibble() %>%
    group_by(Label,DIR,centroid_id) %>%
    arrange(Label,DIR,centroid_id) %>%
    #filter out stops far from tdm network (buffer?)
    mutate(Avgmph_C = mean(Avgmph),
           Avgmphdwell_C = mean(Avgmphdwell)) %>%
    filter(!is.na(centroid_id)) %>%
    select(centroid_id,LabelNum,Label,DIR,PkOk,STOP,STOP2,Avgmph_C,Avgmphdwell_C) %>%
    unique()
}

calc_segment_speeds <- function(centroids,centroid_speeds){
  centroids %>%
    left_join((centroid_speeds), by = c("centroid_id","LabelNum","Label"))
    #fill in missing values
}

