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
    select(centroid_id,link_id,A.x,B.x,MODE,LabelNum,Label,ONEWAY,LINKSEQ1,LINKSEQ2,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,midlinep)
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
    mutate(Avgmph_C = mean(Avgmph),
           Avgmphdwell_C = mean(Avgmphdwell)) %>%
    filter(!is.na(centroid_id)) %>%
    select(centroid_id,LabelNum,Label,DIR,PkOk,STOP,STOP2,Avgmph_C,Avgmphdwell_C) %>%
    unique()
  centroid_speeds
}

calc_centroid_speed_summary <- function(centroid_speeds){
  centroid_speeds %>%
    summarize(STOP1 = list(STOP), STOP2 = list(STOP2)) %>%
    mutate(start = as.numeric(map(STOP1,1)), end = as.numeric(map(STOP2,last))) %>%
    # determine direction uta bus is traveling relative to table direction (up the table or down the table)
    mutate(bus_direction = ifelse(start > lead(start),"up","down")) %>%
    fill(bus_direction) %>%
    # some routes have no values, so just assum a down direction
    mutate(bus_direction = ifelse(is.na(bus_direction),"down",bus_direction))
}

calc_segment_speeds <- function(centroids,centroid_speeds, centroid_speed_summary){
  centroids %>%
    left_join((centroid_speeds), by = c("centroid_id","LabelNum","Label")) %>% 
    distinct(centroid_id,LabelNum,Label,.keep_all=TRUE) %>%
    select(-STOP,-STOP2) %>%
    left_join(centroid_speed_summary, by = c("centroid_id","Label","DIR")) %>%
    mutate(STOP1 = ifelse(STOP1=="NULL",NA,STOP1), STOP2 = ifelse(STOP2=="NULL",NA,STOP2)) %>%
    fill(bus_direction) %>%
    mutate(Avgmph_C_down = Avgmph_C, Avgmph_C_up = Avgmph_C,
           Avgmphdwell_C_down = Avgmphdwell_C, Avgmphdwell_C_up = Avgmphdwell_C) %>%
    group_by(LabelNum) %>%
    fill(Avgmph_C_down, .direction = "down") %>% fill(Avgmph_C_up, .direction = "up") %>%
    fill(Avgmphdwell_C_down, .direction = "down") %>% fill(Avgmphdwell_C_up, .direction = "up")
}

estimated_segment_speeds <- function(segment_speeds, tdm_segments){
  ss <- segment_speeds %>%
    mutate(EstAvgmph = ifelse(bus_direction == "up",Avgmph_C_up,Avgmph_C_down),
           EstAvgmphdwell = ifelse(bus_direction == "up",Avgmphdwell_C_up, Avgmphdwell_C_down)) %>%
    select(-Avgmph_C,-Avgmphdwell_C,-Avgmph_C_down,-Avgmph_C_up, -Avgmphdwell_C_down, -Avgmphdwell_C_up) %>%
    fill(DIR, PkOk, EstAvgmph, EstAvgmphdwell) %>%
    mutate(EstAvgmphdwell = ifelse(EstAvgmphdwell == 0, EstAvgmph,EstAvgmphdwell)) %>%
    select(centroid_id,link_id,A.x,B.x,Label,LabelNum,MODE,ONEWAY,LINKSEQ1,LINKSEQ2,DIR,PkOk,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,EstAvgmphdwell)
  
  seg1 <- tdm_segments %>% select(link_id)
  speed1 <- ss %>% as_tibble() %>% select(-midlinep) %>% 
    group_by(LabelNum) %>%
    left_join(seg1) %>% distinct(centroid_id,LabelNum,Label,.keep_all=TRUE) %>% 
    st_as_sf()
  
  speed1 %>% 
    mutate(speedRatio = ifelse(PkOk == "pk", P_SPEED1/EstAvgmphdwell, O_SPEED1/EstAvgmphdwell)) %>%
    mutate(speedColor = ifelse(speedRatio > 1, "red", "blue"))
  
}



