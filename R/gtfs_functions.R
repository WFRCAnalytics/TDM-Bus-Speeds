### GTFS FUNCTIONS ###

merge_trip_routes <- function(uta_gps_lines,trips,routes){
  uta_gps_lines %>% 
    left_join(trips, by = c("trip_id","shape_id")) %>%
    left_join(routes, by = c("route_id","route_type")) %>%
    # match route id with TDM/UTA route names
    group_by(trip_id) %>%
    arrange(trip_id,stop_sequence)
}


calculate_time_peak <- function(uta_gps_lines_arr){
  uta_gps_lines_arr %>%
    mutate(calcTime = ifelse(is.na(to_timestamp), NA, as_hms(difftime(lead(timestamp),timestamp)))) %>%
    mutate(cumtime2 = cumsum(calcTime)) %>%
    mutate(cumtime = ifelse(is.na(cumtime), cumtime2, cumtime)) %>%
    mutate(timepoint = ifelse(stop_sequence == 1, cumtime, cumtime - lag(cumtime)),
           startHour = as.numeric(substr(timestamp,1,2)),
           rownum = row_number()) %>%
    mutate(firstStartHour = ifelse(rownum == 1, startHour, NA)) %>%
    fill(firstStartHour, .direction = "down") %>%
    mutate(PkOk = ifelse(firstStartHour %in% c(6:8), "pk", ifelse(firstStartHour %in% c(9:14), "ok", "other")))
}

filter_lines <- function(uta_gps_lines_atr,mflines){
  uta_gps_lines_atr %>%
    filter(service_id %in% mflines) %>%
    ungroup() %>%
    group_by(route_short_name, PkOk, direction_id, stop_sequence) %>%
    filter(PkOk != "other") %>%
    arrange(stop_sequence, from_timestamp)
}

summarize_speeds <- function(uta_gps_lines_mf){
  uta_gps_lines_mf %>%
    group_by(route_short_name,PkOk,direction_id,stop_sequence) %>%
    summarize(aveTimepoint = mean(timepoint),
              aveDistance = mean(dist),
              aveSpeed = mean(speed))
}

calculate_gtfs_compass <- function(uta_gps_segments){
  uta_segments_geosphere <- uta_gps_segments %>%
    filter(!(is.na(lines)))
  uta_segments_geosphere$orient = orient(uta_segments_geosphere)
  
  uta_segments_compass_key <- uta_segments_geosphere %>%
    mutate(orient = ifelse(orient < 0, (360 + orient), orient)) %>%
    mutate(compass = ifelse(orient > 315 | orient <= 45, "NB",
                            ifelse(orient > 45 & orient <= 135, "EB",
                                   ifelse(orient > 135 & orient <= 225, "SB",
                                          ifelse(orient > 225 & orient <= 315, "WB", NA))))) %>% as_tibble() %>% select(-line) %>%
    select(uniquerow,orient,compass)
  
  uta_segments_compass <- uta_gps_segments %>% as_tibble() %>%
    left_join(uta_segments_compass_key, by = c("uniquerow")) %>%
    st_as_sf() %>%
    filter(!is.na(aveSpeed))
}

st_make_segments <- function(uta_gps_lines_df){
  uta_gps_points <- uta_gps_lines_df %>%
    group_by(route_short_name,PkOk,direction_id) %>%
    sfheaders::sf_cast("POINT") %>%
    group_by(route_short_name,PkOk,direction_id) %>%
    mutate(rownum = row_number()) %>%
    mutate(uniquerow = paste0(route_short_name,PkOk,direction_id,rownum))
  
  uta_gps_points_dups <- uta_gps_points %>%
    group_by(route_short_name,PkOk,direction_id) %>%
    slice(2:n()) %>%
    slice(1:(n()-1))
  
  uta_gps_lines_lines <- uta_gps_points %>%
    bind_rows(uta_gps_points_dups) %>%
    group_by(route_short_name,PkOk,direction_id) %>%
    arrange(rownum) %>%
    mutate(lines = ifelse(rownum == 1, 1, lag(rownum))) %>%
    group_by(route_short_name,PkOk,direction_id,lines) %>% #group_by more things
    summarize(line = st_makeline(geometry)) %>%
    group_by(route_short_name,PkOk,direction_id) %>%
    mutate(rownum = row_number()) %>%
    mutate(uniquerow = paste0(route_short_name,PkOk,direction_id,rownum)) %>%
    st_as_sf()
  
  uta_gps_segments <- uta_gps_points %>%
    as_tibble() %>% select(-geometry) %>%
    left_join(uta_gps_lines_lines, by = c("uniquerow", "route_short_name", "PkOk", "direction_id","rownum")) %>%
    st_as_sf()
}


### TDM FUNCTIONS ###
calculate_tdm_compass <- function(tdm_transit_lines){
  tdm_transit_lines2 <- tdm_transit_lines %>%
    filter(!is.na(AREATYPE)) %>%
    st_transform(4326)
  tdm_transit_lines2$orient = orient(tdm_transit_lines2)
  
  tdm_compass <- tdm_transit_lines2 %>% as_tibble() %>%
    mutate(orient = ifelse(orient < 0, (360 + orient), orient)) %>%
    mutate(compass = ifelse(orient > 315 | orient <= 45, "NB",
                            ifelse(orient > 45 & orient <= 135, "EB",
                                   ifelse(orient > 135 & orient <= 225, "SB",
                                          ifelse(orient > 225 & orient <= 315, "WB", NA))))) %>%
    as_tibble()
  
  tdm_compass
}

join_gtfs <- function(uta_compass_tdm_r,tdm_centroids_clean2){
  pkroutes <- list()
  okroutes <- list()
  compasses <- c("EB","WB","NB","SB")
  last_route <- 109
  iterations <- 1
  
  for(i in 1:last_route){
    
    for(j in 1: length(compasses)){
      tdm_filtered <- tdm_centroids_clean2 %>%
        filter(
          LabelNum == i,
          compass == compasses[j]
        )
      uta_filtered <- uta_compass_tdm_r %>%
        filter(
          LabelNum == i,
          compass == compasses[j]
        )
      
      uta_fpk <- uta_filtered %>% filter(PkOk == "pk")
      uta_fok <- uta_filtered %>% filter(PkOk == "ok")
      
      
      joined_pk <- tdm_filtered %>%
        cbind(uta_fpk[st_nearest_feature(tdm_filtered,uta_fpk),])%>%
        mutate(dist = ifelse(is.na(LabelNum.1), NA, st_distance(midlinep, line, by_element = T))) %>%
        as_tibble() %>%select(-line) %>% st_as_sf()
      joined_ok <- tdm_filtered %>%
        cbind(uta_fok[st_nearest_feature(tdm_filtered,uta_fok),])%>%
        mutate(dist = ifelse(is.na(LabelNum.1), NA, st_distance(midlinep, line, by_element = T))) %>%
        as_tibble() %>%select(-line) %>% st_as_sf()
      
      pkroutes[[iterations]] <- joined_pk
      okroutes[[iterations]] <- joined_ok
      
      iterations <- iterations + 1
    }
  }
  
  uta_tdm_pk <- bind_rows(pkroutes) %>%
    select(-LabelNum.1, -Label)
  uta_tdm_ok <- bind_rows(okroutes) %>%
    select(-LabelNum.1, -Label)
  
  uta_tdm <- bind_rows(pkroutes,okroutes) %>%
    filter(!is.na(PkOk))
}

merge_tdm_links <- function(joint_gtfs_points,tdm_compass){
  tdm_compass_small <- tdm_compass %>% select(link_id,geometry)
  joint_lines_test <- joint_gtfs_points %>% 
    as_tibble() %>% select(-midlinep) %>%
    group_by(LabelNum,Label,PkOk)%>%
    left_join(tdm_compass_small) %>% 
    distinct(centroid_id,LabelNum,Label,.keep_all=TRUE) %>% 
    st_as_sf()
}


### GLOBAL FUNCTIONS ###
first_last_dir <- function(xy){
  geosphere::bearing(xy[1,1:2], xy[nrow(xy),1:2])
}

orient <- function(lines){
  pts = data.frame(sf::st_coordinates(lines))
  pts = split(pts, pts$L1)
  bearing = sapply(pts, first_last_dir)
  bearing    
}


      