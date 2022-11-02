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

get_stop_locations <- function(uta_gtfs, gtfs_gps_points, mflines){
  trips <- uta_gtfs$trips
  routes <- uta_gtfs$routes
  
  uta_gps_points <- tar_read(gtfs_gps_points) %>%
    filter(!is.na(stop_id)) %>%
    merge_trip_routes(trips,routes)

  uta_gps_points_s <- uta_gps_points %>%
    mutate(startHour = as.numeric(substr(timestamp,1,2)),
           rownum = row_number()) %>%
    mutate(firstStartHour = ifelse(rownum == 1, startHour, NA)) %>%
    fill(firstStartHour, .direction = "down") %>%
    mutate(PkOk = ifelse(firstStartHour %in% c(6:8), "pk", ifelse(firstStartHour %in% c(9:14), "ok", "other"))) %>%
    filter(service_id %in% mflines) %>%
    ungroup() %>%
    group_by(route_short_name, PkOk, direction_id, stop_sequence) %>%
    filter(PkOk != "other") %>%
    arrange(stop_sequence, timestamp) %>%
    group_by(route_short_name,PkOk,direction_id,stop_sequence) %>%
    summarize(aveSpeed = mean(speed)) 
  
  stop_locations <- uta_gps_points_s %>% 
    ungroup() %>%
    mutate(long = unlist(map(uta_gps_points_s$geometry,1)),
           lat = unlist(map(uta_gps_points_s$geometry,2)))
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
    st_set_crs(26912) %>%
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

join_to_gtfs <- function(tdm_sum,gtfs_csum){
  pkroutes <- list()
  okroutes <- list()
  compasses <- c("EB","WB","NB","SB")
  last_route <- 109
  iterations <- 1
  
  for(i in 1:last_route){
    
    for(j in 1: length(compasses)){
      tdm_filtered <- tdm_sum %>%
        filter(
          LabelNum == i,
          compass == compasses[j]
        )
      gtfs_filtered <- gtfs_csum %>%
        filter(
          LabelNum == i,
          compass == compasses[j]
        )
      
      gtfs_fpk <- gtfs_filtered %>% filter(PkOk == "pk")
      gtfs_fok <- gtfs_filtered %>% filter(PkOk == "ok")
      
      
      joined_pk <- gtfs_fpk %>%
        cbind(tdm_filtered[st_nearest_feature(gtfs_fpk,tdm_filtered),])%>%
        mutate(dist = ifelse(is.na(LabelNum.1), NA, st_distance(midlinep, geometry, by_element = T))) %>%
        as_tibble() %>%select(-geometry) %>% st_as_sf()
      joined_ok <- gtfs_fok %>%
        cbind(tdm_filtered[st_nearest_feature(gtfs_fok,tdm_filtered),])%>%
        mutate(dist = ifelse(is.na(LabelNum.1), NA, st_distance(midlinep, geometry, by_element = T))) %>%
        as_tibble() %>%select(-geometry) %>% st_as_sf()
      
      pkroutes[[iterations]] <- joined_pk
      okroutes[[iterations]] <- joined_ok
      
      iterations <- iterations + 1
    }
  }
  
  gtfs_tdm_pk <- bind_rows(pkroutes) %>%
    select(-LabelNum.1, -Label)
  gtfs_tdm_ok <- bind_rows(okroutes) %>%
    select(-LabelNum.1, -Label)
  
  gtfs_tdm <- bind_rows(pkroutes,okroutes) %>%
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



### VISUALS ###
make_histo_df <- function(joint_gtfs_lines){
  joint_gtfs_lines %>%
    mutate(Modeled = ifelse(PkOk == "pk", P_SPEED1, O_SPEED1),
           Observed = as.numeric(aveSpeed)*0.621371) %>%
    as_tibble() %>%
    mutate(dif = Observed - Modeled) %>%
    select(LabelNum,Label,LINKSEQ1,PkOk,dif,dist,MODE,FT_2019,AREATYPE,Observed,Modeled) %>%
    pivot_longer(!c(LabelNum,Label,LINKSEQ1,PkOk,dif,dist,MODE,FT_2019,AREATYPE), names_to = "Type", values_to = "Speed") %>%
    mutate(filterout = ifelse(Type == "Modeled", FALSE, 
                              ifelse(dif > 30 & Speed > 50, TRUE, FALSE))) %>%
    mutate(filterout = ifelse(Type == "Observed" & dist > 3000, TRUE, filterout)) %>%
    filter(filterout == FALSE) %>%
    group_by(LabelNum,Label,PkOk,Type) %>%
    arrange(LabelNum,LINKSEQ1)
}

make_histo_df_2 <- function(joint_gtfs_lines){
  joint_gtfs_lines %>%
    mutate(Modeled = ifelse(PkOk == "pk", AM_SPD, MD_SPD),
           Observed = as.numeric(aveSpeed)*0.621371) %>%
    as_tibble() %>%
    mutate(dif = Observed - Modeled) %>%
    select(LabelNum,Label,LINKSEQ1,PkOk,dif,dist,MODE,FT,AREATYPE,Observed,Modeled) %>%
    pivot_longer(!c(LabelNum,Label,LINKSEQ1,PkOk,dif,dist,MODE,FT,AREATYPE), names_to = "Type", values_to = "Speed") %>%
    mutate(filterout = ifelse(Type == "Modeled", FALSE, 
                              ifelse(dif > 30 & Speed > 50, TRUE, FALSE))) %>%
    mutate(filterout = ifelse(Type == "Observed" & dist > 3000, TRUE, filterout)) %>%
    filter(filterout == FALSE) %>%
    group_by(LabelNum,Label,PkOk,Type) %>%
    arrange(LabelNum,LINKSEQ1)
}

plot_routes <- function(histo){
  routeplots <- list()
  for (i in 1:109){
    if(i == 72){next}
    
    routeMap <- histo %>%
      filter(LabelNum == i)
    
    routeplots[[i]] <- 
      ggplot(routeMap, aes(x = LINKSEQ1, y = Speed, fill = Type))+
      facet_wrap(~PkOk)+
      geom_col(alpha = .25, position = "dodge2")+     
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      geom_line(aes(x = LINKSEQ1,y = Speed, colour = Type))+
      scale_color_manual(values = c("blue","red")) +
      scale_fill_manual(values = c("blue","red")) +
      xlab("Link Sequence ID") + ylab("Speed (mph)") +
      ggtitle(paste0("Speed Comparison by Link for Route ",routeMap$LabelNum, "-",routeMap$Label)) +
      theme()+
      theme_bw()
  }
  
  for (i in 1:109){
    if(i == 72){next}
    routeMap <- histo %>%
      filter(LabelNum == i)
    
    name <- paste0(routeMap[1, , drop = FALSE]$LabelNum, "-",routeMap[1, , drop = FALSE]$Label)
    names(routeplots)[i] <- name
  }
  
  routeplotsclean <- routeplots[-c(72,101,104,105,106,107,108)]; 
  routeplotsclean
}

join_gtfs_speeds <- function(joint_gtfs_lines){
  joint_gtfs_lines %>%
    mutate(Modeled = ifelse(PkOk == "pk", P_SPEED1, O_SPEED1),
           Observed = as.numeric(aveSpeed)*0.621371) %>%
    mutate(PercentError = (Observed - Modeled)/Modeled) %>%  
    mutate(dif = Observed - Modeled) %>%
    mutate(filterout = ifelse(dif > 30 & Observed > 50, TRUE, 
                              ifelse(dist > 3000, TRUE, FALSE))) %>%
    filter(filterout == FALSE)
}

join_gtfs_speeds_2 <- function(joint_gtfs_lines){
  joint_gtfs_lines %>%
    mutate(Modeled = ifelse(PkOk == "pk", AM_SPD, MD_SPD),
           Observed = as.numeric(aveSpeed)*0.621371) %>%
    mutate(PercentError = (Observed - Modeled)/Modeled) %>%  
    mutate(dif = Observed - Modeled) %>%
    mutate(filterout = ifelse(dif > 30 & Observed > 50, TRUE, 
                              ifelse(dist > 3000, TRUE, FALSE))) %>%
    filter(filterout == FALSE)
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

### Other functions to use later?
mutate_joint_speeds2 <- function(joint_speeds, ft_grouping_col){
  joint_speeds %>%
    mutate(
      MODE_Name = case_when(
        MODE == 4 ~ "4-Local Bus",
        MODE == 5 ~ "5-Core Bus",
        MODE == 6 ~ "6-Express Bus",
        MODE == 7 ~ "7-Light Rail",
        MODE == 8 ~ "8-Commuter Rail",
        MODE == 9 ~ "9-BRT",
        TRUE ~ "None"
      ),
      FTG2 = case_when(
        FT %in% c(1,4:10) ~ 1,       # collectors & locals
        FT == 3 ~  2,                # minor arterials
        FT == 2 ~ 3,                 # principal arterials
        FT %in% c(11:19) ~ 4,        # expressways
        FT %in% c(20:26,30:40) ~ 5,  # freeways
        FT %in% c(28:29,41:42) ~ 6,  # ramps
        TRUE ~ 0 
      )) %>%
    mutate(
      FTG_Name= case_when(
        FTG2 == 1 ~ "1-Collectors & Locals",
        FTG2 == 2 ~ "2-Minor Arterials",
        FTG2 == 3 ~ "3-Principal Arterials",
        FTG2 == 4 ~ "4-Expressways",
        FTG2 == 5 ~ "5-Freeways",
        FTG2 == 6 ~ "6-Ramps",
        TRUE ~ "None"
      )
    ) %>%
    filter(FTG2 != 0) %>%
    group_by({{ft_grouping_col}}, MODE) %>%
    arrange(-Modeled) %>%
    mutate(link_seq = row_number())
}
mapPlots2 <- function(jointspeeds,func){
  ftplots <- list()
  for (f in 1:5){
    ftplots[[f]] = func(jointspeeds,f)
  }
  list("1-Collectors&Locals" = ftplots[[1]], "2-MinorArterials" = ftplots[[2]], "3-PrincipalArterials" = ftplots[[3]],
       "4-Expressways" = ftplots[[4]], "5-Freeways" = ftplots[[5]])
}
descLinePlotter2 <- function(jointspeeds){
  jointspeeds2 <- mutated_gtfs_speeds_2  %>% 
    filter(FTG == 2) %>% 
    as.tibble()  %>%
    select(link_id,link_seq,FTG,FTGCLASS,AREATYPE,Observed,Modeled)  %>% 
    pivot_longer(!c(link_id,FTG,FTGCLASS,AREATYPE,link_seq),names_to = "Type",values_to = "Speed") %>%
    arrange(link_seq) %>%  mutate(Type = factor(Type, levels = c("Observed","Modeled")))
  
  #ftTitle <- jointspeeds2$FTGCLASS[1]
  
  ggplot(jointspeeds2, aes(x = link_seq, y = Speed, fill = Type))+
    facet_wrap(~AREATYPE, nrow = 1, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_col(alpha = .25, position = "dodge2")+     
    geom_line(aes(x = link_seq,y = Speed, colour = Type))+
    scale_color_manual(values = c("red", "blue")) +
    scale_fill_manual(values = c("red", "blue")) +
    xlab("Links by Descending Modeled Speed") + ylab("Speed (mph)") +
    #ggtitle(paste0("Bus Speeds Comparison for '",ftTitle, "' by Mode")) +
    theme()+
    theme_bw()
}
      