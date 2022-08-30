# Create Spatial Objects-------------------------------------------------------------#
# read in TDM Network shapefile
get_tdm_segments <- function(shp){
  st_read(shp) %>%
    select(A,B) %>%
    #create link id composed of start and end nodes
    mutate(link_id = paste0(A,"_",B)) %>%
    st_as_sf()
}

# create UTA Bus Stop point spatial object
get_uta_points <- function(uta_data){
  uta_data %>%
    st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>% 
    st_transform(26912)
}

# create the TDM transit lines spatial object
get_transit_lines <- function(tdm_segment_data,tdm_segments){
  tdm_segment_data %>%
    # select bus transit modes
    # 4 - Local Bus / 5 - BRT1 / 6 - Express BUs / 7 - Light Rail / 8 - Commuter Rail / 9 - BRT ?
    filter(MODE %in% c(4,5,6,9)) %>%
    # create end node and link id
    mutate(B = lead(A)) %>%
    mutate(link_id = paste0(A,"_",B)) %>%
    select("link_id",1:8,"B",9:24,"Label","LabelNum") %>%
    # join segment data to segment saptial object
    left_join(tdm_segments,by = c("link_id")) %>%
    filter(!is.na(A.y)) %>%
    st_as_sf()
}

# create the TDM transit nodes spatial object
get_transit_nodes <- function(tdm_transit_lines){
  tdm_transit_lines %>%
    st_cast("POINT")
}
  
# create the  TDM transit line centroids spatial object
make_centroids <- function(tdm_transit_lines) {
  tdm_transit_lines %>%
    mutate(midlinep = st_centroid(geometry)) %>%
    as.tibble() %>% select(-geometry) %>% st_as_sf() %>%
    #create centroid id
    mutate(centroid_id = row_number()) 
}


# Clean Data ------------------------------------------------------------------#
#' clean the uta point dataset
clean_uta_points <- function(uta_points){
  uta_points %>%
    group_by(ROUTE,DIR,period) %>%
    # create end node
    mutate(STOP2 = lead(STOP)) %>%
    # filter out repeat stops or empty stop locations
    filter(STOP != STOP2, !is.na(STOP2)) %>%
    # create peak / off-peak variable
    mutate(PkOk = ifelse(grepl("m peak",period),"pk","ok")) %>%
    select(LabelNum,Label,DIR,period,PkOk,STOP,STOP2,Avgmph,Avgmphdwell,geometry) %>%
    ungroup()
}

# adjust spatial attributes of uta points to map them easier
mapable_uta_points <- function(uta_points){
  uta_points %>%
    st_transform(4326) %>%
    mutate(lon = sf::st_coordinates(.)[,1],
           lat = sf::st_coordinates(.)[,2]) %>%
    as.tibble() %>% select(-geometry)
}

#' clean the centroids object by selecting wanted columns, making it
#' easier to understand and manipulate  
clean_centroids <- function(tdm_centroids){
  tdm_centroids %>%
    select(centroid_id,link_id,A.x,B.x,MODE,LabelNum,Label,ONEWAY,LINKSEQ1,LINKSEQ2,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,midlinep)
} 


#DATA ANALYSIS ------------------------------------------------------------------------------------#
#' filters out all uta stops that aren't within designated buffer area
#' of tdm transit lines
filter_far_uta_stops <- function(uta_points_clean,tdm_segment_gis,distance,last_route){
  betterstops <- list()
  #' loop through each route individually and filters out uta points 
  #' that aren't within buffer area of tdm transit lines
  for(i in 1:last_route){
    good_stops <- uta_points_clean %>% filter(LabelNum == i)
    route_buffer_zone <- tdm_segment_gis %>% filter(LabelNum == i) %>%
      st_segmentize(50) %>% st_buffer(dist = distance, enCapStyle = "ROUND")
    close_stops <- good_stops %>% st_filter(route_buffer_zone)
    betterstops[[i]] <- close_stops
  }
  
  #combines filtered uta stops into one table
  close_uta_stops <- bind_rows(betterstops)
  close_uta_stops
}

#' joins the nearest uta point value onto the corresponding 
#' centroid value
merge_uta_tdm <- function(periodType,direction,last_route,uta_points_clean,tdm_centroids){
  routes <- list()
  #' loop through all routes, and joins the data from the nearest
  #' uta point onto the closest centroid value
  for(i in 1:last_route){
    tdm_centroids_route <- tdm_centroids %>% filter(LabelNum == i)
    uta_route <- uta_points_clean %>% filter(LabelNum == i, PkOk == periodType, DIR == direction)
    joined_sf <- uta_route %>% 
      cbind(tdm_centroids_route[st_nearest_feature(uta_route, tdm_centroids_route),]) %>%
      mutate(dist = ifelse(is.na(LabelNum.1),NA,st_distance(midlinep, geometry, by_element = T))) %>%
      as.tibble() %>% select(-geometry) %>% st_as_sf()
    routes[[i]] <- joined_sf
  }
  
  #' combines joined uta-centroid data into one table
  uta_tdm <- bind_rows(routes) %>%
    select(-LabelNum.1,-Label.1)
  uta_tdm
}

#' Calculate a UTA speed for every centroid that is near a UTA stop
#'  (Since multiple uta points could correspond to the same link, we
#'  take the average of these speeds for the link)
calc_centroid_speeds <- function(uta_on_tdm){
  centroid_speeds <- uta_on_tdm %>% as.tibble() %>%
    group_by(Label,DIR,centroid_id) %>%
    arrange(Label,DIR,centroid_id) %>%
    #' take average if more than one stop exists
    mutate(Avgmph_C = mean(Avgmph),
           Avgmphdwell_C = mean(Avgmphdwell)) %>%
    filter(!is.na(centroid_id)) %>%
    select(centroid_id,LabelNum,Label,DIR,PkOk,STOP,STOP2,Avgmph_C,Avgmphdwell_C) %>%
    # remove duplicates
    unique()
  centroid_speeds
}

#' create centroid summary statistics to determine the start and 
#' end TDM nodes which correspond to the uta speeds (uta point
#' speeds span several tdm links)
calc_centroid_speed_summary <- function(centroid_speeds){
  centroid_speeds %>%
    # determine start and end tdm nodes per uta speed value
    summarize(STOP1 = list(STOP), STOP2 = list(STOP2)) %>%
    mutate(start = as.numeric(map(STOP1,1)), end = as.numeric(map(STOP2,last))) %>%
    # determine direction uta bus is traveling relative to table direction (up the table or down the table)
    mutate(bus_direction = ifelse(start > lead(start),"up","down")) %>%
    fill(bus_direction) %>%
    # some routes have no values, so just assume a down direction
    mutate(bus_direction = ifelse(is.na(bus_direction),"down",bus_direction))
}

#' calculate all the missing tdm segment speeds by assuming all 
#' empty segments in-between segments with speeds get the previous
#' speed value
calc_segment_speeds <- function(centroids,centroid_speeds, centroid_speed_summary){
  centroids %>%
    #' join centroid speed summary and clean data
    left_join((centroid_speeds), by = c("centroid_id","LabelNum","Label")) %>% 
    distinct(centroid_id,LabelNum,Label,.keep_all=TRUE) %>%
    select(-STOP,-STOP2) %>%
    left_join(centroid_speed_summary, by = c("centroid_id","Label","DIR")) %>%
    mutate(STOP1 = ifelse(STOP1=="NULL",NA,STOP1), STOP2 = ifelse(STOP2=="NULL",NA,STOP2)) %>%
    #' fill out all "in-between" links with uta bus centroid speeds 
    #' that correspond with the link location
    fill(bus_direction) %>%
    mutate(Avgmph_C_down = Avgmph_C, Avgmph_C_up = Avgmph_C,
           Avgmphdwell_C_down = Avgmphdwell_C, Avgmphdwell_C_up = Avgmphdwell_C) %>%
    group_by(LabelNum) %>%
    #' fill the speeds according to the direction the uta points flow
    fill(Avgmph_C_down, .direction = "down") %>% fill(Avgmph_C_up, .direction = "up") %>%
    fill(Avgmphdwell_C_down, .direction = "down") %>% fill(Avgmphdwell_C_up, .direction = "up")
}

#' clean data, shift to spatial Line object, and calculate speed ratio
estimated_segment_speeds <- function(segment_speeds, tdm_segments){
  ss <- segment_speeds %>%
    #' clean data
    mutate(EstAvgmph = ifelse(bus_direction == "up",Avgmph_C_up,Avgmph_C_down),
           EstAvgmphdwell = ifelse(bus_direction == "up",Avgmphdwell_C_up, Avgmphdwell_C_down)) %>%
    select(-Avgmph_C,-Avgmphdwell_C,-Avgmph_C_down,-Avgmph_C_up, -Avgmphdwell_C_down, -Avgmphdwell_C_up) %>%
    fill(DIR, PkOk, EstAvgmph, EstAvgmphdwell) %>%
    #' use AvgSpeed when AvgDwellSpeed is 0  
    mutate(EstAvgmphdwell = ifelse(EstAvgmphdwell == 0, EstAvgmph,EstAvgmphdwell)) %>%
    select(centroid_id,link_id,A.x,B.x,Label,LabelNum,MODE,ONEWAY,LINKSEQ1,LINKSEQ2,DIR,PkOk,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,EstAvgmphdwell)
  
  #' shift centroid spatial object to link spatial object
  seg1 <- tdm_segments %>% select(link_id)
  speed1 <- ss %>% as_tibble() %>% select(-midlinep) %>% 
    group_by(LabelNum) %>%
    left_join(seg1) %>% distinct(centroid_id,LabelNum,Label,.keep_all=TRUE) %>% 
    st_as_sf()
  
  # fix interstate speeds by using the TDM network speeds instead of the bus speeds (DELETE IF NOT WANTED)
  fixed_interstate <- speed1 %>%
    mutate(EstAvgmphdwell = ifelse (PkOk == "pk", ifelse(EstAvgmphdwell < 40 & P_SPEED1 > 50, P_SPEED1, EstAvgmphdwell), 
                                                  ifelse(EstAvgmphdwell < 40 & O_SPEED1 > 50, O_SPEED1, EstAvgmphdwell)))
  
  #' calculate speed ratio of TDM Modeled Speed and UTA Estimated Dwelling Speed
  fixed_interstate %>% 
    mutate(speedRatio = ifelse(PkOk == "pk", P_SPEED1/EstAvgmphdwell, O_SPEED1/EstAvgmphdwell)) %>%
    mutate(speedColor = ifelse(speedRatio > 1, "red", "blue"))
}

average_estimated_speeds <- function(speed0, speed1){
  speed1small <- speed1 %>% select(centroid_id, EstAvgmphdwell) %>% rename("EstAvgmphdwell1" = EstAvgmphdwell) %>% as.tibble() %>% select(-geometry)
  avespeed <- left_join(speed0,speed1small,by = c("centroid_id")) %>%
    mutate(EstAvgmphdwell0 = EstAvgmphdwell,
           EstAvgmphdwell = ifelse(is.na(EstAvgmphdwell),EstAvgmphdwell0,
                                   ifelse(is.na(EstAvgmphdwell1), EstAvgmphdwell,
                                   (EstAvgmphdwell + EstAvgmphdwell1) / 2))
           )
  avespeed
}





