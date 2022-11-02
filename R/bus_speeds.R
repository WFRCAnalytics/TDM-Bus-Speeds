# Create Spatial Objects-------------------------------------------------------------#
# read in TDM Network shapefile
get_tdm_segments <- function(shp, loadednet){
  loadednet_select <- read.dbf(loadednet) %>%
    mutate(link_id = paste0(A,"_",B)) %>%
    select(link_id, AREATYPE)
  
  st_read(shp) %>%
    #create link id composed of start and end nodes
    mutate(link_id = paste0(A,"_",B)) %>%
    left_join(loadednet_select, by = c("link_id")) %>%
    select(A,B, FT_2019, AREATYPE, link_id) %>%
    st_as_sf()
}
get_tdm_segments_2 <- function(shp){
  st_read(shp) %>%
    #create link id composed of start and end nodes
    mutate(link_id = paste0(A,"_",B)) %>%
    select(A,B, FT, AREATYPE, link_id, AM_SPD, MD_SPD, PM_SPD) %>%
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
    #select("link_id",1:8,"B",9:24,"Label","LabelNum") %>%
    # join segment data to segment spatial object
    left_join(tdm_segments,by = c("link_id")) %>%
    #filter(!is.na(A.y)) %>%
    st_as_sf()
}

fix_transit_lines <- function(tdm_transit_lines){
  outnbacklinks <- c(1,2,3,5,12,15,18,19,26,27,37,41,42,43,45,62,75,77,84,85,87)
  
  tdm_transit_lines_fixed <- tdm_transit_lines %>%
    group_by(LabelNum,Label)%>%
    mutate(halfseq = max(LINKSEQ1)/2) %>%
    ungroup() %>%
    mutate(Label = case_when(
      (LabelNum %in% outnbacklinks & LINKSEQ1 <= halfseq) ~ paste0(Label,"_A"),
      (LabelNum %in% outnbacklinks & LINKSEQ1 > halfseq) ~ paste0(Label,"_B"),
      TRUE ~ Label
    ))
  
  # add compass direction for ~40% of
  tdmcompassdir <- read.dbf("data/TDM/TDMLinksDirection.dbf") %>%
    select(LINKID,Direction)
  left_join(tdm_transit_lines_fixed, tdmcompassdir, by = c("link_id" = "LINKID"))
  
  
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

make_centerpoint <- function(tdm_transit_lines) {
  tdm_transit_lines %>%
    mutate(midlinep = st_point_on_surface(geometry)) %>%
    as.tibble() %>% select(-geometry) %>% st_as_sf() %>%
    #create centroid id
    mutate(centroid_id = row_number()) 
}

#' just am routes 2x, 919, 920 (40,106,107)

# Clean Data ------------------------------------------------------------------#
#' clean the uta point dataset
clean_uta_points <- function(uta_points){
  test1 <- uta_points %>%
    group_by(ROUTE,DIR,period) %>%
    # create end node
    mutate(STOP2 = lead(STOP)) %>%
    # filter out repeat stops or empty stop locations
    filter(STOP != STOP2, !is.na(STOP2)) %>%
    # create peak / off-peak variable
    mutate(PkOk = ifelse(grepl("m peak",period),"pk",ifelse(grepl("off-peak",period),"ok","other"))) %>%
    filter(PkOk %in% c("pk","ok")) %>%
    group_by(LabelNum,Label,DIR,PkOk,STOP,STOP2) %>%
    summarise(Avgmph_pkok = mean(Avgmph), Avgmphdwell_pkok = mean(Avgmphdwell)) %>%
    rename("Avgmph" = Avgmph_pkok, "Avgmphdwell" = Avgmphdwell_pkok) %>%
    ungroup()
}

#' For routes 12, 36, 39, 43, 45, 90, and 93 the uta dataset shows stops on both sides of the road
#' In this function, we correct the dataset and split those routes to have two directions
fix_clean_uta_points <- function(uta_points_clean){
  uta_points_clean_fixed <- uta_points_clean %>%
    mutate(DIR_fix = case_when(
      (LabelNum == 12 & STOP > 10) ~ 1,
      (LabelNum == 36 & STOP > 21) ~ 1,
      (LabelNum == 39 & STOP > 20) ~ 1,
      (LabelNum == 43 & STOP > 36) ~ 1,
      (LabelNum == 45 & STOP > 35) ~ 1, 
      (LabelNum == 90) ~ case_when(
        (DIR == 0 & STOP > 38) ~ 3,
        (DIR == 1 & STOP > 30) ~ 3,
        TRUE ~ 2
      ),
      (LabelNum == 93) ~ case_when(
        (DIR == 1 & PkOk == "pk" & STOP >= 29) ~ 3,
        (DIR == 0 & PkOk == "pk" & STOP >= 51) ~ 3,
        (DIR == 0 & PkOk == "ok" & STOP >= 51) ~ 3,
        (DIR == 0 & PkOk == "ok" & STOP <= 24) ~ 3,
        TRUE ~ 2
      ),
      TRUE ~ 2
    )) %>%
    filter(DIR_fix != 3) %>%
    mutate(DIR = ifelse(DIR_fix != 2, DIR_fix, DIR)) %>%
    select(-DIR_fix)
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
    select(centroid_id,link_id,A.x,B.x,MODE,LabelNum,Label,ONEWAY,LINKSEQ1,LINKSEQ2,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,midlinep, FT_2019, AREATYPE)
} 

clean_centroids2 <- function(tdm_centroids){
  tdm_centroids %>%
    select(centroid_id,link_id,A.x,B.x,MODE,LabelNum,Label,ONEWAY,LINKSEQ1,LINKSEQ2,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,midlinep, FT_2019, AREATYPE, compass) %>%
    mutate(long = unlist(map(tdm_centroids$midlinep,1)),
           lat = unlist(map(tdm_centroids$midlinep,2)))
} 

clean_centroids3 <- function(tdm_centroids){
  tdm_centroids %>%
    select(centroid_id,link_id,A.x,B.x,MODE,LabelNum,Label,ONEWAY,LINKSEQ1,LINKSEQ2,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,midlinep, FT, AREATYPE,AM_SPD,MD_SPD, compass) %>%
    mutate(long = unlist(map(tdm_centroids$midlinep,1)),
           lat = unlist(map(tdm_centroids$midlinep,2)))
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
    tdm_centroids_route_A <- tdm_centroids_route %>% filter(grepl("_A",Label))
    tdm_centroids_route_B <- tdm_centroids_route %>% filter(grepl("_B",Label))
    uta_route <- uta_points_clean %>% filter(LabelNum == i, PkOk == periodType, DIR == direction)
    
    if(nrow(tdm_centroids_route_A) > 0){
      joined_sf_A <- uta_route %>% 
        cbind(tdm_centroids_route_A[st_nearest_feature(uta_route, tdm_centroids_route_A),]) %>%
        mutate(dist = ifelse(is.na(LabelNum.1),NA,st_distance(midlinep, geometry, by_element = T))) %>%
        as.tibble() %>% select(-geometry) %>% st_as_sf()
      joined_sf_B <- uta_route %>% 
        cbind(tdm_centroids_route_B[st_nearest_feature(uta_route, tdm_centroids_route_B),]) %>%
        mutate(dist = ifelse(is.na(LabelNum.1),NA,st_distance(midlinep, geometry, by_element = T))) %>%
        as.tibble() %>% select(-geometry) %>% st_as_sf()
      joined_sf <- rbind(joined_sf_A,joined_sf_B)
    } else{
      joined_sf <- uta_route %>% 
        cbind(tdm_centroids_route[st_nearest_feature(uta_route, tdm_centroids_route),]) %>%
        mutate(dist = ifelse(is.na(LabelNum.1),NA,st_distance(midlinep, geometry, by_element = T))) %>%
        as.tibble() %>% select(-geometry) %>% st_as_sf()
    }
    routes[[i]] <- joined_sf
  }
  
  #' combines joined uta-centroid data into one table
  uta_tdm <- bind_rows(routes) %>%
    select(-LabelNum.1, -Label) %>%  rename("Label" = Label.1)
  uta_tdm
}

#' Calculate a UTA speed for every centroid that is near a UTA stop
#'  (Since multiple uta points could correspond to the same link, we
#'  take the average of these speeds for the link)
calc_centroid_speeds <- function(uta_on_tdm){
  centroid_speeds <- uta_on_tdm %>% as.tibble() %>%
    group_by(LabelNum,Label,DIR,centroid_id) %>%
    arrange(Label,DIR,centroid_id) %>%
    #' take average if more than one stop exists
    mutate(Avgmph_C = mean(Avgmph),
           Avgmphdwell_C = mean(Avgmphdwell)) %>%
    filter(!is.na(centroid_id)) %>%
    select(centroid_id,LabelNum,Label,DIR,PkOk,STOP,STOP2,Avgmph,Avgmph_C,Avgmphdwell,Avgmphdwell_C)
  centroid_speeds
}

#' create centroid summary statistics to determine the start and 
#' end TDM nodes which correspond to the uta speeds (uta point
#' speeds span several tdm links)
calc_centroid_speed_summary <- function(centroid_speeds, centroids){
  cs <- centroid_speeds %>%
    # determine start and end tdm nodes per uta speed value
    summarize(STOP1 = list(STOP), STOP2 = list(STOP2)) %>%
    mutate(start = as.numeric(map(STOP1,1)), end = as.numeric(map(STOP2,last)))
  
  centroid_dirs <- cs %>%
    left_join((centroids), by = c("centroid_id","LabelNum")) %>% 
    filter(STOP1 != "NULL") %>%
    mutate(bus_direction = ifelse(start > lead(start),"up","down")) %>%
    fill(bus_direction) %>%
    mutate(bus_direction = ifelse(is.na(bus_direction),"down",bus_direction)) %>%
    rename("Label" = Label.y) %>% select(-Label.x)
  
  directionsummary <- centroid_dirs %>%
    group_by(LabelNum,Label,bus_direction,DIR) %>%
    summarize(n = n()) %>% ungroup() %>%
    group_by(LabelNum,Label,DIR) %>%
    slice(which.max(n)) %>%
    mutate(finaldir = bus_direction) %>% select(-n, -bus_direction) 
  
  centroid_dirs2 <- centroid_dirs %>% 
    arrange(LabelNum,LINKSEQ1) %>%
    left_join(directionsummary, by = c("LabelNum","Label","DIR")) %>%
    left_join((centroid_speeds), by = c("centroid_id","LabelNum","Label"))%>%
    mutate(tdmDir = ifelse(finaldir == "up" & LINKSEQ1 <= lead(LINKSEQ1), 2, 1)) %>%
    mutate(tdmDir = ifelse(is.na(tdmDir), lag(tdmDir), tdmDir)) %>%
    rename("DIR" = DIR.x, "STOP2" = STOP2.x) %>% select(-DIR.y, -STOP2.y) %>%
    select(centroid_id,LabelNum, Label, DIR, STOP1,STOP2, start, end, bus_direction, finaldir, tdmDir, PkOk, STOP, Avgmph, Avgmph_C, Avgmphdwell,Avgmphdwell_C)
  centroid_dirs2
}

#' calculate all the missing tdm segment speeds by assuming all 
#' empty segments in-between segments with speeds get the previous
#' speed value
calc_segment_speeds <- function(centroids, centroid_speed_summary){
  centroids %>%
    left_join(centroid_speed_summary, by = c("centroid_id", "LabelNum", "Label")) %>%
    arrange(LabelNum,LINKSEQ1) %>%
    mutate(STOP1 = ifelse(STOP1=="NULL",NA,STOP1), STOP2 = ifelse(STOP2=="NULL",NA,STOP2)) %>%
    group_by(LabelNum,Label) %>%
    #ugly fix, so maybe do join directionSummary later on to fix it
    fill(finaldir) %>% fill(finaldir, .direction="up") %>% fill(finaldir, .direction="down") %>%
    fill(tdmDir) %>% fill(tdmDir, .direction = "up") %>% fill(tdmDir, .direction = "down") %>%
    fill(DIR) %>% fill(DIR, .direction = "up") %>% fill(DIR, .direction = "down") %>%
    fill(PkOk) %>% fill(PkOk, .direction = "up") %>% fill(PkOk, .direction = "down") %>%
    fill(bus_direction) %>% 
    mutate(bus_direction = ifelse(is.na(bus_direction), finaldir, bus_direction)) %>%
    mutate(Avgmph_down = Avgmph, Avgmph_up = Avgmph, Avgmphdwell_down = Avgmphdwell, Avgmphdwell_up = Avgmphdwell) %>%
    #' fill the speeds according to the direction the uta points flow
    fill(Avgmph_down, .direction = "down") %>% fill(Avgmph_up, .direction = "up") %>%
    fill(Avgmphdwell_down, .direction = "down") %>% fill(Avgmphdwell_up, .direction = "up")
}

#' clean data, shift to spatial Line object, and calculate speed ratio
estimated_segment_speeds <- function(segment_speeds, tdm_segments){
  ss <- segment_speeds %>%
    #' clean data by filling in missing values
    mutate(EstAvgmph = ifelse(finaldir == "up",Avgmph_up,Avgmph_down),
           EstAvgmphdwell = ifelse(finaldir == "up",Avgmphdwell_up, Avgmphdwell_down)) %>%
    mutate(EstAvgmph = ifelse(is.na(Avgmph), EstAvgmph, Avgmph_C),
           EstAvgmphdwell = ifelse(is.na(Avgmphdwell), EstAvgmphdwell, Avgmphdwell_C)) %>%
    mutate(EstAvgmph = ifelse(is.na(EstAvgmph), ifelse(finaldir == "up",Avgmph_down,Avgmph_up),EstAvgmph),
          EstAvgmphdwell = ifelse(is.na(EstAvgmphdwell), ifelse(finaldir == "down",Avgmphdwell_up,Avgmphdwell_down),EstAvgmphdwell)) %>%
    distinct(centroid_id,LabelNum,Label,.keep_all=TRUE) %>%
    select(-Avgmph_C,-Avgmphdwell_C,-Avgmph_down,-Avgmph_up, -Avgmphdwell_down, -Avgmphdwell_up) %>%
    fill(DIR, PkOk) %>%
    #' use AvgSpeed when AvgDwellSpeed is 0  
    mutate(EstAvgmphdwell = ifelse(EstAvgmphdwell == 0, EstAvgmph,EstAvgmphdwell)) %>%
    select(centroid_id,link_id,A.x,B.x,Label,LabelNum,MODE,FT_2019,AREATYPE,ONEWAY,LINKSEQ1,LINKSEQ2,DIR,tdmDir,PkOk,P_SPEED1,P_SPEED2,O_SPEED1,O_SPEED2,EstAvgmphdwell)
  
  #' shift centroid spatial object to link spatial object
  #' also calculate the speed ratio
  seg1 <- tdm_segments %>% select(link_id, Direction)
  speed1 <- ss %>% as_tibble() %>% select(-midlinep) %>% 
    group_by(LabelNum,Label) %>%
    left_join(seg1) %>% distinct(centroid_id,LabelNum,Label,.keep_all=TRUE) %>% 
    st_as_sf()
    
  #fix interstate speeds by deleting any speeds that just don't make sense
  fixed_interstate <- speed1 %>%
    mutate(EstAvgmphdwell = ifelse (PkOk == "pk", ifelse(EstAvgmphdwell < 30 & P_SPEED1 > 50, NA, EstAvgmphdwell), 
                                                      ifelse(EstAvgmphdwell < 30 & O_SPEED1 > 50, NA, EstAvgmphdwell)))
  #' calculate speed ratio of TDM Modeled Speed and UTA Estimated Dwelling Speed
  fixed_interstate %>% 
    mutate(ModelSpeed = ifelse(PkOk == "pk",ifelse(tdmDir == 2 & P_SPEED2 != 0, P_SPEED2, P_SPEED1), 
                               ifelse(tdmDir == 2 & O_SPEED2 != 0, O_SPEED2, O_SPEED1))) %>%
    mutate(speedRatio = ModelSpeed / EstAvgmphdwell) %>%
    
    #'calculate percent error
    mutate(PercentError = (EstAvgmphdwell - ModelSpeed)/ModelSpeed)  
}

average_estimated_speeds <- function(speed0, speed1){
  speed1small <- speed1 %>% select(centroid_id, EstAvgmphdwell) %>% rename("EstAvgmphdwell1" = EstAvgmphdwell) %>% as.tibble() %>% select(-geometry)
  avespeed <- left_join(speed0,speed1small,by = c("centroid_id")) %>%
    mutate(EstAvgmphdwell0 = EstAvgmphdwell,
           EstAvgmphdwell = ifelse(is.na(EstAvgmphdwell),EstAvgmphdwell1,
                                   ifelse(is.na(EstAvgmphdwell1), EstAvgmphdwell,
                                   (EstAvgmphdwell + EstAvgmphdwell1) / 2))
           )
  avespeed
}
