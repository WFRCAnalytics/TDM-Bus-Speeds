tdm_testr <-tdm_data %>%
  filter(MODE %in% c(4,5,6,9)) %>%
  # create end node and link id
  mutate(B = lead(A)) %>%
  mutate(link_id = paste0(A,"_",B)) %>%
  #select("link_id",1:8,"B",9:24,"Label","LabelNum") %>%
  # join segment data to segment spatial object
  left_join(tdm_segments,by = c("link_id")) %>%
  #filter(!is.na(A.y)) %>%
  st_as_sf()















  test1 <- pk_0_centroid_speeds %>% summarize(STOP1 = list(STOP), STOP2 = list(STOP2))%>%
    mutate(start = as.numeric(map(STOP1,1)), end = as.numeric(map(STOP2,last)))
  test2 <-  test1 %>%
    left_join(tdm_centroids_clean, by = c("centroid_id","Label")) %>%
    filter(STOP1 != "NULL") %>%
    mutate(bus_direction = ifelse(start > lead(start),"up","down")) %>%
    fill(bus_direction) %>%
    mutate(bus_direction = ifelse(is.na(bus_direction),"down",bus_direction)) %>%
    rename("LabelNum" = LabelNum.x)

  test2sum <- test2 %>%
    group_by(LabelNum,Label,bus_direction,DIR) %>%
    summarize(n = n()) %>% ungroup() %>%
    #filter(!LabelNum %in% overlaps, !LabelNum %in% badoneways) %>%
    group_by(LabelNum,Label,DIR) %>%
    slice(which.max(n))

  test3 <- pk_0_centroid_speed_summary %>%
    left_join((pk_0_centroid_speeds), by = c("centroid_id","LabelNum","Label"))%>%
    mutate(tdmDir = ifelse(finaldir == "up" & LINKSEQ1 <= lead(LINKSEQ1), 2, 1)) %>%
    mutate(tdmDir = ifelse(is.na(tdmDir), lag(tdmDir), tdmDir)) %>%
    rename("DIR" = DIR.x, "STOP2" = STOP2.x) %>% select(-LabelNum.y, -DIR.y, -STOP2.y)

overlaps <- c(12,35,36,39,43,45,88,90,93)
badoneways <- c(5,15,41,42,62,85)

  speed_summary <- bind_rows(test3,test4) %>%
    group_by(LabelNum,DIR,tdmDir) %>% summarize(n = n()) %>%
    filter(!LabelNum %in% overlaps, !LabelNum %in% badoneways)

test3clean <- test3 %>% select(centroid_id,LabelNum, Label, DIR, STOP1,STOP2, start, end, bus_direction, finaldir, tdmDir, PkOk, STOP, Avgmph, Avgmph_C, Avgmphdwell,Avgmphdwell_C)

segspeeds3 <- tdm_centroids_clean %>%
  left_join(test3clean, by = c("centroid_id", "LabelNum", "Label")) %>%
  arrange(LabelNum,LINKSEQ1) %>%
  mutate(STOP1 = ifelse(STOP1=="NULL",NA,STOP1), STOP2 = ifelse(STOP2=="NULL",NA,STOP2)) %>%
  group_by(LabelNum) %>%
  fill(finaldir) %>% fill(finaldir, .direction="up") %>% fill(finaldir, .direction="down") %>%
  fill(tdmDir) %>% fill(tdmDir, .direction = "up") %>% fill(tdmDir, .direction = "down") %>%
  fill(bus_direction) %>% mutate(bus_direction = ifelse(is.na(bus_direction), finaldir, bus_direction)) %>%
  #fill(bus_direction) %>% fill(finaldir) %>% fill(tdmDir) %>%
  mutate(Avgmph_down = Avgmph, Avgmph_up = Avgmph,
         Avgmphdwell_down = Avgmphdwell, Avgmphdwell_up = Avgmphdwell) %>%
  group_by(LabelNum) %>%
  #' fill the speeds according to the direction the uta points flow
  fill(Avgmph_down, .direction = "down") %>% fill(Avgmph_up, .direction = "up") %>%
  fill(Avgmphdwell_down, .direction = "down") %>% fill(Avgmphdwell_up, .direction = "up")

oneways <- tdm_centroids %>%
  group_by(LabelNum,Label,ONEWAY) %>%
  summarize(n = n())

ss <- segspeeds3 %>%
  #' clean data
  mutate(EstAvgmph = ifelse(finaldir == "up",Avgmph_up,Avgmph_down),
         EstAvgmphdwell = ifelse(finaldir == "up",Avgmphdwell_up, Avgmphdwell_down)) %>%
  mutate(EstAvgmph = ifelse(is.na(Avgmph), EstAvgmph, Avgmph_C),
         EstAvgmphdwell = ifelse(is.na(Avgmphdwell), EstAvgmphdwell, Avgmphdwell_C)) %>%
  #' join centroid speed summary and clean data
  distinct(centroid_id,LabelNum,Label,.keep_all=TRUE)








cs <- pk_0_centroid_speeds %>%
  # determine start and end tdm nodes per uta speed value
  summarize(STOP1 = list(STOP), STOP2 = list(STOP2)) %>%
  mutate(start = as.numeric(map(STOP1,1)), end = as.numeric(map(STOP2,last)))

centroid_dirs <- cs %>%
  left_join((tdm_centroids_clean), by = c("centroid_id","LabelNum")) %>% # CAN"T DO LABEL IF LABEL HAS CHANGED. FIX THIS!!!!!!!!!!!!!
  filter(STOP1 != "NULL") %>%
  mutate(bus_direction = ifelse(start > lead(start),"up","down")) %>%
  fill(bus_direction) %>%
  mutate(bus_direction = ifelse(is.na(bus_direction),"down",bus_direction)) %>%
  rename("Label" = Label.y) %>% select(-Label.x)

directionsummary <- centroid_dirs %>%
  group_by(LabelNum,Label,bus_direction,DIR) %>%
  summarize(n = n()) %>% ungroup() %>%
  #filter(!LabelNum %in% overlaps, !LabelNum %in% badoneways) %>%
  group_by(LabelNum,Label,DIR) %>%
  slice(which.max(n)) %>%
  mutate(finaldir = bus_direction) %>% select(-n, -bus_direction) 

centroid_dirs2 <- centroid_dirs %>% 
  arrange(LabelNum,LINKSEQ1) %>%
  left_join(directionsummary, by = c("LabelNum","Label","DIR")) %>%
  left_join((pk_0_centroid_speeds), by = c("centroid_id","LabelNum", "Label"))%>%
  mutate(tdmDir = ifelse(finaldir == "up" & LINKSEQ1 <= lead(LINKSEQ1), 2, 1)) %>%
  mutate(tdmDir = ifelse(is.na(tdmDir), lag(tdmDir), tdmDir)) %>%
  rename("DIR" = DIR.x, "STOP2" = STOP2.x) %>% select(-DIR.y, -STOP2.y) %>%
  select(centroid_id,LabelNum, Label, DIR, STOP1,STOP2, start, end, bus_direction, finaldir, tdmDir, PkOk, STOP, Avgmph, Avgmph_C, Avgmphdwell,Avgmphdwell_C)
centroid_dirs2



#' uta point onto the closest centroid value
for(i in 1:last_route){
  
  i <- 11
  periodType <- "pk"
  direction <- 0
  tdm_centroids_route <- tdm_centroids_clean %>% filter(LabelNum == i)
  tdm_centroids_route_A <- tdm_centroids_route %>% filter(grepl("_A",Label))
  tdm_centroids_route_B <- tdm_centroids_route %>% filter(grepl("_B",Label))
  uta_route <-  close_uta_points %>% filter(LabelNum == i, PkOk == periodType, DIR == direction)
  
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
  
