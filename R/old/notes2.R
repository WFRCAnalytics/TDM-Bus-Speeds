pk_0_estimated_speeds <- tar_read(pk_0_estimated_speeds)
ok_0_estimated_speeds <- tar_read(ok_0_estimated_speeds)
pk_1_estimated_speeds <- tar_read(pk_1_estimated_speeds)
ok_1_estimated_speeds <- tar_read(ok_1_estimated_speeds)
pk_estimated_speeds <- tar_read(pk_estimated_speeds)
ok_estimated_speeds <- tar_read(ok_estimated_speeds)
uta_points <- tar_read(uta_map_points)

dperiod <- pk_0_estimated_speeds
finalPeriod <- "pk"
finalDir <- 0

dlabel <- 17

routeMap <- dperiod %>% filter(LabelNum ==  dlabel) %>% 
  st_as_sf %>% st_transform(4326) %>% as.tibble() %>%
  rename("Modeled" = ModelSpeed, "Observed" = EstAvgmphdwell) %>%
  mutate(Modeled = ifelse(is.na(Modeled),P_SPEED1,Modeled)) %>%
  select(LINKSEQ1,Observed,Modeled)  %>% 
  pivot_longer(!LINKSEQ1,names_to = "Type",values_to = "Speed")






  routeMap <- dperiod %>% 
    filter(LabelNum ==  dlabel) %>% 
    group_by(LabelNum,DIR) %>%
    mutate(halfseq = max(LINKSEQ1) / 2) %>%
    mutate(color = ifelse(LINKSEQ1 <= halfseq, "blue","red")) %>%
    ungroup() %>%
    #filter(LINKSEQ1 < halfseq) %>%
    st_as_sf %>% st_transform(4326)
  utaStops <- uta_points %>% 
    filter(PkOk == finalPeriod) %>% 
    filter(DIR %in% finalDir) %>% 
    filter(LabelNum == dlabel)
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(lng = utaStops$lon, lat = utaStops$lat,
                     popup = paste("Avgmphdwell: ",utaStops$Avgmphdwell, "<br>",
                                   "STOP1: ", utaStops$STOP, "<br>",
                                   "STOP2: ", utaStops$STOP2),
                     color = "red",
                     radius = 8,
                     group = "UTA Stops") %>%
    addPolylines(
      data = routeMap$geometry, 
      color = routeMap$color,
      #weight = routeMap$speedRatio*10,
      group = "TDM Links",
      popup = paste("MODE:", routeMap$MODE, "<br>",
                    "ONEWAY:", routeMap$ONEWAY, "<br>",
                    "LINKSEQ1:", routeMap$LINKSEQ1, "<br>",
                    "LINKSEQ2:", routeMap$LINKSEQ2, "<br>",
                    "P_SPEED1:", routeMap$P_SPEED1, "<br>",
                    "P_SPEED2:", routeMap$P_SPEED2, "<br>",
                    "O_SPEED1:", routeMap$O_SPEED1, "<br>",
                    "O_SPEED2:", routeMap$O_SPEED2, "<br>",
                    "EstAvgmphdwell", routeMap$EstAvgmphdwell, "<br>",
                    "speedRatio:", routeMap$speedRatio)
    ) %>%
    addLayersControl(
      overlayGroups =c("TDM Links", "UTA Stops"),
      options = layersControlOptions(collapsed=FALSE)
    )

