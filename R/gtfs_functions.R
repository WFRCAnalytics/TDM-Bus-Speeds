



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


first_last_dir <- function(xy){
  geosphere::bearing(xy[1,1:2], xy[nrow(xy),1:2])
}

orient <- function(lines){
  pts = data.frame(sf::st_coordinates(lines))
  pts = split(pts, pts$L1)
  bearing = sapply(pts, first_last_dir)
  bearing    
}


      "