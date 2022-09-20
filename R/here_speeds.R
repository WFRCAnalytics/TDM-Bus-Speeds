#' this script uses HERE data to assign real roadway conditions to geospatial objects
#' Within HERE, we create a route. We then export three files based on that route:
#'     1. Speeds: Speed data by link id by time of day
#'     2. Distances: The distance from the start of the geospatial route to the begining of each link id
#'     3. Route: A geospatial object of the create route
#' Using these three files, this script creates a poit at the start of each link id along the route, and assigns
#' the speed value to that point


library(geojsonsf)
library(sf)
library(lwgeom)

speeds <- read_csv("data/Road/d460.csv") %>%
  filter(source_id %in% routes$link_id, source_ref == "link") %>%
  mutate(source_id = as.numeric(source_id))
distances <- read_csv("data/Road/1 - D460 (SB) (1).csv")
geodude <- geojsonsf::geojson_sf("data/Road/1 - D460 (NB).geojson") %>%
  st_as_sf()
length <- st_length(geodude)

route1 <- c()
for (i in 1:nrow(distances)){
  disalong <- distances[i,8] %>% pull * 1609.34
  routename <- distances[i,2] %>% pull
  linkid <- distances[i,6] %>% pull
  geopoint <- st_geod_segmentize(geodude, length) %>%
    st_linesubstring(from = 0, to = (disalong/length)) %>%
    st_endpoint() %>% st_as_sf() %>%
    mutate(route = routename, link_id = linkid)
  
  route1[[i]] <- geopoint
}
routes <- do.call(bind_rows, route1)

routes2 <- left_join(routes,speeds, by = c("link_id" = "source_id")) %>%
  group_by(link_id) %>%
  summarize(meanspeed = mean(avg_speed_mph))

mapview(routes2)