# Mapping Layers

# The purpose of this script is to pull spatial layers that will be used
#  in report maps. These layers are not for data analysis, but will sit in the
#  basemap

# 0. Packages
library(tidyverse)
library(tidytransit)
library(tigris)
library(sf)
library(tmap)
library(magrittr)

tmap_mode("view")

# 1. Read in county shapefiles ------------------------------------------------

counties <- counties(state = c("NJ", "NY"), cb = TRUE, resolution = "500k")


# 2. Read in bus route GTFS ---------------------------------------------------
subway <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")

### subway ----

# filter to just SI
si_border <- counties %>%
  filter(NAME == "Richmond")

subway_sir <- subway %>%
  filter_feed_by_area(si_border)

sir_sf <- gtfs_as_sf(subway_sir)

sir_stopstripsroutes <- subway_sir$trips %>%
  select(route_id, trip_id, shape_id, direction_id) %>%
  group_by(route_id, direction_id) %>%
  summarise(ntrips = n_distinct(trip_id),
            trip_id = first(trip_id),
            shape_id = first(shape_id),
            direction_id = first(direction_id)) %>%
  ungroup() %>%
  # add stop_id
  left_join(select(subway_sir$stop_times, trip_id, stop_id), by = "trip_id") %>%
  # add route_short_name
  left_join(select(subway_sir$routes, route_id, route_short_name, route_long_name), by = "route_id")

## Filter spatial data and join on ID vars
sir_stops <- filter_feed_by_trips(subway_sf, pull(sir_stopstripsroutes, trip_id)) %>% 
  extract2('stops') %>%
  left_join(sir_stopstripsroutes, by = "stop_id")

sir_routes <- filter_feed_by_trips(subway_sf, pull(sir_stopstripsroutes, trip_id)) %>% 
  extract2('shapes') %>%
  #join to get useful IDs (note that this file is deduplicated first to avoid duplicating shapes)
  left_join(distinct(sir_stopstripsroutes, shape_id, .keep_all = T), by = "shape_id") %>%
  select(-trip_id, -stop_id)

# Plot routes
tm_shape(sir_routes) + 
  tm_lines(lwd = 2)


# 3. Export data

# counties
st_write(counties, "mapping-layers/counties/counties.shp", delete_dsn = T)

# SIR
st_write(sir_routes, "mapping-layers/sir/sir_routes.shp", delete_dsn = T)
st_write(sir_stops, "mapping-layers/sir/sir_stops.shp", delete_dsn = T)
