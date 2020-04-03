library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf) ; library(lubridate)
library(sf)  ;  library(leaflet)  ;  library(htmltools) ; library(knitr)  ; library(feather)

source("code/segmentation_code.R")

# step 1: create your stop file
stops <- read_stops()

# step 2: pull in coded links from AGO 
gis_dat <- pull_arcgis_dat() 

# Step 3: run links through coding, nesting into segments, and then assign APC data to them

nested_data <- gis_dat %>%
  load_coded_links(stops) %>%  # step 3.1: load coded links with stop_ids
  nest_segments(stops)         # step 3.2: nest the links into segments with FINAL_IDs

FINAL_ID_LIST <- unique((as.numeric(nested_data$FINAL_ID)))

filter_segments <- function(segments, ) {
  first <- as.numeric(first(FINAL_ID_LIST))
  middle <- ceiling(length(FINAL_ID_LIST) / 2)
  last <- as.numeric(FINAL_ID_LIST)
  
  segments %>% filter(between(as.numeric(FINAL_ID), first, ))
}

# step 3.3: compile APC trip data to the segment level
segments_with_apc_dat <- nested_data %>% 
  filter(between(as.numeric(FINAL_ID), as.numeric(first(FINAL_ID_LIST)), as.numeric(FINAL_ID_LIST[10]))) %>%
  compile_apc_dat()

segments_with_apc_dat_2 <- nested_data %>% 
  filter(between(as.numeric(FINAL_ID), as.numeric(FINAL_ID_LIST[160]), as.numeric(last(FINAL_ID_LIST)))) %>%
  compile_apc_dat() 

# step 4: run analytics on each segment
segments_with_apc_analytics <- segments_with_apc_dat %>%
  bind_rows(segments_with_apc_dat_2) %>%
  add_analytics(gis_dat)

# Step 5: (optional) export to geojson
st_write(segments_with_apc_analytics, "./data/segments_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)

