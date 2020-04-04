library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf) ; library(lubridate)
library(sf)  ;  library(htmltools) ; library(knitr)  ; library(feather)  ;  
library(dtplyr)  ;  library(tidyfast)

source("code/segmentation_code.R")

library(googledrive)
drive_auth(use_oob = TRUE)

drive_download("preped_apc_data.feather", path = "./data/preped_apc_data.feather", overwrite = TRUE)

# step 1: create your stop file
stops <- read_stops()

# step 2: pull in coded links from AGO 
gis_dat <- pull_arcgis_dat() 

# Step 3: run links through coding, nesting into segments, and then assign APC data to them

nested_data <- gis_dat %>%
  load_coded_links(stops) %>%  # step 3.1: load coded links with stop_ids
  nest_segments(stops)         # step 3.2: nest the links into segments with FINAL_IDs

FINAL_ID_LIST <- unique((as.numeric(nested_data$FINAL_ID)))

# step 3.3: compile APC trip data to the segment level
segments_with_apc_dat <- nested_data %>% 
  filter(between(as.numeric(FINAL_ID), as.numeric(first(FINAL_ID_LIST)), as.numeric(FINAL_ID_LIST[347]))) %>%
  compile_apc_dat()


# step 4: run analytics on each segment
segments_with_apc_analytics <- segments_with_apc_dat %>%
  add_analytics(gis_dat)

# Step 5: (optional) export to geojson
st_write(segments_with_apc_analytics, "./data/segments_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)

