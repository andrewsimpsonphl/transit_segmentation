library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf) ; library(lubridate)
library(sf)  ;  library(htmltools) ; library(knitr)  ; library(feather)  ;  library(dplyr)
library(dtplyr)  ;  library(tidyfast)  ;  library(pryr)

source("code/segmentation_code.R")

library(googledrive)
#drive_auth(use_oob = TRUE)
#drive_download("preped_apc_data.feather", path = "./data/preped_apc_data.feather", overwrite = TRUE)

# step 1: create your stop file
stops <- read_stops()

# step 2: pull in coded links from AGO 
gis_dat <- pull_arcgis_dat()

# Step 3: run links through coding, nesting into segments, and then assign APC data to them
nested_data <- gis_dat %>%
  load_coded_links(stops) %>%  # step 3.1: load coded links with stop_ids
  nest_segments(stops)         # step 3.2: nest the links into segments with FINAL_IDs

# step 3.3: compile APC trip data to the segment level
FINAL_ID_LIST <- unique((as.numeric(nested_data$FINAL_ID)))
list <- c(1 : length(FINAL_ID_LIST))
#list <- c(1 : 2)
final_segments <- data.frame()
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(nested_data[val])
  final_segments <- rbind(final_segments, x)
  mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

# step 4: run analytics on each segment
segments_with_apc_analytics <- final_segments %>%
  add_analytics(gis_dat)
segments_with_apc_route_analytics <- final_segments %>%
  add_route_analytics(gis_dat)

# Step 5: (optional) export to geojson
st_write(segments_with_apc_analytics, "./data/segments_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(segments_with_apc_route_analytics, "./data/segments_routes_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)


