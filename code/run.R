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
final_segments <- data.frame()


list <- c(1 : 200)

for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(nested_data[val])
  final_segments <- rbind(final_segments, x)
  mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

list <- c(200: length(FINAL_ID_LIST))
  
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

# Step 7: Import Reuben's Analyzed Segments and Prepare GeoJSON
scored_segments <- st_read("./data/City_Transit_segments_May_11_A/City_Transit_segments_May_11_A.shp")

scored_segments_output <- scored_segments %>%
  mutate(ridership_percentile = percent_rank(ridership)) %>%
  mutate(ridership_km_percentile = percent_rank(riders_per)) %>%
  mutate(score_percentile = percent_rank(M1_Raw)) %>% 
  mutate(low_inc_percentile = percent_rank(pct_low))

st_write(scored_segments_output, "./data/scored_segments.geojson", driver = "GeoJSON", delete_dsn = TRUE)

# Step 8: Produce General Statistics Figures
ggplot(scored_segments_output, aes(ridership_percentile, score_percentile, color = pct_low)) + 
  geom_point() +
  scale_colour_continuous(type = "viridis") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(scored_segments_output, aes(ridership_km_percentile, score_percentile, color = pct_low)) + 
  geom_point() +
  scale_colour_continuous(type = "viridis") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(scored_segments_output, aes(pct_low, score_percentile, color = ridership_km_percentile)) + 
  geom_point() +
  scale_colour_continuous(type = "viridis") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(scored_segments_output, aes(pct_low, score_percentile, color = ridership_km_percentile)) + 
  geom_point() +
  scale_colour_continuous(type = "viridis") +
  geom_smooth(method = "lm", se = FALSE)



