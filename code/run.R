library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf) ; library(lubridate)
library(sf)  ;  library(htmltools) ; library(knitr)  ; library(feather)  ;  library(dplyr)
library(dtplyr)  ;  library(tidyfast)  ;  library(pryr) ; library(readxl)


source("code/segmentation_code.R")

#library(googledrive)
#drive_auth(use_oob = TRUE)
#drive_download("preped_apc_data.feather", path = "./data/preped_apc_data.feather", overwrite = TRUE)

# step 1: create your stop file
stops <- read_stops()

# step 2: pull in coded links from AGO 
gis_dat <- pull_arcgis_dat()

apc_data <- import_apc() %>%
  #rowwise() %>% 
  mutate(stop_id = paste0(agency_id, stop_id))

# Step 3: run links through coding, nesting into segments, and then assign APC data to them
segments_with_stops <- gis_dat %>%
  load_coded_links(stops) %>%  # step 3.1: load coded links with stop_ids
  nest_segments(stops)         # step 3.2: nest the links into segments with FINAL_IDs

# step 3.3: compile APC trip data to the segment level
FINAL_ID_LIST <- unique((as.numeric(segments_with_stops$FINAL_ID)))
final_segments <- data.frame()


list <- c(1 : 10)

for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

list <- c(200: length(FINAL_ID_LIST))
  
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

# remove any duplicates
final_segments2 <- final_segments %>% filter(!duplicated(FINAL_ID))

# step 4: run analytics on each segment
segments_with_apc_analytics <- final_segments %>% filter(!duplicated(FINAL_ID)) %>% 
  add_analytics(gis_dat)
segments_with_apc_route_analytics <- final_segments %>% filter(!duplicated(FINAL_ID)) %>% 
  add_route_analytics(gis_dat)

# Step 5: (optional) export to geojson
st_write(segments_with_apc_analytics, "./data/segments_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(segments_with_apc_route_analytics, "./data/segments_routes_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)


# Step 6: Produce Scoring:

# Import low income data from Econsult
low_income <- read_xlsx("./data/key_equity.xlsx")
#%>% rename(c(1 =+ "route_id", 2 = "med_inc", 3 = "poverty_share", 4 = "white_share", 5 = "black_share", 6 = "hispanic_share", 7 = "noveh_share", 8 = "senior_share", 9 = "disabled_share"))

# merge with route level analytics to develop segment level equity measure
equity_scoring <- segments_with_apc_route_analytics %>% 
  left_join(low_income, by = c("route_id" = "Route Name")) %>% 
  mutate(low_inc_riders = riders_per_km * `Est. Share in Poverty`) %>% 
  group_by(FINAL_ID) %>% 
  summarise(low_inc_riders = sum(low_inc_riders, na.rm = TRUE)) %>% 
  as_tibble()

# Find the 95th percentile values
segment_stats <- segments_with_apc_analytics %>% 
  left_join(equity_scoring, by = c("FINAL_ID")) %>% 
  group_by() %>% 
  summarise(quantile = scales::percent(c(0, 0.05, 0.5, 0.75, 0.95, 1)),
            ridership = as.numeric(quantile(riders_per_km, c(0, 0.05, 0.5, 0.75, 0.95, 1))),
            low_income = quantile(low_inc_riders, c(0, 0.05, 0.5, 0.75, 0.95, 1)), 
            reliability = quantile(avg_speed_cv, c(0, 0.05, 0.5, 0.75, 0.95, 1)),
            speed = quantile(avg_speed, c(0, 0.05, 0.5, 0.75, 0.95, 1)),
            service = quantile(service_hour_km, c(0, 0.05, 0.5, 0.75, 0.95, 1)))

# helper function to normalize the values for scores over 1
normalize_value <- function(value) {
  ifelse(value >10, 10, value)
}

# normalize values
scored_segments <- segments_with_apc_analytics %>% 
  left_join(equity_scoring, by = c("FINAL_ID")) %>% 
  group_by(FINAL_ID) %>% 
  mutate(rider_norm = as.numeric(riders_per_km) / segment_stats$ridership[[5]] * 10) %>% 
  mutate(equity_norm = low_inc_riders / segment_stats$low_income[[5]] * 10) %>% 
  mutate(reliability_norm = avg_speed_cv / segment_stats$reliability[[5]] * 10) %>% 
  ungroup() %>% 
  mutate(speed_score = 10 - (percent_rank(avg_speed) * 10)) %>% # have to deal with speed actually needing higher scores for lower values
  mutate(service_norm = service_hour_km / segment_stats$service[[5]] * 10) %>% 
  mutate(across(c("rider_norm", "equity_norm", "reliability_norm", "service_norm") , normalize_value)) %>% 
  mutate(final_score = 
           (service_norm * 0.125) + 
           (reliability_norm * 0.125) + 
           (rider_norm * 0.25) + 
           (service_norm * 0.25) +
           (equity_norm * 0.25)
  ) %>% 
  left_join(segments_with_apc_analytics %>%  select("FINAL_ID", "geometry"))
  

# Step 7: Import Reuben's Analyzed Segments and Prepare GeoJSON
#scored_segments <- st_read("./data/City_Transit_segments_May_11_A/City_Transit_segments_May_11_A.shp")
scored_segments <- readxl::read_xlsx("./data/All_Segments_Scored_Spd_VC_8_June_2020.xlsx") %>% 
  select(1:14) %>% 
  mutate(speed_score = 10 - (percent_rank(avg_speed) * 10)) %>% 
  mutate(comp_score = 
           (reliability_norm_score * 0.25) + 
           (riders_norm_score * 0.25) + 
           (serfv_hr_norm_score * 0.25) +
           (low_norm_score * 0.25)
        ) %>% 
  mutate(comp_score2 = 
           (speed_score * 0.125) + 
           (reliability_norm_score * 0.125) + 
           (riders_norm_score * 0.25) + 
           (serfv_hr_norm_score * 0.25) +
           (low_norm_score * 0.25)
         ) %>% 
  left_join(segments_with_apc_analytics %>%  select("FINAL_ID", "geometry"))

st_write(scored_segments, "./data/scored_segments.geojson", driver = "GeoJSON", delete_dsn = TRUE)

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


# Step 9: Import Manually Coded Scored Segments
segments_sf <- segments_with_apc_analytics %>% select("FINAL_ID", "geometry") %>% mutate(FINAL_ID = as.character(FINAL_ID))

coded_segments <- readxl::read_xlsx("./data/scored_segments_round2.xlsx") %>% 
  filter(!is.na(name)) %>% 
  arrange(rank) %>% 
  mutate(FINAL_ID = as.character(FINAL_ID)) %>% 
  select(1:13) %>% 
  left_join(segments_sf)

st_write(coded_segments, "./data/scored_segments_round2.geojson", drive = "GeoJSON", delete_dsn = TRUE)










# RUN DVRPC DATA FOR THEM
dvrpc_segments <- read.csv("./data/segID_stopID.csv")  %>% 
  select("stop_id" = "Stop_ID", "FINAL_ID" = "SEGID") %>% 
  mutate(geometry = 0, FINAL_ID = as.character(FINAL_ID)) %>%
  filter(stop_id > 0) %>% distinct()

nested_dvrpc <- dvrpc_segments %>% 
  lazy_dt() %>% 
  group_by(FINAL_ID) %>%
  summarise(stops = list(unique(stop_id))) %>%
  #data.frame() %>% select(-geometry) %>%
  group_by(FINAL_ID) %>%
  dt_nest(FINAL_ID)

# step 3.3: compile APC trip data to the segment level
FINAL_ID_LIST <- unique((as.numeric(nested_dvrpc$FINAL_ID)))
final_DVRPC_segments <- data.frame()

list <- c(1 : 200)

for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(nested_dvrpc[val])
  final_DVRPC_segments <- rbind(final_DVRPC_segments, x)
  mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

list <- c(200: length(FINAL_ID_LIST))

for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(nested_dvrpc[val])
  final_DVRPC_segments <- rbind(final_DVRPC_segments, x)
  mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

dvrpc_segments_with_apc_analytics <- final_DVRPC_segments %>%
  add_analytics(gis_dat)

dvrpc_segments_with_apc_route_analytics <- final_DVRPC_segments %>%
  add_route_analytics(gis_dat)

st_write(dvrpc_segments_with_apc_analytics, "./data/dvrpc_output.geojson", driver = "GeoJSON", delete_dsn = TRUE)
