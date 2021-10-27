library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf) ; library(lubridate)
library(sf)  ;  library(htmltools) ; library(knitr)  ; library(feather)  ;  library(dplyr)
library(dtplyr)  ;  library(tidyfast)  ;  library(pryr) ; library(readxl)
library(editData)

library(lubridate)

source("code/segmentation_code.R")

#library(googledrive)
#drive_auth(use_oob = TRUE)
#drive_download("preped_apc_data.feather", path = "./data/preped_apc_data.feather", overwrite = TRUE)

# step 1: create your stop file
stops <- read_stops()

# step 2: pull in coded links from AGO 
gis_dat <- pull_arcgis_dat()
#st_write(gis_dat, "./data/GIS_links.geojson", drive = "GeoJSON")

apc_data <- import_apc() %>%
  #rowwise() %>% 
  mutate(stop_id = paste0(agency_id, stop_id))

# Step 3: run links through coding, nesting into segments, and then assign APC data to them
links_with_stops <- gis_dat %>%
  load_coded_links(stops)

segments_with_stops <- links_with_stops %>%  # step 3.1: load coded links with stop_ids
  nest_segments(stops)         # step 3.2: nest the links into segments with FINAL_IDs

# step 3.3: compile APC trip data to the segment level
FINAL_ID_LIST <- unique((as.numeric(segments_with_stops$FINAL_ID)))
final_segments <- data.frame()
#final_segments -> final_segments2

list <- c(1 : 50)
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}
list <- c(50 : 100)
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

list <- c(100 : 150)
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

list <- c(150 : 200)
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

list <- c(200 : 250)
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}

list <- c(250: length(FINAL_ID_LIST))
  
for(val in list) {
  print(paste("Running segment number:", FINAL_ID_LIST[val], sep = " "))
  x <- compile_apc_dat(apc_data, segments_with_stops[val])
  final_segments <- rbind(final_segments, x)
  #mem_used() #%>% paste("currently used memory", sep = " ")  %>% print()
  #paste(val, "of", length(list), "segments complete - ", round(val/length(list)*100, 2), "%", sep = " ") %>% print()
}


# step 4: run analytics on each segment
segments_with_apc_analytics <- final_segments %>% filter(!duplicated(FINAL_ID)) %>% 
  add_analytics(gis_dat)
segments_with_apc_route_analytics <- final_segments %>% filter(!duplicated(FINAL_ID)) %>% 
  add_route_analytics(gis_dat)

# Step 5: (optional) export to geojson
st_write(segments_with_apc_analytics, "./data/segments_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(segments_with_apc_route_analytics, "./data/segments_routes_analyzed.geojson", driver = "GeoJSON", delete_dsn = TRUE)

segments_with_apc_analytics <- st_read("./data/segments_analyzed.geojson")

# Step 6: Produce Scoring:

# Import low income data from Econsult
low_income <- read_xlsx("./data/key_equity.xlsx")
segment_names <- read_xlsx("./data/named_segments.xlsx") %>% mutate(FINAL_ID = as.character(FINAL_ID))

routes <- segments_with_apc_route_analytics$route_fixed %>%  unique() %>% as.tibble() %>% mutate(apc = "apc")
key_data_routes <- low_income$`Route Name` %>% as.tibble() %>%  mutate(key = "key")
routes_combo <- full_join(routes, key_data_routes)

# merge with route level analytics to develop segment level equity measure
equity_scoring <- segments_with_apc_route_analytics %>% 
  #mutate(route_fixed = fix_routes(route_id)) %>%
  left_join(low_income, by = c("route_fixed" = "Route Name")) %>% 
  mutate(low_inc_riders = riders_per_km * `Est. Share in Poverty`) %>% 
  group_by(FINAL_ID) %>% 
  summarise(low_inc_riders = sum(low_inc_riders, na.rm = TRUE)) %>% 
  as_tibble()

# Find the 95th percentile values
segment_stats <- segments_with_apc_analytics %>% 
  left_join(equity_scoring, by = c("FINAL_ID")) %>% 
  group_by() %>% 
  filter(!is.na(ridership)) %>% 
  summarise(quantile = scales::percent(c(0, 0.05, 0.5, 0.75, 0.95, 1)),
            ridership = as.numeric(quantile(riders_per_km, c(0, 0.05, 0.5, 0.75, 0.95, 1))),
            low_income = quantile(low_inc_riders, c(0, 0.05, 0.5, 0.75, 0.95, 1)), 
            reliability = quantile(avg_speed_cv, c(0, 0.05, 0.5, 0.75, 0.95, 1)),
            speed = quantile(avg_speed, c(0, 0.05, 0.5, 0.75, 0.95, 1)),
            service = quantile(service_hour_km, c(0, 0.05, 0.5, 0.75, 0.95, 1))) %>% 
  mutate(across(c("ridership", "low_income", "reliability", "speed", "service"), as.numeric))

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
  mutate(speed_norm = 10 - (percent_rank(avg_speed) * 10)) %>% # have to deal with speed actually needing higher scores for lower values
  mutate(service_norm = service_hour_km / segment_stats$service[[5]] * 10) %>% 
  mutate(across(c("rider_norm", "equity_norm", "reliability_norm", "service_norm") , as.numeric)) %>% 
  mutate(across(c("rider_norm", "equity_norm", "reliability_norm", "service_norm") , normalize_value)) %>% 
  mutate(final_score = 
           (speed_norm * 0.125) + 
           (reliability_norm * 0.125) + 
           (rider_norm * 0.25) + 
           (service_norm * 0.25) +
           (equity_norm * 0.25)
  ) %>% 
  arrange(-final_score) %>% 
  mutate(rank = row_number()) %>% 
  left_join(segments_with_apc_analytics %>%  select("FINAL_ID", "geometry")) %>% 
  left_join(segment_names)
  

# Step 7: Compare First Scoring to Second
#scored_segments <- st_read("./data/City_Transit_segments_May_11_A/City_Transit_segments_May_11_A.shp")
old_scored_segments <- readxl::read_xlsx("./data/All_Segments_Scored_Spd_VC_8_June_2020.xlsx") %>% 
  select(1:14) %>% 
  mutate(speed_score = 10 - (percent_rank(avg_speed) * 10)) %>% 
  mutate(comp_score = 
           (reliability_norm_score * 0.25) + 
           (riders_norm_score * 0.25) + 
           (serfv_hr_norm_score * 0.25) +
           (low_norm_score * 0.25)
        ) %>% 
  mutate(final_score = 
           (speed_score * 0.125) + 
           (reliability_norm_score * 0.125) + 
           (riders_norm_score * 0.25) + 
           (serfv_hr_norm_score * 0.25) +
           (low_norm_score * 0.25)
         ) %>% 
  left_join(segments_with_apc_analytics %>%  select("FINAL_ID", "geometry")) %>% 
  filter(!is.na(FINAL_ID) & !is.na(final_score)) %>% 
  arrange(-final_score) %>% 
  mutate(rank = row_number())

compare <- old_scored_segments %>%  select(FINAL_ID, final_score, rank, "rider_norm"= riders_norm_score, "service_norm" = serfv_hr_norm_score,  "equity_norm" = "low_norm_score", "speed_norm" = "speed_score", "reliability_norm" = "reliability_norm_score") %>% 
  full_join(scored_segments %>% select(FINAL_ID, final_score, rank, rider_norm,  service_norm, equity_norm, speed_norm, reliability_norm), suffix = c(".old", ".new"), by = "FINAL_ID") %>% 
  mutate(diff.score = final_score.new -final_score.old,
         diff.rank = rank.new - rank.old,
         diff.rider = rider_norm.new - rider_norm.old,
         diff.equity = equity_norm.new - equity_norm.old,
         diff.service = service_norm.new - service_norm.old,
         diff.speed = speed_norm.new - speed_norm.old,
         diff.reliability = reliability_norm.new - reliability_norm.old)

ggplot(compare, aes(x = final_score.old, y = final_score.new)) + geom_point()
ggplot(compare, aes(x = rider_norm.old, y = rider_norm.new)) + geom_point()
ggplot(compare, aes(x = equity_norm.old, y = equity_norm.new)) + geom_point()
ggplot(compare, aes(x = service_norm.old, y = service_norm.new)) + geom_point()
ggplot(compare, aes(x = speed_norm.old, y = speed_norm.new)) + geom_point()
ggplot(compare, aes(x = reliability_norm.old, y = reliability_norm.new)) + geom_point()

ggplot(compare, aes(x = diff.score)) + geom_histogram()
ggplot(compare, aes(x = diff.rank)) + geom_histogram()
ggplot(compare, aes(x = diff.rider)) + geom_histogram()
ggplot(compare, aes(x = diff.equity)) + geom_histogram()
ggplot(compare, aes(x = diff.service)) + geom_histogram()
ggplot(compare, aes(x = diff.speed)) + geom_histogram()
ggplot(compare, aes(x = diff.reliability)) + geom_histogram()

st_write(scored_segments, "./data/scored_segments.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(scored_segments, "./map_app/scored_segments.geojson", driver = "GeoJSON", delete_dsn = TRUE)

scored_segments <- st_read("./data/scored_segments.geojson") %>% as.data.frame() %>% select(-geometry)
write.csv(scored_segments, "./data/scored_segments.csv")


# Step 9: Import Manually Coded Scored Segments
segments_sf <- segments_with_apc_analytics %>% select("FINAL_ID", "geometry") %>% mutate(FINAL_ID = as.character(FINAL_ID))

# Import manual prioritization
prioritized_segments_input <- read_csv("./data/prioritized_segments_input.csv")

# Make any changes uses editData() then resave it
prioritized_segments_input <- editData(prioritized_segments_input)
write_excel_csv(prioritized_segments_input, path = "./data/prioritized_segments_input.csv")

prioritized_segments_input <- read_csv("./data/prioritized_segments_input.csv")

prioritized_segments <- prioritized_segments_input %>% 
  filter(!is.na(name)) %>% 
  arrange(rank) %>% 
  mutate(FINAL_ID = as.character(FINAL_ID)) %>% 
  select(FINAL_ID, rank, priority, name, extents, final_score, ridership, trips, rider_norm, equity_norm, service_norm, reliability_norm, speed_norm, routes_str) %>% 
  filter(!is.na(priority)) %>% 
  left_join(segments_sf) %>% 
  mutate(across(c(final_score, ridership, trips, rider_norm, equity_norm, service_norm, reliability_norm, speed_norm), as.numeric)) %>% 
  mutate(across(where(is.numeric), round, digits = 2)) %>% 
  mutate(priority = factor(priority, levels = c("1st Tier", "Direct Bus", "2nd Tier", "3rd Tier", "Coordinate", "Hold"))) %>% 
  arrange(priority, rank)

st_write(prioritized_segments, "./data/prioritized_segments.geojson", drive = "GeoJSON", delete_dsn = TRUE)


# Quick Code to Pull Timeframe Data

find_segment_analytics_bytimeframe <- function(final_segment_dat, segment_id) {
  dat <- final_segment_dat %>% filter(FINAL_ID == segment_id) %>% select(trip_dat) %>% unnest() %>% 
    mutate(trip_hour = trip_begin %>% as.character() %>% hms() %>% hour()) %>%
    mutate(timeframe = case_when(
      trip_hour <=6 ~"Early AM",
      trip_hour >=7 & trip_hour <=10 ~"AM Rush",
      trip_hour >=11 & trip_hour <=14 ~"Afternoon",
      trip_hour >=15 & trip_hour <=20 ~"PM Rush",
      trip_hour >=21 & trip_hour <=24 ~"Evening",
    )) %>% 
    mutate(timeframe = factor(timeframe, ordered = TRUE, levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening")))
  
  output <- dat %>% 
    filter(avg_speed < 40 & avg_speed > 0) %>% 
    group_by(timeframe) %>% 
    summarise(daily_trips = n(),
              daily_ridership = sum(ridership, na.rm = TRUE),
              avg_speed = mean(avg_speed, na.rm = TRUE),
              avg_runtime = mean(run, na.rm = TRUE) %>% as.duration() %>%  round(digits = 2)) %>% 
    arrange(timeframe)
  
  return(output)
}

find_segment_analytics_bytimeframe_direction <- function(final_segment_dat, segment_id) {
  dat <- final_segment_dat %>% filter(FINAL_ID == segment_id) %>% select(trip_dat) %>% unnest() %>% 
    mutate(trip_hour = trip_begin %>% as.character() %>% hms() %>% hour()) %>%
    mutate(timeframe = case_when(
      trip_hour <=6 ~"Early AM",
      trip_hour >=7 & trip_hour <=10 ~"AM Rush",
      trip_hour >=11 & trip_hour <=14 ~"Afternoon",
      trip_hour >=15 & trip_hour <=20 ~"PM Rush",
      trip_hour >=21 & trip_hour <=24 ~"Evening",
    )) %>% 
    mutate(timeframe = factor(timeframe, ordered = TRUE, levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening")))
  
  output <- dat %>% 
    filter(avg_speed < 40 & avg_speed > 0) %>% 
    group_by(timeframe, direction_id) %>% 
    summarise(daily_trips = n(),
              daily_ridership = sum(ridership, na.rm = TRUE),
              avg_speed = mean(avg_speed, na.rm = TRUE),
              avg_runtime = mean(run, na.rm = TRUE) %>% as.duration() %>%  round(digits = 2)) %>% 
    arrange(timeframe)
  
  return(output)
}

find_segment_analytics_byhour <- function(final_segment_dat, segment_id) {
  dat <- final_segment_dat %>% filter(FINAL_ID == segment_id) %>% select(trip_dat) %>% unnest() %>% 
    mutate(trip_hour = trip_begin %>% as.character() %>% hms() %>% hour()) %>%
    mutate(timeframe = case_when(
      trip_hour <=6 ~"Early AM",
      trip_hour >=7 & trip_hour <=10 ~"AM Rush",
      trip_hour >=11 & trip_hour <=14 ~"Afternoon",
      trip_hour >=15 & trip_hour <=20 ~"PM Rush",
      trip_hour >=21 & trip_hour <=24 ~"Evening",
    )) %>% 
    mutate(timeframe = factor(timeframe, ordered = TRUE, levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening")))
  
  output <- dat %>% 
    filter(avg_speed < 40 & avg_speed > 0) %>% 
    group_by(trip_hour) %>% 
    summarise(daily_trips = n(),
              daily_ridership = sum(ridership, na.rm = TRUE),
              avg_speed = mean(avg_speed, na.rm = TRUE),
              avg_runtime = mean(run, na.rm = TRUE) %>% as.duration() %>%  round(digits = 2)) %>% 
    arrange(trip_hour)
  
  return(output)
}

jfk <- find_segment_analytics_bytimeframe(final_segments, 107111) %>% mutate(name = "JFK")
arrott <- find_segment_analytics_bytimeframe(final_segments, 253111) %>% mutate(name = "Arrott")

castor_lower <- find_segment_analytics_bytimeframe_direction(final_segments, 185111) %>% mutate(name = "Castor Lower")
castor_upper <- find_segment_analytics_bytimeframe_direction(final_segments, 185112) %>% mutate(name = "Castor Upper")
castor <- rbind(castor_lower, castor_upper)


market_east <- find_segment_analytics_byhour(final_segments, 104121) %>%  mutate(name = "Market East")

seventh <- find_segment_analytics_byhour(final_segments, 118211) %>%  mutate(name = "7th Street")


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


# generate stop size standards - 
stop_sizes <- generate_queue_sizes(apc_data)

county_shp <- sf::read_sf("./data/PACounty2020_01.geojson") %>% 
  select(COUNTY_NAM)

stop_sizes_city <- stop_sizes %>% 
  filter(is.na(stop_lat) == FALSE) %>% 
  sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  sf::st_join(county_shp) %>% 
  filter(COUNTY_NAM == "PHILADELPHIA")

plot_dat <- stop_sizes_city %>% 
  mutate(PASSENGER_75 = case_when(
    ons_sum >= 75 ~ TRUE, 
    ons_sum < 75 ~ FALSE,
    TRUE ~ FALSE
  ))

ggplot(plot_dat, aes(x = max_avg_pax)) + geom_histogram() + xlim(0, 25)
ggplot(plot_dat, aes(x = ons_sum)) + geom_histogram() + xlim(0, 500) + ylim(0, 2100)

ggplot(plot_dat, aes(x = max_avg_pax, fill = PASSENGER_75)) + geom_histogram(binwidth = 1.5) + xlim(0, 15)

castor <- stop_sizes_city %>% filter(grepl('59', routes))

library(hms)

stops_ridership <- apc_data %>% 
  filter(agency_id == "SEPTA") %>% 
  group_by(stop_id, stop_lat, stop_lon) %>% 
  summarise(ons = sum(ons, na.rm = TRUE),
            offs = sum(offs, nna.rm = TRUE))
# calculate passenger wait time at a stop
pax_wait_agg 

test <- apc_data %>% #filter(route_id == 64) %>% 
  filter(is.na(stop_lat) == FALSE) %>% #remove stops without coords
  sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% #generate shape object out of lat/lon
  sf::st_join(county_shp) %>%   # join spatially to county shapefile to ID county location - should move this up...
  filter(COUNTY_NAM == "PHILADELPHIA") %>% # remove non philly stops
  mutate(time_stamp = as_hms(as.character(time_stamp))) %>% 
  group_by(stop_id, direction_id, stop_name) 

test2 <- test %>% 
  arrange(stop_id, direction_id, time_stamp) %>% 
  mutate(prev_time_stamp = lag(time_stamp)) %>% 
  mutate(headway_secs = round((time_stamp - prev_time_stamp), 2)) %>% 
  filter(headway_secs < 3600) %>%  # filter out headways over 1 hour
  mutate(agg_wait = ons * (headway_secs / 2)) %>% 
  mutate(ons_over_2 = case_when(
    ons >= 2 ~ TRUE,
    ons < 2 ~ FALSE
  ))

test3 <- test2 %>% 
  group_by(stop_id, stop_name, ons_over_2) %>% 
  summarise(daily_pax_wait = round(sum(agg_wait)),
            avg_ons = mean(ons, na.rm = TRUE),
            daily_ons = sum(ons),
            daily_trips = n()) %>% 
  arrange(-daily_pax_wait)

test4 <- test3 %>% 
  filter(daily_ons > 50) %>% 
  as.data.frame() %>% 
  select(stop_id, stop_name, ons_over_2, daily_trips) %>% 
  spread(ons_over_2, daily_trips) %>% 
  mutate(over_2 = replace_na(`TRUE`, 0),
         under_2 = replace_na(`FALSE`, 0),
         total = over_2 + under_2,
         pct_over_2 = over_2 / total) %>% 
  left_join(stops_ridership)

test5 <- test4 %>% 
  filter(pct_over_2 <= 0.1)

test6 <- test4 %>% 
  filter(pct_over_2 > 0.1)

shelter_data <- read.csv("./data/shelter_data.csv") %>% 
  mutate(stop_id = paste0("SEPTA", as.character(Stop.ID))) %>% 
  select(stop_id, Project.Planning)

microshelters <- test5 %>% 
  left_join(shelter_data, by = c("stop_id" = "stop_id")) %>% 
  filter(is.na(Project.Planning))

write_csv(microshelters, "microshelters.csv")

ggplot(pax_wait_agg, aes(x = daily_pax_wait)) + geom_histogram() + xlim(0, 250000) + ylim(0, 3000)

mean_wait = median(pax_wait_agg$daily_pax_wait)

pax_wait_agg <- pax_wait_agg %>% 
  mutate(daily_pax_wait = as.numeric(daily_pax_wait)) %>% 
  mutate(adj_wait = case_when(
    daily_pax_wait == 0 ~ 1,
    TRUE ~ daily_pax_wait
  )) %>% 
  #ungroup() %>% 
  mutate(adj_wait_hours = adj_wait / 60 / 60) %>% 
  mutate(wait_plus =  adj_wait / as.numeric(mean_wait) * 100) %>% 
  ungroup() %>% 
  mutate(wait_pct = round(percent_rank(adj_wait), 3)) %>% 
  mutate(ons_pct = round(percent_rank(daily_ons), 3)) %>% 
  mutate(diff = wait_pct - ons_pct) %>% 
  mutate(target = case_when(
    daily_ons >= 100 & wait_pct >= 0.75 ~ "already_targeted",
    daily_ons >= 100 & wait_pct <= 0.75 ~ "reconsider", 
    daily_ons > 75 & daily_ons < 100 & wait_pct >= 0.75 ~ "new_target",
    TRUE ~ "no_shelter_needed"
  ))

ggplot(pax_wait_agg, aes(x = wait_plus)) + geom_histogram()
ggplot(pax_wait_agg, aes(x = wait_plus)) + geom_histogram() + xlim(-0, 100)

ggplot(pax_wait_agg, aes(x = wait_pct, y = log(daily_ons), colour = target)) + geom_point()


leaflet(pax_wait_agg) %>% 
  addTiles() %>% 
  addCircleMarkers()

#### ANALYZING STOP CONTROLED CORRIDORS

stops_ixn <- readxl::read_excel('./data/intersection_stops.xlsx') %>% 
  mutate(stop_id = paste("SEPTA", Stop_ID, sep = "")) %>% 
  mutate(route_id = Route, 
         led_status = LED_STATUS) %>% 
  mutate(control = replace_na(STOPTYPE, "None")) %>% 
  select(stop_id, route_id, control, led_status)

stops_route_ridership <- apc_data %>% 
  filter(agency_id == "SEPTA") %>% 
  group_by(stop_id, route_id, stop_lat, stop_lon) %>% 
  summarise(ons = sum(ons, na.rm = TRUE),
            offs = sum(offs, nna.rm = TRUE))

intersection_stops <- stops_ixn %>% 
  left_join(stops_route_ridership, by = c("stop_id", "route_id")) %>% 
  group_by(stop_id, control, led_status) %>% 
  summarise(ons = sum(ons, na.rm = TRUE), offs = sum(offs, na.rm = TRUE))

segment_data <- st_read("./data/scored_segments.geojson")

segment_stop_data <- segment_data %>% 
  mutate(FINAL_ID = as.character(FINAL_ID)) %>% 
  mutate(stop_id = strsplit(as.character(stops_str), ",")) %>% 
  unnest(stop_id) %>% 
  mutate(stop_id = str_squish(stop_id)) %>% 
  filter(stop_id != "NA") %>% 
  filter(str_detect(stop_id, "^SEPTA")) %>%  #drop NJT 
  select(FINAL_ID, stop_id)

segment_control_data <- segment_stop_data %>% 
  left_join(intersection_stops %>% select(-ons, -offs)) %>% #join to control data
  group_by(FINAL_ID, control) %>% # code the control type for the corridor
  summarise(n = n()) %>% 
  spread(control, n) %>% 
  mutate(intersections = sum(`All Way`,`Signalized`, na.rm = TRUE),
         p_signalized = Signalized / intersections, 
         p_allway = `All Way` / intersections) %>% 
  mutate(n_allway = replace_na(`All Way`, 0),
         n_signal = replace_na(Signalized, 0),
         p_signalized = replace_na(p_signalized, 0),
         p_allway = replace_na(p_allway, 0)) %>% 
  select(FINAL_ID, n_signal, n_allway, n_intersections = intersections, p_signalized, p_allway)

segments <- st_read("./data/prioritized_segments.geojson")

segment_data_full <- segments %>% 
  mutate(FINAL_ID = as.character(FINAL_ID)) %>% 
  left_join(as.data.frame(segment_data, by = c("FINAL_ID")))  %>% 
  left_join(as.data.frame(segment_control_data)) 



ggplot(segment_data_full) +
  geom_histogram(aes(x = p_signalized))

ggplot(segment_data_full) +
  geom_point(aes(x = rank, y = p_signalized))
  
ggplot(segment_data_full) +
  geom_point(aes(x = avg_speed, y = p_signalized))


sampling_source <- apc_data %>% 
  group_by(route_id, source) %>% 
  summarise(n = n())

route_21_dwell <- apc_data %>% 
  filter(route_id == 21) %>% 
  group_by(stop_id, hour_bin) %>% 
  summarise(dwell_avg = mean(dwell_time, na.rm = TRUE))


stops <- apc_data %>% 
  group_by(stop_id, stop_name, agency_id, stop_lat, stop_lon) %>% 
  summarise(daily_ons = sum(ons, na.rm = TRUE), 
            daily_offs = sum(offs, na.rm=TRUE),
            average_load = mean(load, na.rm = TRUE)) %>% 
  filter(agency_id == "SEPTA") #%>% 
  #mutate(stop_id = as.numeric(str_squish(str_remove(stop_id, "SEPTA"))))

#write_csv(stops, "./data/stop_data_2019.csv")
  
  
stop_routes <- apc_data %>% 
  group_by(stop_id, agency_id, route_id) %>% 
  summarise() %>% 
  filter(agency_id == "SEPTA") %>% 
  mutate(stop_id = as.numeric(str_squish(str_remove(stop_id, "SEPTA"))))
  
#write_csv(stop_routes, "./data/stop_routes_2019.csv")

stop_names <- apc_data %>% 
  group_by(stop_id, agency_id, stop_name) %>% 
  summarise() %>% 
  filter(agency_id == "SEPTA") %>% 
  mutate(stop_id = as.numeric(str_squish(str_remove(stop_id, "SEPTA"))))

#write_csv(stop_names, "./data/stop_names_2019.csv")
