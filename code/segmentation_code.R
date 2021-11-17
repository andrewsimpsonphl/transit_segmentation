# external scrips for usage.rmd

library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf) ; library(lubridate)
library(sf)  ;  library(htmltools) ; library(knitr)  ; library(feather)  ;  library(dplyr)
library(dtplyr)  ;  library(tidyfast)  ;  library(pryr) ; library(readxl)
library(editData)

## @knitr pull_data

pull_arcgis_dat <- function() {
  token = "cP17eAcEi6efVDUJKKqiJBcfahrr8h198K9ZS9iG7Tsm1z_-yvrHweuw_evqwwDvD7eZORz2mBRvldAzg68rmPBI5saK-s_F7e9sGTji17XB7X-6RvvH6S8rjXuVv-nJSAE4O_d6O85cSzimPIVyFl6HquwYXQrCg2r5kPB4hIZtzdPXdo8rWuvGkwlQUzvVFquiABcpCfxu0iL8QVGZn_7IB78Q8nnrLxnFAJ6quDDZxDA5ijL2xcrqlBzNv_Ey"
  
  path_geo_0_4000 <- paste0("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/corrected_segments/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=", token)
  path_geo_4001_8000 <- paste0("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/corrected_segments/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=4001&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=", token)
  path_geo_8001_12000 <- paste0("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/corrected_segments/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=8001&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=", token)
  path_geo_12001_13500 <- paste0("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/corrected_segments/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=12001&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=", token)
  
  
  request_geo_1 <- GET(url = path_geo_0_4000)
  request_geo_2 <- GET(url = path_geo_4001_8000)
  request_geo_3 <- GET(url = path_geo_8001_12000)
  request_geo_4 <- GET(url = path_geo_12001_13500)
  
  request_dat <- rbind(geojson_sf(content(request_geo_1)),  geojson_sf(content(request_geo_2)),
                       geojson_sf(content(request_geo_3)), geojson_sf(content(request_geo_4)))
  
  segmentation_dat <- request_dat  %>%
    mutate(FINAL_ID = paste0(BASE_ID, TYPE, TRANSIT, OTHER))
  
  return(segmentation_dat)
}


# Pull in DVRPC's stop database
read_stops <- function() {
  septa_stops <- st_read("./data/stops_shp/PA_Stops_w_Segment.shp") %>%
    mutate(combo = as.character(combo)) %>%
    #select(`StopId`, `fromnode`, `tonode`, `combo`) %>%
    as.data.frame() %>%
    select(`StopId`, `combo`) %>%
    rename(stop_id = StopId) %>% 
    rowwise() %>% 
    mutate(stop_id = paste0("SEPTA", stop_id))
  
  njt_stops <- st_read("./data/njt_coded_stops.geojson") %>%
    as.data.frame() %>%
    mutate(combo = as.character(combo)) %>%
    select(`stop_id`, `combo`) %>% 
    rowwise() %>% 
    mutate(stop_id = paste0("NJT", stop_id))
  
  stops <- full_join(septa_stops, njt_stops)
}

## @knitr load_data
# filter out segments, join stop_id to each
load_coded_links <- function(segmentation_dat, stops, joinby = c("fromto" = "combo")) {
  loaded_segments <- segmentation_dat %>%
    left_join(stops, by = joinby) %>%
    filter(BASE_ID > 0 & BASE_ID < 900) %>% # filter out BASE_IDs below 900
    filter(TYPE > 0 & TRANSIT > 0 & OTHER > 0) # filter out incomplete FINAL_IDs
  
  return(loaded_segments)
}

nest_segments <- function(coded_links, stops) {
  nested_dat <- lazy_dt(coded_links) %>%
    left_join(stops, by = c("fromto" = "combo")) %>%
    group_by(FINAL_ID) %>%
    summarise(stops = list(unique(stop_id.x))) %>%
    #data.frame() %>% select(-geometry) %>%
    group_by(FINAL_ID) %>%
    dt_nest(FINAL_ID)
  
  return(nested_dat)
}

#nested_segments <- nest_segments(coded_links, stops)

dwell_per_on = 3.8
dwell_per_off = 1.3 # dwell time for off-only
dwell_constant = 6


adjust_dwell_and_velo <- function(apc_data) {
  
  # function that estimates dwell time based on forumla we've developed
  # known limitations - there are some places with unexplained extremely high dwell time observed
  # this deal with beginning of line locations but not end of line
  # allows for use of dwell_est in place of dwell_time and extrapolatese what dwell should generally be
  # not tested on NJT stops - expect dwell to be higher there
  
  dat <- apc_data %>% 
    mutate(     #I have no idea what's going on here so I'm going to try to create a simple variable below
        dwell_time = case_when(
          agency_id == "NJT" ~ dwell_time * 60,
          TRUE ~ dwell_time),
        
      dwell_on = case_when(
        ons > 0 ~ ons * dwell_per_on + dwell_constant,
        ons <= 0 ~ 0),
      dwell_off = case_when(
        offs > 0 ~ offs * dwell_per_off + dwell_constant,
        offs <= 0 ~ 0),
      
      dwell_est = case_when(
        stop_seq == 1 ~ NA_real_,
        dwell_on >= dwell_off ~ dwell_on,
        dwell_on < dwell_off ~ dwell_off),
      
      dwell_source = case_when(
        dwell_on >= dwell_off ~ "ons",
        dwell_on < dwell_off ~ "offs"),
      
      run_minus_dwell = case_when(
        dwell_est > 0 ~ runtime - dwell_est,
        TRUE ~ runtime),
      
    )
  
  output<- dat %>% 
    mutate(velocity = case_when(
      velocity <= 0 ~ NA_real_,
      velocity == Inf ~ NA_real_,
      velocity > 60 ~ NA_real_,
      TRUE ~ as.numeric(velocity)),
      velo_minus_dwell = delta_miles / (as.numeric(run_minus_dwell) / 60 / 60)
    )
  
  return(output)
}

# output <- apc_trip_data %>% adjust_dwell_and_velo()
# test <- output %>% 
#   filter(source == 'infodev') %>% 
#   filter(stop_seq >1) %>% 
#   filter(dwell_time < 60 & dwell_est >0 & dwell_time >0 ) %>% 
#   filter(ons + offs > 0)
# 
# test_ons <- test %>% filter(dwell_source == "ons") %>% 
#   mutate(diff = dwell_est - dwell_time)
# 
# test3 <- test2 %>% group_by(route_id) %>% 
#   summarise(diff_mean = mean(diff, na.rm = TRUE))
# 
# test_offs <- test %>% filter(dwell_source == "offs") %>% 
#   mutate(diff = dwell_est - dwell_time, na.rm = TRUE)
# 
# test5 <- test4 %>% group_by(route_id) %>% 
#   summarise(diff_mean = mean(diff))
# 
# mean(test_ons$diff, na.rm = TRUE)
# mean(test_offs$diff, na.rm = TRUE)
# 
# ggplot(test, aes(x = (dwell_time), y = (dwell_est), group = dwell_source, color = dwell_source)) + geom_point() + geom_smooth(method=lm , se=FALSE)
# 
# ggplot(test_ons, aes(x= ons, y= diff)) + geom_point()

#import apc trip data
import_apc <- function() {
  #apc_trip_data <- read_feather("./data/preped_apc_data.feather")
  apc_trip_data <- read.csv("./data/combined_apc_dataset.csv") %>% 
    mutate(stop_id = paste0(agency_id, stop_id))
  
  output <- apc_trip_data %>% adjust_dwell_and_velo()
  
  return(output)
}
#apc_trip_data <- import_apc()

# Inputs: 
# nested_data - data that has had nest_segments called on it
# segments - data that has had load_segments called on it

# pull all trips that stop at a stop in a list
# returns an unested data fram of all trips that stop at one of the given stops
filter_trip_list <- function(apc_trip_data, list){
  list_a <- apc_trip_data %>%
    filter(`stop_id` %in% list ) %>%
    distinct(trip_id) %>%
    pull(1) # returns list of trips stopping at A
  
  x <- apc_trip_data %>%
    filter(`trip_id` %in% list_a) # filter if trip # is in list
  
  #print("Filtering")
  return(x)
}

# build a nested data frame of individual trips
nest_trip_data_v2 <- function(filtered_dat) {
  y <- filtered_dat %>%
    lazy_dt() %>%
    arrange(trip_id, stop_seq) %>%
    group_by(trip_id) %>%
    dt_nest(trip_id)
  
  #print("Nesting")
  return(y)
}

#nested_trip_dat = nest_trip_data_v2(filter_trip_list(apc_data, stop_list))
#nested_apc_df <- nested_trip_dat[[2]][[2]]

# calc_pass_v2 returns corridor level analytics on a trip by trip basis; used in further processing of corridor-level statistics
# magic
calc_pass_v2 <- function(nested_apc_df, list) {
  
  # arrange by stop sequence and then slice
  # to- do - seperate out the slicing into a seperate function
  x <- nested_apc_df %>%
    as_tibble() %>% 
    arrange(stop_seq) %>%
    distinct(`stop_id`, .keep_all = TRUE) %>%
    mutate(id = row_number()) %>%
    group_by(id)
  
  id_list <- x %>% as_tibble() %>% filter(stop_id %in% list) %>% select(id) %>% as.list() %>% unlist()
    
  #x$id[x$`stop_id` %in% list]
  
  # filter to stops that occur between the first and last stop in the list
  y <- x %>%
    filter(between(id, first(id_list), last(id_list))) %>%
    ungroup()
  
  #calc. run times and passenger activity for each trip
  output <- y %>%
    lazy_dt(immutable = TRUE) %>%
    mutate(entry_load = first(load)) %>%
    group_by(route_id, direction_id, pattern_id, source) %>%
    summarise(
      stop_list = list(unique(stop_id)), 
      run = as.duration(last(hms(time_stamp)) - first(hms(time_stamp))),
      trip_begin = (first((time_stamp))),
      trip_end = (last((time_stamp))),
      distance_traveled = sum(delta_miles, na.rm = TRUE),
      n_stops = n(),
      avg_stop_spacing_ft = sum(delta_miles, na.rm = TRUE) / n() * 5280,
      dwell_sum = as.duration(sum(dwell_time, na.rm = TRUE)), # observed dwell time (infodev routes)
      dwell_est = as.duration(sum(dwell_est, na.rm = TRUE)), # new estimated dwell value
      ons_total = sum(ons),
      offs_total = sum(offs),
      max_entry_load = max(entry_load),
      avg_speed = mean(velocity, na.rm = TRUE),
      avg_load = mean(load, na.rm = TRUE),
      max_load = max(load)) %>%
    as.data.frame() %>%
    mutate(ons_offs = ons_total + offs_total) %>%
    mutate(ridership = max_entry_load + ons_total) %>%
    mutate(avg_speed = na_if(avg_speed, Inf))
  
  return(output)
}

# run calc_pass() on each nested set of data to calculate ridership (load + boards on segment)
run_passenger_data_v2 <- function(nested_trip_dat, stop_list) {
  output <- nested_trip_dat %>%
    lazy_dt(immutable = FALSE) %>%
    mutate(calculated_pass = map(data, calc_pass_v2, list = stop_list)) %>%
    as.data.frame()
  
  return(output)
}
#test <- run_passenger_data_v2(nested_trip_dat, list)

run_passenger_data_v3 <- function(nested_trip_dat, stop_list) {
  output <- nested_trip_dat %>% mutate(calculated_pass = map(data, calc_pass_v2, list = stop_list))
  
  return(output)
}

# function that returns a dataframe of APC trip data for a list of stops
find_trip_dat_v2 <- function(apc_trip_data, stop_list) {
  
  filtered_dat <- filter_trip_list(apc_trip_data, stop_list)
  nested_trip_dat <- nest_trip_data_v2(filtered_dat)
  print(paste("Running passenger data for", count(nested_trip_dat %>% as_tibble()), "trips", sep = " "))
  
  final_dat <- lazy_dt(run_passenger_data_v3(nested_trip_dat, stop_list)) %>%
    ungroup()  %>%
    lazy_dt() %>%
    dt_unnest(calculated_pass) %>% 
    #na_if(0) %>% # no clue why this was here
    as_tibble() %>% 
    filter(run > as.duration(0))
  
  # previously used to skip the BLVD Direct - ignore for now
  # if(final_dat$route_id == 500){
  #   # do nothing for Direct Bus
  #   return(final_dat)
  # } else{
  #   #print(final_dat$route_id)
  #   final_dat <- final_dat %>% filter(run > as.duration(0)) # filter out trips that only make 1 stop on the corridor
  #   return(final_dat)
  # }
  # 
  # gc()
  
  return(final_dat)
}






#list <- c()
#test <- find_trip_dat_v2(combined_apc_dataset, stop_list = list)

# Inputs:
# nested_data - data that has had nest_segments called on it
# segments - data that has had load_segments called on it
compile_apc_dat <- function(apc_trip_data, nested_data) {
  
  # helper function for compile_apc_dat that helps call find_trip_dat_v2
  help_find_trip_dat <- function(list) {
    find_trip_dat_v2(apc_trip_data, list)
    #print(paste("Analyzing trip", .$FINAL_ID))
    #mem_used()
  }
  
  final <- nested_data %>%
    unnest(FINAL_ID, data) %>%
    filter(is.na(stops) != TRUE) %>% # filters out segments that do not have any stops assigned.
    lazy_dt() %>%
    mutate(trip_dat = map(stops, help_find_trip_dat)) %>%
    as.data.frame()
  
  gc()
  return(final)
}


fix_routes <- function(route_list){
  
  char_vect <- c(`331` = "33S",
                 `101` = "10B",
                 `471` = "47",
                 `701` = "BSO",
                 `702` = "C",
                 `703` = "G",
                 `704` = "HRS",
                 `705` = "HXH",
                 `706` = "J",
                 `707` = "K",
                 `708` = "KLS",
                 `709` = "KSL",
                 `710` = "L",
                 `711` = "MFO",
                 `712` = "R",
                 `713` = "13B",
                 `714` = "WCS",
                 `715` = "WPA",
                 `716` = "WPS",
                 `734` = "34B",
                 `801` = "H",
                 `802` = "XH",
                 `500` = "BLVDDIR")
  
  output <- recode(route_list, !!!char_vect)
  
  #output <- list_modify(route_list, `500` = "BLVDDIR")
  
  return(output)
}

add_analytics <- function(compiled_apc_dat, gis_dat) {
  
  segments_geometry <- gis_dat %>%
    group_by(FINAL_ID) %>%
    summarise() %>%
    mutate(length = st_length(geometry))
  
  quant_num <- function(speed, level) {
    as.numeric(unlist(quantile(speed, probs=c(level), na.rm = TRUE)))
  }
  
  output <- compiled_apc_dat %>% 
    mutate(trip_dat = map(trip_dat, ~filter(.,  .$avg_speed < 40 & .$avg_speed > 0))) %>% # filter speeds between 0 and 40 b/c NJT data is weird
    left_join(segments_geometry) %>%
    mutate(ridership = map_dbl(trip_dat, ~sum(.$ridership, na.rm = TRUE))) %>%
    mutate(avg_speed = map(trip_dat, ~as.numeric(mean(.$avg_speed, na.rm = TRUE)))) %>%
    mutate(avg_speed_q10 = map(trip_dat, ~quant_num(.$avg_speed, 0.1))) %>%
    mutate(avg_speed_q50 = map(trip_dat, ~quant_num(.$avg_speed, 0.5))) %>%
    mutate(avg_speed_q90 = map(trip_dat, ~quant_num(.$avg_speed, 0.9))) %>%
    mutate(routes_list = map(trip_dat, ~unique(.$route))) %>%
    mutate(trips = map(trip_dat, nrow)) %>%
    mutate(avg_load = map(trip_dat, ~as.numeric(mean(.$avg_load, na.rm = FALSE)))) %>%
    mutate(avg_load_q10 = map(trip_dat, ~quant_num(.$avg_load, 0.1))) %>%
    mutate(avg_load_q50 = map(trip_dat, ~quant_num(.$avg_load, 0.5))) %>%
    mutate(avg_load_q90 = map(trip_dat, ~quant_num(.$avg_load, 0.9))) %>%
    mutate(service_hours = map(trip_dat, ~(sum(.$run) %>% as.numeric() / 60 / 60) %>% round(2))) %>%
    mutate(avg_speed_sd = map(trip_dat, ~sd(as.numeric(unlist(.$avg_speed) , na.rm = TRUE), na.rm = TRUE))) %>%
    mutate(ridership = na_if(ridership, 0)) %>%
    mutate(riders_per_m = ridership / length) %>%
    mutate(riders_per_km = ridership / (length / 1000)) %>%
    mutate_at(c("avg_speed", "avg_speed_q10", "avg_speed_q50", "avg_speed_q90", "avg_speed_sd",
                "avg_load", "avg_load_q50", "avg_load_q90", "avg_load_q10", "trips"), as.numeric) %>%
    mutate(avg_speed_cv = as.numeric((avg_speed_sd)) / as.numeric((avg_speed))) %>%
    rowwise() %>%
    #mutate(routes_list = fix_routes(routes_list)) %>%
    mutate(routes_str = (routes_list %>% unlist() %>% paste(collapse = ", "))) %>%
    mutate(stops_str = (stops %>% unlist() %>% paste(collapse = ", "))) %>%
    mutate(riders_per_service_hour = ridership / service_hours) %>%
    mutate(service_hour_km = service_hours / (length / 1000)) %>%
    mutate(service_hours = as.numeric(service_hours))
  
  return(output)
}

quant_num <- function(speed, level) {
  as.numeric(unlist(quantile(speed, probs=c(level), na.rm = TRUE)))
}

add_route_analytics <- function(compiled_apc_dat, gis_dat) {
  
  segments_geometry <- gis_dat %>%
    group_by(FINAL_ID) %>%
    summarise() %>%
    mutate(length = st_length(geometry))
  

  
  segments_with_apc_route_analytics <- compiled_apc_dat %>% unnest(cols = c(trip_dat)) %>%
    group_by(FINAL_ID, route_id) %>%
    summarise(daily_ridership = sum(ridership),
              avg_route_speed = mean(avg_speed, na.rm = TRUE),
              avg_speed_q50 = quant_num(avg_speed, 0.5),
              avg_speed_q10 = quant_num(avg_speed, 0.1),
              avg_speed_q90 = quant_num(avg_speed, 0.9),
              trips = n(),
              service_hours = sum(run) / 60 / 60 %>% round(2),
              riders_per_service_hour = sum(ridership) / service_hours) %>%
    mutate(route_fixed = fix_routes(route_id)) %>%
    left_join(segments_geometry) %>% 
    mutate(riders_per_km = daily_ridership / (length / 1000))
  
  return(segments_with_apc_route_analytics)
}

####        ####

analyze_segment <- function(trip_dat) {
  output <- trip_dat %>% 
    group_by() %>% 
    summarise(
      daily_ridership = round(sum(ridership, na.rm = TRUE), 0),
      trips = n(),
      routes_served = paste(unique(fix_routes(route_id)) %>% unlist(), collapse = ", "),
      service_hours = sum(run) / 60 / 60 %>% round(2),
      riders_per_hour = round(daily_ridership / service_hours, 2),
      on_off = (sum(ons_offs, na.rm = TRUE)),
      dwell_per_onoff = round(on_off / sum(dwell_est), 2),
      onoff_per_trip = round( on_off / n(), 2),
      onoff_per_tripstop = round( on_off / n() / max(n_stops), 2),
      avg_segment_speed = mean(avg_speed, na.rm = TRUE),
      #avg_adj_speed = mean(adj_speed, na.rm = TRUE), #adjusted velo = velo-dwell needs works
      avg_speed_10_pct = round(quant_num(avg_speed, 0.1), 2),
      avg_speed_25_pct = round(quant_num(avg_speed, 0.25), 2),
      avg_speed_75_pct = round(quant_num(avg_speed, 0.75), 2),
      avg_speed_90_pct = round(quant_num(avg_speed, 0.9), 2)
    )
}

analyze_segment_route <- function(trip_dat) {
  output <- trip_dat %>% 
    group_by(route_id) %>% 
    summarise(
      daily_ridership = round(sum(ridership, na.rm = TRUE), 0),
      trips = n(),
      routes_served = paste(unique(fix_routes(route_id)) %>% unlist(), collapse = ", "),
      service_hours = sum(run) / 60 / 60 %>% round(2),
      riders_per_hour = round(daily_ridership / service_hours, 2),
      on_off = (sum(ons_offs, na.rm = TRUE)),
      dwell_per_onoff = round(on_off / sum(dwell_est), 2),
      onoff_per_trip = round( on_off / n(), 2),
      onoff_per_tripstop = round( on_off / n() / max(n_stops), 2),
      avg_segment_speed = mean(avg_speed, na.rm = TRUE),
      #avg_adj_speed = mean(adj_speed, na.rm = TRUE), #adjusted velo = velo-dwell needs works
      avg_speed_10_pct = round(quant_num(avg_speed, 0.1), 2),
      avg_speed_25_pct = round(quant_num(avg_speed, 0.25), 2),
      avg_speed_75_pct = round(quant_num(avg_speed, 0.75), 2),
      avg_speed_90_pct = round(quant_num(avg_speed, 0.9), 2)
    )
}

analyze_segment_hourbin <- function(trip_dat) {
  output <- trip_dat %>% 
    mutate(trip_hour = trip_begin %>% as.character() %>% hms() %>% hour()) %>%
    mutate(timeframe = case_when(
      trip_hour <=6 ~"Early AM",
      trip_hour >=7 & trip_hour <=10 ~"AM Rush",
      trip_hour >=11 & trip_hour <=15 ~"Afternoon",
      trip_hour >=16 & trip_hour <=19 ~"PM Rush",
      trip_hour >=20 & trip_hour <=24 ~"Evening",
    )) %>% 
    mutate(timeframe = factor(timeframe, ordered = TRUE, 
                              levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening"))) %>% 
    group_by(timeframe) %>% 
    summarise(
      daily_ridership = round(sum(ridership, na.rm = TRUE), 0),
      trips = n(),
      routes_served = paste(unique(fix_routes(route_id)) %>% unlist(), collapse = ", "),
      service_hours = sum(run) / 60 / 60 %>% round(2),
      riders_per_hour = round(daily_ridership / service_hours, 2),
      on_off = (sum(ons_total, na.rm = TRUE) + sum(offs_total, na.rm = TRUE) ),
      on_off = (sum(ons_offs, na.rm = TRUE)),
      dwell_per_onoff = round(on_off / sum(dwell_est), 2),
      onoff_per_trip = round( on_off / n(), 2),
      onoff_per_tripstop = round( on_off / n() / max(n_stops), 2),
      avg_segment_speed = mean(avg_speed, na.rm = TRUE),
      #avg_adj_speed = mean(adj_speed, na.rm = TRUE), #adjusted velo = velo-dwell needs works
      avg_speed_10_pct = round(quant_num(avg_speed, 0.1), 2),
      avg_speed_25_pct = round(quant_num(avg_speed, 0.25), 2),
      avg_speed_75_pct = round(quant_num(avg_speed, 0.75), 2),
      avg_speed_90_pct = round(quant_num(avg_speed, 0.9), 2)
    )
}

analyze_segment_route_hourly <- function(trip_dat) {
  output <- trip_dat %>% 
    mutate(trip_hour = trip_begin %>% as.character() %>% hms() %>% hour()) %>%
    mutate(timeframe = case_when(
      trip_hour <=6 ~"Early AM",
      trip_hour >=7 & trip_hour <=10 ~"AM Rush",
      trip_hour >=11 & trip_hour <=14 ~"Afternoon",
      trip_hour >=15 & trip_hour <=19 ~"PM Rush",
      trip_hour >=20 & trip_hour <=24 ~"Evening",
    )) %>% 
    mutate(timeframe = factor(timeframe, ordered = TRUE, 
                              levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening"))) %>% 
    group_by(trip_hour, route_id) %>% 
    summarise(
      daily_ridership = round(sum(ridership, na.rm = TRUE), 0),
      trips = n(),
      avg_headway_min = (60 / n()),
      #routes_served = paste(unique(fix_routes(route_id)) %>% unlist(), collapse = ", "),
      service_hours = round(sum(run) / 60 / 60, 2),
      riders_per_hour = round(daily_ridership / service_hours, 2),
      on_off = (sum(ons_offs, na.rm = TRUE)),
      dwell_per_onoff = round(on_off / sum(dwell_est), 2),
      onoff_per_trip = round( on_off / n(), 2),
      onoff_per_tripstop = round( on_off / n() / max(n_stops), 2),
      avg_segment_speed = mean(avg_speed, na.rm = TRUE),
      #avg_adj_speed = mean(adj_speed, na.rm = TRUE), #adjusted velo = velo-dwell needs works
      avg_speed_10_pct = round(quant_num(avg_speed, 0.1), 2),
      avg_speed_25_pct = round(quant_num(avg_speed, 0.25), 2),
      avg_speed_75_pct = round(quant_num(avg_speed, 0.75), 2),
      avg_speed_90_pct = round(quant_num(avg_speed, 0.9), 2),
      avg_run = hms::as_hms(round(mean(run, na.rm = TRUE)))
    )
}

analyze_segment_hourly <- function(trip_dat) {
  output <- trip_dat %>% 
    mutate(trip_hour = trip_begin %>% as.character() %>% hms() %>% hour()) %>%
    # mutate(timeframe = case_when(
    #   trip_hour <=6 ~"Early AM",
    #   trip_hour >=7 & trip_hour <=10 ~"AM Rush",
    #   trip_hour >=11 & trip_hour <=14 ~"Afternoon",
    #   trip_hour >=15 & trip_hour <=20 ~"PM Rush",
    #   trip_hour >=21 & trip_hour <=24 ~"Evening",
    # )) %>% 
    # mutate(timeframe = factor(timeframe, ordered = TRUE, 
    #                           levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening"))) %>% 
    group_by(trip_hour) %>% 
    summarise(
      daily_ridership = round(sum(ridership, na.rm = TRUE), 0),
      trips = n(),
      avg_headway_min = (60 / n()),
      routes_served = paste(unique(fix_routes(route_id)) %>% unlist(), collapse = ", "),
      service_hours = round(sum(run) / 60 / 60, 2),
      riders_per_hour = round(daily_ridership / service_hours, 2),
      on_off = (sum(ons_offs, na.rm = TRUE)),
      dwell_per_onoff = round(on_off / sum(dwell_est), 2),
      onoff_per_trip = round( on_off / n(), 2),
      onoff_per_tripstop = round( on_off / n() / max(n_stops), 2),
      avg_segment_speed = mean(avg_speed, na.rm = TRUE),
      #avg_adj_speed = mean(adj_speed, na.rm = TRUE), #adjusted velo = velo-dwell needs works
      avg_speed_10_pct = round(quant_num(avg_speed, 0.1), 2),
      avg_speed_25_pct = round(quant_num(avg_speed, 0.25), 2),
      avg_speed_75_pct = round(quant_num(avg_speed, 0.75), 2),
      avg_speed_90_pct = round(quant_num(avg_speed, 0.9), 2)
    )
}

analyze_segment_route_direction_hourly <- function(trip_dat) {
  output <- trip_dat %>% 
    mutate(trip_hour = trip_begin %>% as.character() %>% hms() %>% hour()) %>%
    mutate(timeframe = case_when(
      trip_hour <=6 ~"Early AM",
      trip_hour >=7 & trip_hour <=10 ~"AM Rush",
      trip_hour >=11 & trip_hour <=14 ~"Afternoon",
      trip_hour >=15 & trip_hour <=19 ~"PM Rush",
      trip_hour >=20 & trip_hour <=24 ~"Evening",
    )) %>% 
    mutate(timeframe = factor(timeframe, ordered = TRUE, 
                              levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening"))) %>% 
    group_by(trip_hour, route_id, direction_id) %>% 
    summarise(
      daily_ridership = round(sum(ridership, na.rm = TRUE), 0),
      trips = n(),
      avg_headway_min = (60 / n()),
      #routes_served = paste(unique(fix_routes(route_id)) %>% unlist(), collapse = ", "),
      service_hours = round(sum(run) / 60 / 60, 2),
      riders_per_hour = round(daily_ridership / service_hours, 2),
      on_off = (sum(ons_offs, na.rm = TRUE)),
      dwell_per_onoff = round(on_off / sum(dwell_est), 2),
      onoff_per_trip = round( on_off / n(), 2),
      onoff_per_tripstop = round( on_off / n() / max(n_stops), 2),
      avg_segment_speed = mean(avg_speed, na.rm = TRUE),
      #avg_adj_speed = mean(adj_speed, na.rm = TRUE), #adjusted velo = velo-dwell needs works
      avg_speed_10_pct = round(quant_num(avg_speed, 0.1), 2),
      avg_speed_25_pct = round(quant_num(avg_speed, 0.25), 2),
      avg_speed_75_pct = round(quant_num(avg_speed, 0.75), 2),
      avg_speed_90_pct = round(quant_num(avg_speed, 0.9), 2),
      avg_run = hms::as_hms(round(mean(run, na.rm = TRUE)))
    )
}

# dwell_available <- apc_data %>% filter(source == "infodev") %>% 
#   mutate(actvity = ons + offs) %>% 
#   mutate(dwell_fix = case_when(
#     dwell_time > 120 ~ 0,
#     stop_seq == 1 ~ 0,
#     TRUE ~ dwell_time
#   )) %>%
#   mutate(dwell_per_onoff = dwell_fix / actvity)
# 
# avg_dwell <- dwell_available %>% 
#   group_by() %>% 
#   summarise(dwell_per_person = mean(dwell_fix, na.rm = TRUE))


find_stop_dat <- function(apc_trip_data = apc_data, stop_list) {
  filtered_dat <- filter_trip_list(apc_trip_data, stop_list) %>% adjust_velocity()
  
  output <- filtered_dat %>% filter(stop_id %in% stop_list)
  
  return(output)
}

analyze_stops_daily <- function(stop_trip_dat) {
  output <- stop_trip_dat %>% group_by(stop_id, stop_name, stop_lat, stop_lon) %>% 
    summarise(routes = paste(unlist(list(unique(route_id))), collapse = ","),
              total_trips = n(), 
              avg_headway = 60 / (n() / 24),
              total_ons = sum(ons, na.rm = TRUE), 
              total_offs = sum(offs, na.rm = TRUE), 
              avg_load = mean(load, na.rm = TRUE),
              avg_dwell = mean(dwell_time, na.rm = TRUE), 
              avg_speed = mean(velocity, na.rm = TRUE),
              avg_dwell_per_pass = round(avg_dwell / (total_ons + total_offs), 2)) %>% 
    mutate_at(c(6:11), round) %>% 
    arrange(stop_name)
}

analyze_stops_routes_daily <- function(stop_trip_dat) {
  output <- stop_trip_dat %>% group_by(stop_id, stop_name, route_id, direction_id)  %>% 
    summarise(routes = paste(unlist(list(unique(route_id))), collapse = ","),
              total_trips = n(), 
              avg_headway = 60 / (n() / 24),
              total_ons = sum(ons, na.rm = TRUE), 
              total_offs = sum(offs, na.rm = TRUE), 
              avg_load = mean(load, na.rm = TRUE),
              avg_dwell = mean(dwell_time, na.rm = TRUE), 
              avg_speed = mean(velocity, na.rm = TRUE),
              avg_dwell_per_pass = round(avg_dwell / (total_ons + total_offs), 2)) %>%     
    mutate_if(is.double, round)  
}
 
analyze_stops_routes_hourbin <- function(stop_trip_dat) {
  output <- stop_trip_dat %>% 
    mutate(trip_hour = time_stamp %>% as.character() %>% hms() %>% hour()) %>%
    mutate(timeframe = case_when(
      trip_hour <=6 ~"Early AM",
      trip_hour >=7 & trip_hour <=10 ~"AM Rush",
      trip_hour >=11 & trip_hour <=15 ~"Afternoon",
      trip_hour >=16 & trip_hour <=19 ~"PM Rush",
      trip_hour >=20 & trip_hour <=24 ~"Evening",
    )) %>% 
    mutate(hour_len = case_when(
      trip_hour <=6 ~ 6,
      trip_hour >=7 & trip_hour <=10 ~ 4,
      trip_hour >=11 & trip_hour <=15 ~5,
      trip_hour >=16 & trip_hour <=19 ~4,
      trip_hour >=20 & trip_hour <=24 ~4,
    )) %>% 
    mutate(timeframe = factor(timeframe, ordered = TRUE, 
                              levels = c("Early AM", "AM Rush", "Afternoon", "PM Rush", "Evening"))) %>% 
    group_by(stop_id, stop_name, route_id, direction_id, timeframe) %>% 
    summarise(routes = paste(unlist(list(unique(route_id))), collapse = ","),
              total_trips = n(), 
              #avg_headway = (60 / total_trips / hour_len),
              total_ons = sum(ons, na.rm = TRUE), 
              total_offs = sum(offs, na.rm = TRUE), 
              avg_load = mean(load, na.rm = TRUE),
              avg_dwell = mean(dwell_time, na.rm = TRUE), 
              avg_speed = mean(velocity, na.rm = TRUE),
              avg_dwell_per_pass = round(avg_dwell / (total_ons + total_offs), 2)) %>%     
    mutate_if(is.double, round)
}

analyze_stops_routes_hourly <- function(stop_trip_dat) {
  output <- stop_trip_dat %>% 
    mutate(hour = time_stamp %>% as.character() %>% hms() %>% hour()) %>%
    group_by(stop_id, stop_name, route_id, direction_id, hour) %>% 
    summarise(routes = paste(unlist(list(unique(route_id))), collapse = ","),
              total_trips = n(), 
              avg_headway = 60 / (n()),
              total_ons = sum(ons, na.rm = TRUE), 
              total_offs = sum(offs, na.rm = TRUE), 
              avg_load = mean(load, na.rm = TRUE),
              avg_dwell = mean(dwell_time, na.rm = TRUE),
              avg_speed = mean(velocity, na.rm = TRUE),
              avg_dwell_per_pass = round(avg_dwell / (total_ons + total_offs), 2)) %>%     
    mutate_if(is.double, round)
}

generate_queue_sizes <- function(apc_data, input_route_id) {
  dat <- apc_data %>%
    #filter(route_id == input_route_id) %>% 
    group_by(stop_id, route_id, stop_name, stop_lat, stop_lon) %>% 
    summarise(ons_sum = sum(ons),
              offs_sum = sum(offs),
              ons_per_trip = mean(ons),
              ons_max = round(quant_num(ons, 1), 2),
              offs_per_trip = mean(offs),
              offs_max = round(quant_num(offs, 1), 2),
              sq_ft_min = (ons_max + offs_max) * 12,
              sq_ft_pref = (ons_max + offs_max) * 25) %>% 
    group_by(stop_id, stop_name, stop_lat, stop_lon) %>% 
    summarise(routes = paste(unlist(list(unique(route_id))), collapse = ","),
              ons_sum = sum(ons_sum),
              offs_sum = sum(offs_sum),
              max_avg_wait = sum(ons_max),
              max_avg_total = sum(ons_max) + sum(offs_max),
              los_c = sum(sq_ft_min),
              los_a = sum(sq_ft_pref))
}




