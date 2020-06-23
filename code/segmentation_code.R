# external scrips for usage.rmd

## @knitr pull_data

pull_arcgis_dat <- function() {
  token = "lOl2_707XCNDe11MqOQSYTa6RNQeK48BFQrwmebshI9dpxirD6TMp_aLjIwNV98oUV-D9gphuiATkVbpoBrukyztOUnc2BLZuLC_O8y6S9PHFcjTHCEd9EfIrRz1t39jPmD7uJ8wM_F22f5_LVgaT7qcgOmLlN9ePYmZxxcPfxrC7aurNea2tDhdneXJY3Y7lhUO5ihEJoTBhTcQF6i777MkeZkM1y0kGXiQC9DOk5Crj_gytHwUoFLZ2jJRZi32"
  
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

## @knitr compile_dat functions

#import apc trip data
import_apc <- function() {
  #apc_trip_data <- read_feather("./data/preped_apc_data.feather")
  apc_trip_data <- read.csv("./data/combined_apc_dataset.csv")
  return(apc_trip_data)
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

#nested_apc_df = nest_trip_data_v2(filter_trip_list(combined_apc_dataset, list))[10]$data[[1]]

# magic
calc_pass_v2 <- function(nested_apc_df, list) {
  
  # arrange by stop sequence and then slice
  x <- nested_apc_df %>%
    arrange(stop_seq) %>%
    distinct(`stop_id`, .keep_all = TRUE) %>%
    mutate(id = row_number()) %>%
    group_by(id)
  
  id_list <- x$id[x$`stop_id` %in% list]
  
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
      run = as.duration(last(hms(time_stamp)) - first(hms(time_stamp))),
      trip_begin = (first((time_stamp))),
      trip_end = (last((time_stamp))),
      dwell_sum = as.duration(sum(dwell_time, na.rm = TRUE)),
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
#test <- run_passenger_data_v2(df, list)

run_passenger_data_v3 <- function(nested_trip_dat, stop_list) {
  output <- nested_trip_dat %>%
    mutate(calculated_pass = map(data, calc_pass_v2, list = stop_list))
  
  return(output)
}
#test <- run_passenger_data_v3(df, list)

# function that returns a dataframe of APC trip data for a list of stops
find_trip_dat_v2 <- function(apc_trip_data, stop_list) {
  
  filtered_dat <- filter_trip_list(apc_trip_data, stop_list)
  nested_trip_dat <- nest_trip_data_v2(filtered_dat)
  print(paste("Running passenger data for", count(nested_trip_dat %>% as_tibble()), "trips", sep = " "))
  
  final_dat <- lazy_dt(run_passenger_data_v2(nested_trip_dat, stop_list)) %>%
    ungroup()  %>%
    lazy_dt() %>%
    dt_unnest(calculated_pass) %>%
    filter(run > as.duration(0)) # filter out trips that only make 1 stop on the corridor
  
  return(final_dat)
  gc()
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
                 `471` = "47M",
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
  
  output <- compiled_apc_dat %>% left_join(segments_geometry) %>%
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

add_route_analytics <- function(compiled_apc_dat, gis_dat) {
  
  segments_geometry <- gis_dat %>%
    group_by(FINAL_ID) %>%
    summarise() %>%
    mutate(length = st_length(geometry))
  
  quant_num <- function(speed, level) {
    as.numeric(unlist(quantile(speed, probs=c(level), na.rm = TRUE)))
  }
  
  segments_with_apc_route_analytics <- compiled_apc_dat %>% unnest(cols = c(trip_dat)) %>%
    group_by(FINAL_ID, route_id) %>%
    summarise(daily_ridership = sum(ridership),
              avg_route_speed = mean(avg_speed, na.rm = TRUE),
              avg_speed_q50 = quant_num(avg_speed, 0.5),
              avg_speed_q10 = quant_num(avg_speed, 0.1),
              avg_speed_q90 = quant_num(avg_speed, 0.9),
              trips = n(),
              service_hours = sum(run) / 60 %>% round(2),
              riders_per_service_hour = sum(ridership) / service_hours) %>%
    mutate(route_fixed = fix_routes(c(route_id))) %>%
    left_join(segments_geometry) %>% 
    mutate(riders_per_km = daily_ridership / (length / 1000))
  
  return(segments_with_apc_route_analytics)
}

