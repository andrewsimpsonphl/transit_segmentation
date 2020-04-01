# external scrips for usage.rmd

library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf) ; library(lubridate)
library(sf)  ;  library(leaflet)  ;  library(htmltools) ; library(knitr)  ; library(feather)

## @knitr pull_data

pull_arcgis_dat <- function() {
  path_geo_0_4000 <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/segmentation_v1_shp/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=HodualTVpMTIigvp2LNpZBFGKGm8GECP4vbiJIkTdhREtbkW1jYXBrAqiLAq5BSZ2Kq9ZW7zWWZtwaYkZMh4jakgBiP9DAWidMf6NP4BTvb4tHyeQq5uSL7ICCKQufS8U8Oq4f7irzgmWLAjTs_dLGaQ-ZNQV1ZB5TVLrSNhNltztF1FXxJ7rKUdmjQFjZa4jwDoDtb8AZoanNj5EGajYYdYLGTnulcw_erSXZVPXd2Z0fcU77Ha56p4RGDARLjP"
  path_geo_4001_8000 <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/segmentation_v1_shp/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=4001&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=HodualTVpMTIigvp2LNpZBFGKGm8GECP4vbiJIkTdhREtbkW1jYXBrAqiLAq5BSZ2Kq9ZW7zWWZtwaYkZMh4jakgBiP9DAWidMf6NP4BTvb4tHyeQq5uSL7ICCKQufS8U8Oq4f7irzgmWLAjTs_dLGaQ-ZNQV1ZB5TVLrSNhNltztF1FXxJ7rKUdmjQFjZa4jwDoDtb8AZoanNj5EGajYYdYLGTnulcw_erSXZVPXd2Z0fcU77Ha56p4RGDARLjP"
  path_geo_8001_12000 <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/segmentation_v1_shp/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=8001&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=HodualTVpMTIigvp2LNpZBFGKGm8GECP4vbiJIkTdhREtbkW1jYXBrAqiLAq5BSZ2Kq9ZW7zWWZtwaYkZMh4jakgBiP9DAWidMf6NP4BTvb4tHyeQq5uSL7ICCKQufS8U8Oq4f7irzgmWLAjTs_dLGaQ-ZNQV1ZB5TVLrSNhNltztF1FXxJ7rKUdmjQFjZa4jwDoDtb8AZoanNj5EGajYYdYLGTnulcw_erSXZVPXd2Z0fcU77Ha56p4RGDARLjP"
  path_geo_12001_13500 <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/segmentation_v1_shp/FeatureServer/0/query?where=FID+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=standard&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=12001&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=HodualTVpMTIigvp2LNpZBFGKGm8GECP4vbiJIkTdhREtbkW1jYXBrAqiLAq5BSZ2Kq9ZW7zWWZtwaYkZMh4jakgBiP9DAWidMf6NP4BTvb4tHyeQq5uSL7ICCKQufS8U8Oq4f7irzgmWLAjTs_dLGaQ-ZNQV1ZB5TVLrSNhNltztF1FXxJ7rKUdmjQFjZa4jwDoDtb8AZoanNj5EGajYYdYLGTnulcw_erSXZVPXd2Z0fcU77Ha56p4RGDARLjP"
  
  
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

read_stops <- function() {
  stops <- st_read("./data/stops_shp/PA_Stops_w_Segment.shp") %>%
    select(`StopId`, `fromnode`, `tonode`, `combo`) %>%
    rename(stop_id = StopId)
}

#segmentation_dat <- pull_arcgis_dat()
#stops <- read_stops()

## @knitr load_data

# filter out segments, join stop_id to each
load_coded_links <- function(segmentation_dat, stops) {
  loaded_segments <- segmentation_dat %>% 
    left_join(stops %>% data.frame() %>% select(-geometry), by = c("fromto" = "combo")) %>%
    filter(BASE_ID > 0 & BASE_ID < 900) %>% # filter out BASE_IDs below 900
    filter(TYPE > 0 & TRANSIT > 0 & OTHER > 0) # filter out incomplete FINAL_IDs
  
  return(loaded_segments)
}



#coded_links <- load_coded_links(segmentation_dat, stops)

## @knitr nest_segment_data

# nests the link data for each segment
# key is that it returns a list of stops along each segment
nest_segments <- function(coded_links, stops) {
  nested_dat <- coded_links %>%
    left_join(stops %>% data.frame() %>% select(-geometry), by = c("fromto" = "combo")) %>%
    group_by(FINAL_ID) %>%
    summarise(stops = list(unique(stop_id.x))) %>%
    data.frame() %>% select(-geometry) %>%
    group_by(FINAL_ID) %>%
    nest()
  
  return(nested_dat)
}

#nested_segments <- nest_segments(coded_links, stops)

## @knitr apc_dat functions

#import apc trip data
import_apc <- function() {
  apc_trip_data <- read_feather("./data/preped_apc_data.feather")
  return(apc_trip_data)
}
#apc_trip_data <- import_apc()

# Inputs: 
# nested_data - data that has had nest_segments called on it
# segments - data that has had load_segments called on it
compile_apc_dat <- function(nested_data) {
  apc_trip_data <- import_apc()
  
  # function that returns a dataframe of APC trip data for a list of stops
  find_trip_dat_v2 <- function(apc_trip_data, stop_list = c(20644, 18447, 18448, 10275, 10272, 18451, 10266)) {
    nest_trip_data_v2 <- function(filtered_dat, a = 6066, b = 6115) {
      y <- filtered_dat %>%   
        arrange(trip_id, stop_seq) %>%
        group_by(trip_id) %>%
        nest() # build a nested data frame of individual trips
      
      return(y)
    }
    #test <- nest_trip_data_v2(filter_trip_list(apc_trip_data))
    
    # pull all trips that stop at a stop in a list
    # returns an unested data fram of all trips that stop at one of the given stops
    filter_trip_list <- function(dat, list = c( 20644, 18447, 18448, 10275, 10272, 18451)){
      list_a <- dat %>%
        filter(`stop_id` %in% list ) %>%
        distinct(trip_id) %>% 
        pull(1) # returns list of trips stopping at A
      
      x <- dat %>%
        # filter if trip # is on A list or B list
        filter(`trip_id` %in% list_a) 
      
      return(x)
    }
    
    calc_pass_v2 <- function(df, list = c(20644, 18447, 18448, 10275, 10272, 18451, 10266)) {
      
      # arrange by stop sequence and then slice
      x <- df %>% 
        arrange(stop_seq) %>%
        distinct(`stop_id`, .keep_all = TRUE) %>%
        mutate(id = row_number()) %>%
        group_by(id)
      
      id_list <- x$id[x$`stop_id` %in% list]
      #id_b <- x$id[x$`STOPID` == b]
      
      y <- x %>% 
        filter(between(id, first(id_list), last(id_list))) %>%
        ungroup()
      
      #calc. run times and passenger activity for each trip
      output <- y %>%
        mutate(entry_load = first(load)) %>%
        summarise(
          run = as.duration(last(hms(time_stamp)) - first(hms(time_stamp))),
          trip_begin = (first((time_stamp))),
          trip_end = (last((time_stamp))),
          route = paste(unique(route_id), collapse = ", "),
          direction = paste(unique(direction_id), collapse = ", "),
          source = paste(unique(source), collapse = ", "),
          ons = sum(ons),
          ons_offs = sum(ons) + sum(offs), 
          ridership = max(entry_load) + sum(ons), 
          avg_speed = mean(velocity, na.rm = TRUE),
          avg_speed = na_if(avg_speed, Inf),
          max_load = max(load)) 
      
      # factor and order Period to make sorting easier later on
      #output$period <- factor(output$period, levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night"))
      
      return(output)  
    }
    
    # run calc_pass() on each nested set of data to calculate ridership (load + boards on segment) 
    run_passenger_data_v2 <- function(dat, stop_list = c(20644, 18447, 18448, 10275, 10272, 18451, 10266)) {
      
      output <- dat %>%
        mutate(calculated_pass = map(data, calc_pass_v2, list = stop_list))
      
      return(output)
    }
    
    
    filtered_dat <- filter_trip_list(apc_trip_data, stop_list)
    
    nested_dat <- nest_trip_data_v2(filtered_dat, stop_list)
    
    final_dat <- run_passenger_data_v2(nested_dat, stop_list) %>%  unnest(cols = c(calculated_pass))
    
    return(final_dat)
  }
  
  # helper function for compile_apc_dat that helps call find_trip_dat_v2
  f <- function(list) {
    find_trip_dat_v2(apc_trip_data, list) %>%
      select(-c(data)) # need to drop the data frame within each trip - makes everything huge!
  }
  
  final <- nested_data %>%
    mutate(trip_dat = map(data[[1]], map, f)) %>% 
    unnest(cols = c(trip_dat)) %>%
    mutate(ridership = map_dbl(trip_dat, ~sum(.$ridership, na.rm = TRUE))) %>%
    mutate(avg_speed = map(trip_dat, ~mean(.$avg_speed, na.rm = TRUE))) %>%
    mutate(avg_speed_quantiles = map(trip_dat, ~quantile(.$avg_speed, probs=c(0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE))) %>%
    mutate(routes = map(trip_dat, ~list(unique(.$route)))) %>%
    mutate(avg_speed_sd = map(trip_dat, ~sd(as.numeric(unlist(.$avg_speed) , na.rm = TRUE), na.rm = TRUE)))

  return(final)
}

add_analytics <- function(compiled_apc_dat, gis_dat) {
  
  # return that geometry and length of each segment
  find_segment_geometry <- function(gis_dat) {
    segments_geometry <- gis_dat %>% 
      group_by(FINAL_ID) %>%
      summarise(length = sum(Shape__Length))
    
    return(segments_geometry)
  }
  
  segments_geometry <- find_segment_geometry(gis_dat)
  
  output <- compiled_apc_dat %>% left_join(segments_geometry) %>% 
    mutate(ridership = na_if(ridership, 0)) %>%
    mutate(riders_per_mile = ridership / length * 5280) %>%
    mutate(avg_speed_num = as.numeric(unlist(avg_speed))) %>%
    mutate(avg_speed_cv = as.numeric(unlist(avg_speed_sd)) / as.numeric(unlist(avg_speed)))
  
  return(output)
}

#segments_with_apc_dat <- compile_apc_dat(nested_segments) %>% add_analytics()

## @knitr final_run

# step 1: create your stop file
stops <- read_stops()

# step 2: pull in coded links from AGO 
gis_dat <- pull_arcgis_dat() 

nested_data <- gis_dat %>%
  load_coded_links(stops) %>%  # step 3.1: load coded links with stop_ids
  nest_segments(stops)         # step 3.2: nest the links into segments with FINAL_IDs
  
FINAL_ID_LIST <- unique((as.numeric(nested_data$FINAL_ID)))

# step 3.3: compile APC trip data to the segment level
segments_with_apc_dat <- nested_data %>% 
  filter(between(as.numeric(FINAL_ID), FINAL_ID_LIST[1], FINAL_ID_LIST[100])) %>%
  compile_apc_dat()  

# step 4: run analytics on each segment
segments_with_apc_analytics <- segments_with_apc_dat %>% add_analytics(gis_dat) 
