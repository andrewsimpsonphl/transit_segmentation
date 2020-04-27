# external scrips for usage.rmd

## @knitr pull_data

pull_arcgis_dat <- function() {
  token = "NWNCARyaCcjTuyuIRWUcwvOBMD9CisgHOeWrhfeJkmrJzVtoTsK8-NhjCd_0EsGc355e7-1dQ4JC-riyApugI3X__qFWKN1EabgoeHvk9ZQu82OrcNODPgU3MEy_ccDwbsatiAdXFloB855DR5W3m2T0YpGzqbkKJKQbeip7lkZCvOo25gMlkgMwzublPDjQFGgcFZg6NdMSFUEQXYd6J631LVjPsWlQZ9mC2s3a1DTsNpc6LjuJKb9iiQMT71IT"
  
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

read_stops <- function() {
  stops <- st_read("./data/stops_shp/PA_Stops_w_Segment.shp") %>%
    select(`StopId`, `fromnode`, `tonode`, `combo`) %>%
    rename(stop_id = StopId)
}

## @knitr load_data

# filter out segments, join stop_id to each
load_coded_links <- function(segmentation_dat, stops) {
  loaded_segments <- segmentation_dat %>% 
    left_join(stops %>% data.frame() %>% select(-geometry), by = c("fromto" = "combo")) %>%
    filter(BASE_ID > 0 & BASE_ID < 900) %>% # filter out BASE_IDs below 900
    filter(TYPE > 0 & TRANSIT > 0 & OTHER > 0) # filter out incomplete FINAL_IDs
  
  return(loaded_segments)
}

## @knitr nest_segment_data

# nests the link data for each segment
# key is that it returns a list of stops along each segment
nest_segments <- function(coded_links, stops) {
  nested_dat <- lazy_dt(coded_links) %>%
    left_join(stops %>% data.frame() %>% select(-geometry), by = c("fromto" = "combo")) %>%
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
      
      #print("Filtering")
      return((x))
    }
    
    nest_trip_data_v2 <- function(filtered_dat) {
      y <- filtered_dat %>%
        lazy_dt() %>%
        arrange(trip_id, stop_seq) %>%
        group_by(trip_id) %>%
        dt_nest(trip_id) # build a nested data frame of individual trips
      
      #print("Nesting")
      return(y)
    }
    #test <- nest_trip_data_v2(filter_trip_list(apc_trip_data))
    
    calc_pass_v2 <- function(df, list = c(20644, 18447, 18448, 10275, 10272, 18451, 10266)) {
      #print(paste("Running calc pass on trip:", df$trip_id, sep = " "))
      
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
        lazy_dt(immutable = TRUE) %>%
        mutate(entry_load = first(load)) %>%
        #mutate(ridership = max(entry_load) + sum(ons)) %>%
        summarise(
          run = as.duration(last(hms(time_stamp)) - first(hms(time_stamp))),
          trip_begin = (first((time_stamp))),
          trip_end = (last((time_stamp))),
          route = paste(unique(route_id), collapse = ", "),
          direction = paste(unique(direction_id), collapse = ", "),
          source = paste(unique(source), collapse = ", "),
          ons_total = sum(ons),
          offs_total = sum(offs),
          #ons_offs = ons_total + offs_total, 
          #ridership = max(entry_load) + ons_total, 
          max_entry_load = max(entry_load), 
          avg_speed = mean(velocity, na.rm = TRUE),
          #avg_speed = na_if(avg_speed, Inf),
          avg_load = mean(load, na.rm = TRUE),
          max_load = max(load)) %>%
        as.data.frame() %>%
        mutate(ons_offs = ons_total + offs_total) %>%
        mutate(ridership = max_entry_load + ons_total) %>%
        mutate(avg_speed = na_if(avg_speed, Inf))
      
      return(output)  
    }
    
    # run calc_pass() on each nested set of data to calculate ridership (load + boards on segment) 
    run_passenger_data_v2 <- function(nested_trip_dat, stop_list = c(20644, 18447, 18448, 10275, 10272, 18451, 10266)) {
      output <- nested_trip_dat %>%
        lazy_dt(immutable = FALSE) %>%
        mutate(calculated_pass = map(data, calc_pass_v2, list = stop_list)) %>%
        as.data.frame()
      
      return(output)
    }
    
    filtered_dat <- filter_trip_list(apc_trip_data, stop_list)
    nested_trip_dat <- nest_trip_data_v2(filtered_dat)
    print(paste("Running passenger data for", count(nested_trip_dat %>% as_tibble()), "trips", sep = " "))
    final_dat <- run_passenger_data_v2(nested_trip_dat, stop_list) %>% 
      ungroup()  %>% 
      lazy_dt() %>%
      dt_unnest(calculated_pass) %>%
      filter(run > as.duration(0)) # filter out trips that only make 1 stop on the corridor
    
    return(final_dat)
    gc()
  }
  
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

add_analytics <- function(compiled_apc_dat, gis_dat) {
  
  # return that geometry and length of each segment
  find_segment_geometry <- function(gis_dat) {
    segments_geometry <- gis_dat %>% 
      group_by(FINAL_ID) %>%
      summarise(length = sum(Shape__Length))
    
    #return(segments_geometry)
  }
  
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
    mutate(avg_speed_sd = map(trip_dat, ~sd(as.numeric(unlist(.$avg_speed) , na.rm = TRUE), na.rm = TRUE))) %>%
    mutate(ridership = na_if(ridership, 0)) %>%
    mutate(riders_per_m = ridership / length) %>%
    mutate(riders_per_km = ridership / (length / 1000)) %>%
    mutate_at(c("avg_speed", "avg_speed_q10", "avg_speed_q50", "avg_speed_q90", "avg_speed_sd",
                "avg_load", "avg_load_q50", "avg_load_q90", "avg_load_q10", "trips"), as.numeric) %>%
    mutate(avg_speed_cv = as.numeric((avg_speed_sd)) / as.numeric((avg_speed))) %>%
    rowwise() %>%
    mutate(routes_str = (routes_list %>% unlist() %>% paste(collapse = ", "))) %>%
    mutate(stops_str = (stops %>% unlist() %>% paste(collapse = ", "))) %>%
    mutate(riders_per_trip_km = ridership / (trips * length / 1000))
  return(output) 
}

