# external scrips for usage.rmd

library(tidyverse) ; library(httr) ; library(jsonlite) ; library(geojsonsf)
library(sf)  ;  library(leaflet)  ;  library(htmltools) ; library(knitr)

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
  stops <- st_read("./stops_shp/PA_Stops_w_Segment.shp") %>%
    select(`StopId`, `fromnode`, `tonode`, `combo`) %>%
    left_join(full_dat_by_stop, by = c("StopId" = "stop_id")) %>%
    rename("stop_id" = "StopId")
  
  return(stops)
}

read_links <- function() {
  links <- st_read("./links_shp/ReliabilityScore.shp") %>%
    select(`combo`)
  
  return()
}


## @knitr load_data

load_segments <- function(segmentation_dat) {
  segments_loaded <- segmentation_dat %>% 
    left_join(stops %>% data.frame() %>% select(-geometry), by = c("fromto" = "combo")) %>%
    filter(BASE_ID > 0 & BASE_ID < 900) %>%
    filter(TYPE > 0 & TRANSIT > 0 & OTHER > 0) %>%
    group_by(FINAL_ID) %>%
    summarise(load = mean(total_load, na.rm = TRUE), stops = list(unique(stop_id)))
  
  return(segments_loaded)
}

segment_geometry <- function(segmentation_dat) {
  segments_geometry <- segmentation_dat %>% 
    filter(BASE_ID > 0 & BASE_ID < 900) %>%
    filter(TYPE > 0 & TRANSIT > 0 & OTHER > 0) %>%
    group_by(FINAL_ID) %>%
    summarise(length = sum(Shape__Length))
  
  return(segments_geometry)
}


