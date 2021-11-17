source("./code/segmentation_code.R")

## Step 1 - Import Data ##
apc_data <- import_apc()
stop_list <- c(130, 14079, 14080, 14081, 14084, 14085, 14086, 14088, 14089, 131) %>% 
  paste0("SEPTA", .)

stop_names_2019 <- read_csv("data/stop_names_2019.csv") %>% 
  mutate(stop_id = paste0(agency_id, stop_id))

## Step 2 - Initial Processing ##
# start by getting the nested data together
nested_trip_dat <- nest_trip_data_v2(filter_trip_list(apc_data, stop_list))
                 
# run calc_pass on each set of data to get corridor level descriptive stats for each trip                     
dat <- lazy_dt(run_passenger_data_v3(nested_trip_dat, stop_list)) %>%
  ungroup()  %>%
  as_tibble() 

dat2 <- dat %>% unnest(calculated_pass)

# create stop level dataframe for corridor 
stop_trip_dat <- find_stop_dat(apc_data, stop_list)

daily_stop_analytics <- analyze_stops_daily(stop_trip_dat)

export_stop_dat <- function(stop_dat) {
  export <- stop_dat %>% 
    arrange(stop_lat, stop_lon) %>% 
    mutate(avg_speed = round(avg_speed, 2),
           stop_change = 0)
  
  write_csv(export, "./data/temp/stop_data_export.csv")
}

export_stop_dat(daily_stop_analytics)

stop_route_analytics <- analyze_stops_routes_daily(stop_trip_dat)
stop_route_binned_analytics <- suppressWarnings(analyze_stops_routes_hourbin(stop_trip_dat))
stop_route_hourly_analytics <- suppressWarnings(analyze_stops_routes_hourly(stop_trip_dat))

# Thoughts - 
# Try to export out a database of stop level data that can be edited - e.g. each stop can be "improved", "removed", "null"
# this could then be reimported into the processing pipeline for predictions to be applied on top



# need to control for passengers being reallocated and maybe induced demand from faster service
# need to control for trips that leave corridor early










dat3 <- apc_data %>% 
  mutate(velo_adj = case_when(
    velocity <= 0 ~ NA_real_,
    velocity == Inf ~ NA_real_,
    velocity > 60 ~ NA_real_,
    TRUE ~ as.numeric(velocity))
    ) %>% 
  group_by(X) %>% 
  summarise(avg_speed = mean(velo_adj, na.rm= TRUE))

ggplot(dat3) + geom_histogram(aes(x = avg_speed))
