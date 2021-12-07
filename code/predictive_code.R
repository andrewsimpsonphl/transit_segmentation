source("./code/segmentation_code.R")


## Step 1 - Import Data ##
apc_data <- import_apc()
#stop_list <- c(130, 14079, 14080, 14081, 14084, 14085, 14086, 14088, 14089, 131) %>% paste0("SEPTA", .)
stop_list <- c(17038, 21260, 17419, 16489) %>% paste0("SEPTA", .)

stop_names_2019 <- read_csv("data/stop_names_2019.csv") %>% 
  mutate(stop_id = paste0(agency_id, stop_id))

## Step 2 - Initial Processing ##
# start by getting the nested data together
filtered_trip_dat <- filter_trip_list(apc_data, stop_list)
nested_trip_dat <- nest_trip_data_v2(filtered_trip_dat)
                 
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
           stop_change = 0, 
           TSP = 0, 
           farside = 0,
           platform = 0,
           alldoor = 0, 
           queue_jump = 0)
  
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

import <- read.csv("./data/temp/stop_data_import.csv") %>% 
  select(stop_id, stop_name, stop_change, TSP, farside, platform, alldoor, queue_jump)








model_change <- function(import) {
  
  
  
  # try modifying find trip dat but using our new data 
  test <- filtered_trip_dat %>% 
    left_join(import) %>% 
    nest_trip_data_v2()
    
  nested_apc_df <- test[[2]][[62]]
  
  calc_pass_v3 <- function(nested_apc_df, stop_list) {
    
    # arrange by stop sequence and then slice
    # to- do - seperate out the slicing into a seperate function
    x <- nested_apc_df %>%
      as_tibble() %>% 
      arrange(stop_seq) %>%
      distinct(`stop_id`, .keep_all = TRUE) %>%
      mutate(id = row_number()) %>%
      group_by(id)
    
    id_list <- x %>% as_tibble() %>% filter(stop_id %in% stop_list) %>% select(id) %>% as.list() %>% unlist()
    
    #x$id[x$`stop_id` %in% list]
    
    # filter to stops that occur between the first and last stop in the list
    y <- x %>%
      filter(between(id, first(id_list), last(id_list))) %>%
      ungroup()
    
    
    reallocate_passengers <- function(y) {
      
      # to-do - implement distance-based approach
      
      # for now - split half to previous and half to next stop 
      output <- y %>% 
        #owwise() %>% 
        mutate(move_ons =
          case_when(
            stop_change == 1 ~ ons,
            TRUE ~ 0
          )) %>% 
        mutate(move_offs =
                 case_when(
                   stop_change == 1 ~ offs,
                   TRUE ~ 0
                 )) %>% 
        mutate(new_ons = 
          case_when(
            !is.na(lag(move_ons)) & !is.na(lead(move_ons)) ~ (lag(move_ons)/2) + (lead(move_ons)/2) + ons,
            is.na(lag(move_ons)) & is.na(lead(move_ons)) ~ ons,
            is.na(lag(move_ons)) ~ ons + (lead(move_ons)/2),
            is.na(lead(move_ons)) ~ ons + (lag(move_ons)/2),
            TRUE ~ ons)) %>% 
        mutate(new_offs = 
                 case_when(
                   !is.na(lag(move_offs)) & !is.na(lead(move_offs)) ~ (lag(move_offs)/2) + (lead(move_offs)/2) + offs,
                   is.na(lag(move_offs)) & is.na(lead(move_offs)) ~ offs,
                   is.na(lag(move_offs)) ~ offs + (lead(move_offs)/2),
                   is.na(lead(move_offs)) ~ offs + (lag(move_offs)/2),
                   TRUE ~ offs))
    }
    
    new_y <- reallocate_passengers(y)
    
    #calc. run times and passenger activity for each trip
    output <- new_y %>%
      #lazy_dt(immutable = TRUE) %>%
      mutate(entry_load = first(load),
             #old_dwell = dwell_hybrid,
             ons = new_ons, 
             offs = new_offs) %>%
      adjust_dwell_and_velo()
    
      
      
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
        dwell_hybrid = as.duration(sum(dwell_hybrid, na.rm = TRUE)), # uses observed dwell where possible, estimates otherwise
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
  
  
  
}




test <- generate_queue_sizes(apc_data, 56) %>% filter(stop_id %in% stop_list)





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
