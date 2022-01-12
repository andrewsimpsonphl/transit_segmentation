library(ggridges)
library(viridis)
library(hrbrthemes)

## PLOT FUNCTIONS ##

plot_trips_per_hour <- function(hourly_route_analytics) {
  
  p <- ggplot(hourly_route_analytics) +
    geom_bar(stat = "identity", aes(x = trip_hour, y = trips, fill = route_id)) +
    scale_x_continuous(name = "Hour", breaks = c(0:23)) +
    scale_y_continuous(name = "Trips in Either Direction")
  
  ggplotly(p) %>% 
    layout(title = list(text = paste0("Hourly Trips by Route"),x=0, xref="paper"),
           margin=list(t = 40))
}

# plot daily ridership and trips 
# route_analytics is a return of analyze_segment_route
plot_daily_ridership_trips <- function(route_analytics, corridor_name){
  grid.arrange(ncol=2,
               ggplot(route_analytics, aes(y = route_analytics$daily_ridership, x = route_id, label = daily_ridership)) + 
                 geom_bar(stat = "identity", fill = "skyblue") +
                 geom_text(position=position_dodge(width=0.9), vjust = -0.75, size = 3) +
                 scale_y_continuous(name = "Total Riders Served", n.breaks = 8, limits = c(0, max(route_analytics$daily_ridership) * 1.1)) + 
                 scale_x_discrete(name = "Route Number") + 
                 labs(title = paste0("The Corridor Serves ", round(sum(route_analytics$daily_ridership)), " Riders per Day"),
                      subtitle = corridor_name) +
                 theme(text = element_text(size = 9)),
               ggplot(route_analytics, aes(x=route_id, y = route_analytics$trips, label = trips)) + 
                 geom_bar(position="dodge", stat="identity") + 
                 geom_text(position=position_dodge(width=0.9), vjust = -0.75, size = 3) +
                 ylab("Daily Trips (2019)") + 
                 scale_x_discrete(name = "Route Number") + 
                 scale_fill_phl(palette = "main", discrete = T) +
                 labs(fill= "", title = paste0("The Corridor Serves ", sum(route_analytics$trips, na.rm = TRUE), " Trips per Day")) + 
                 #theme_phl() + 
                 theme(legend.position = "top")+
                 theme(text = element_text(size = 9))
  )
  
}

# interactive hourly speed line graph for a specified subcorridor 
plot_hourly_speed <- function(hourly_analytics, corridor_name){
  
  dat2 <- apc_data %>% 
    group_by(trip_hour = hour_bin) %>% 
    summarise(avg_segment_speed = mean(velocity, na.rm = TRUE)) %>% 
    filter(trip_hour >= 0 & trip_hour < 24) %>% 
    mutate(label = "System Average")
  
  dat <- hourly_analytics %>% select(trip_hour, avg_segment_speed) %>% 
    mutate(label = corridor_name) %>% 
    full_join(dat2)
  
  p <- ggplot(dat) + 
    geom_line(aes(x = trip_hour, y = avg_segment_speed, color = label)) +
    geom_point(aes(x = trip_hour, y = avg_segment_speed), shape=21, size=3) + 
    scale_y_continuous(name = "Speed (MPH) - Includes Dwell Time", n.breaks = 8) +
    scale_x_continuous(name = "Hour", breaks = c(0:23)) +
    theme(text = element_text(size = 9),
          legend.position = "bottom")
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0("Hourly Speed (All Routes)",
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  
  return(output)
}

# binned_analytics is a return of analyze_segment_hourbin
plot_ridership_by_period <- function(binned_analytics, corridor_name) {
  p <- ggplot(binned_analytics, aes(x = timeframe, y = daily_ridership, label = daily_ridership)) + 
    geom_bar(stat = "identity", fill="skyblue", alpha=0.7) +
    geom_text(position=position_dodge(width=0.9), vjust = -0.75, size = 3) +
    scale_y_continuous(name = "Total Riders Served", n.breaks = 5) + #, limits = c(0, max(binned_analytics$avg_speed_90_pct) * 1.1)) +
    scale_x_discrete(name = element_blank()) +
    theme(text = element_text(size = 9))
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0('Ridership by Period (All Routes)',
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  
  return(output)
}

# hourly_route_analytics is a return of analyze_segment_route_hourly
plot_ridership_by_route <- function(hourly_route_analytics, corridor_name) {
  #a <- as.data.frame(subset(df$hourly_route_analytics, subcorridor_id==id))
  p <- ggplot(hourly_route_analytics, aes(x = hourly_route_analytics$trip_hour, y = daily_ridership, group=route_id, fill=route_id)) + 
    geom_bar(position = "stack", stat = "identity") +
    #geom_point(shape=21, size=3) + 
    scale_color_viridis(discrete = TRUE) +
    scale_y_continuous(name = "Riders Served", n.breaks = 8) +
    scale_x_continuous(name = "Hour", breaks = c(0:23)) +
    theme(text = element_text(size = 9)) 
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0('Hourly Ridership by Route (all directions)',
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  
  return(output)
}

# plots hourly ridership by route and direction
# hourly_route_direction_analytics is a return of analyze_segment_route_direction_hourly
plot_ridership_by_route_dir <- function(hourly_route_direction_analytics, corridor_name) {
  p <- ggplot(hourly_route_direction_analytics, aes(x = hourly_route_direction_analytics$trip_hour, 
                                                    y = daily_ridership, fill=direction_id)) + 
    geom_bar(position = "stack", stat = "identity") +
    #geom_point(size=3) + 
    #scale_color_viridis(discrete = TRUE) +
    scale_y_continuous(name = "Riders Served", n.breaks = 8) +
    scale_x_continuous(name = "Hour", breaks = c(0:23)) +
    theme(text = element_text(size = 9)) +
    facet_wrap(~route_id, ncol = 2) + 
    theme(text = element_text(size = 9))  +
    theme(axis.text.x = element_text(angle = 90))
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0("Hourly Ridership by Route/Direction",
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  return(output)
}

# hourly_route_direction_analytics is a return of analyze_segment_route_direction_hourly
plot_running_time <- function(corridor_dat, corridor_name, pattern_list) {
  
  #full_df <- full_corridor_dat$dat[[1]]
  
  x <- corridor_dat$dat[[1]] %>% 
    ungroup() %>% 
    filter(pattern_id %in% pattern_list) %>%
    analyze_segment_route_direction_hourly()
  
  
  p <- ggplot(x %>% subset(avg_run >=0), aes(x = trip_hour, 
                                                                            y = avg_run, fill=direction_id)) + 
    geom_bar(position = "stack", stat = "identity") +
    #geom_point(size=3) + 
    #scale_fill_viridis(discrete = TRUE) +
    ylab("Average Running Time (Includes Dwell)") +
    scale_x_continuous(name = "Hour", breaks = c(0:23)) +
    theme(text = element_text(size = 9)) +
    facet_wrap(~route_id + direction_id, ncol = 2) + 
    #theme_phl(base_size = 9)
    theme(text = element_text(size = 9), legend.position = "none",
          axis.text.x = element_text(angle = 90))
  
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0("Average End-to-End Running Time by Route/Direction",
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  return(output)
}

# route_analytics is a return of analyze_segment_route
plot_service_hrs <- function(route_analytics, corridor_name) {
  p <- ggplot(route_analytics, aes(x=route_id, y = service_hours, label = round(service_hours))) + 
    geom_bar(position="dodge", stat="identity") + 
    geom_text(position = "dodge", stat = "identity", vjust = -0.25) +
    #ggtitle("Market and JFK serve over 1,100 Bus Trips per Day") + 
    ylab("Service Hours Per Day") + 
    scale_x_discrete(name = "Route Number") + 
    scale_fill_phl(palette = "main", discrete = T) +
    #theme_phl() + 
    theme(legend.position = "top")+
    theme(text = element_text(size = 9))
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0("Daily Corridor Service Hours",
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  return(output)
}

# binned_analytics is a return of analyze_segment_hourbin
plot_speed_by_period <- function(binned_analytics, corridor_name) {
  p <- ggplot(binned_analytics, aes(x = timeframe, y = avg_segment_speed)) + 
    geom_bar(stat = "identity", fill="skyblue", alpha=0.7) +
    geom_errorbar(data = binned_analytics, stat = "identity", ymin = binned_analytics$avg_speed_10_pct, ymax =
                    binned_analytics$avg_speed_90_pct, colour="orange", alpha=0.6, size=1.3, width = 0.4) + 
    scale_y_continuous(name = "Speed (MPH) - Includes Dwell Time", n.breaks = 8, 
                       limits = c(0, max(binned_analytics$avg_speed_90_pct) * 1.1)) +
    scale_x_discrete(name = element_blank()) +
    theme(text = element_text(size = 9))
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0("Speed by Period (All Routes)",
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  return(output)
}
# hourly_route_direction_analytics is a return of analyze_segment_route_direction_hourly
plot_speed_by_route_dir <- function(hourly_route_direction_analytics, corridor_name) {
  p<- ggplot(hourly_route_direction_analytics, aes(x = hourly_route_direction_analytics$trip_hour, 
                                                   y = avg_segment_speed, color=direction_id)) + 
    geom_line() +
    geom_point(size=1.5) + 
    #scale_color_viridis(discrete = TRUE) +
    scale_y_continuous(name = "Speed (MPH) - Includes Dwell Time", n.breaks = 8) +
    scale_x_continuous(name = "Hour", breaks = c(0:23)) +
    theme(text = element_text(size = 9)) +
    facet_wrap(~route_id, ncol = 2) + 
    theme(axis.text.x = element_text(angle = 90)) 
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0("Hourly Speed by Route/Direction",
                                                              '<br />','<sup>',
                                                              corridor_name,
                                                              '</sup>','<br />'),x=0, xref="paper"),
                                   margin=list(t = 80))
  return(output)
}


plot_joyplot_runtimes <- function(full_df, route_num, direction = "Eastbound", pattern_list) {
  
  full_df <- full_corridor_dat$dat[[1]]
  x <- full_df %>% ungroup() %>% 
    filter(direction_id == direction & route_id == route_num & pattern_id %in% pattern_list) %>%
    mutate(time_bin = (trip_begin %>% lubridate::as_datetime(format = "%H:%M:%S") %>% hour())) %>% 
    arrange(time_bin) %>% 
    mutate(time_bin = as.factor(time_bin))
  
  title_txt = paste0("Corridor Run Times by Time of Day")
  subtitle_txt = paste0("Route ", route_num, " - ", direction)
  
  #subtitle = make_subtitle(filtered_df)
  
  # Plot
  p <- ggplot(x, aes(x = lubridate::as.duration(run), y = (time_bin), fill = ..x..)) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      scale_fill_viridis(name = "Temp. [F]", option = "C") +
      scale_x_time() +
      theme_ipsum() +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      ylab("Time of Day (00:00 to 24:00)") +
      xlab("Run Time") +
      ggtitle(title_txt, subtitle = subtitle_txt)

  return(p)
}


plot_speed_corridor_comparison <- function(corridor_df) {
  
  # make the full corridor line a dash or bold and label it better
  
  dat <- corridor_df %>% select(corridor_id, corridor_name, hourly_analytics) %>% 
    unnest(hourly_analytics) %>% 
    group_by(corridor_id, corridor_name) %>% 
    mutate(corridor_name = as.factor(corridor_name))
  
  p <- ggplot(dat) +
    geom_line(aes(x = trip_hour, 
                  y = avg_segment_speed, 
                  group = corridor_name, 
                  color = corridor_name)) +
    scale_color_viridis(discrete = TRUE) +
    scale_y_continuous(name = "Speed (MPH) - Includes Dwell Time", n.breaks = 8) +
    scale_x_continuous(name = "Hour", breaks = c(0:23)) +
    theme(legend.title = element_blank())
  
  output <- ggplotly(p) %>% layout(title = list(text = paste0("Hourly Speed (All Routes)"),x=0, xref="paper"),
                                   legend = list(text = element_blank(),
                                                 orientation = "h",   # show entries horizontally
                                                 xanchor = "center",  # use center of legend as anchor
                                                 x = 0.5,
                                                 y=-0.2),
                                   margin=list(t = 60))

    return(output)  
}
