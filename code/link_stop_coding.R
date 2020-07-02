# Stop to Link Coding Framework - 1.0

# Creates a dataframe of stops with the correct relationship to a given set of network links

# Process Outline
#    - Break a GTFS route line down into smaller pieces, coded by route_line_id
#    - Relate bus stops to those smaller route_line_ids
#    - Buffer the network links and select route_line_ids within those buffers - this buffer excludes cross streets in favor of parallel lines
#    - Create a database of bus stops with all of the links that are nearby and parallel
#    - Filter this data down to the closes link lines
#    - Return data frame of bus stops with their "matched" link

# To Dos:
#    - Scale solution up to all routes instead of one by one, then collapse down into single stops
#    - QAQC known trouble spots, e.g. where a bus is turning and passing itself in different directions (Route 48 in Fairmount)


# Some ideas - see if there is a way of getting a line for each direct of a buses travel. This would fixe the Route 48 issue). Check Kenny's work.
#     follow up: can definitely be done with Kenney's route patterns. But there are 1,500 patterns. Think about how to collapse down to most common? cross ref with gtfs
#     alternative - GTFS contains a shape_id that is different for different trips of a route

require(tidyverse) ; require(tidytransit)
require(leaflet)
require(lwgeom)
library(sf)
library(httr)
library(geojsonsf)

#install.packages("devtools")
devtools::install_github("jmt2080ad/polylineSplitter")

source("code/segmentation_code.R")
# step 2: pull in coded links from AGO 
gis_dat <- pull_arcgis_dat()

# import Spring 2019 GTFS
spring_2019_gtfs <- tidytransit::read_gtfs("./data/spring_2019_gtfs.zip")

trips <- spring_2019_gtfs$trips %>% group_by(trip_id, route_id) %>%  summarise()

stops_by_route <- spring_2019_gtfs$stop_times %>% mutate(stop_id = as.numeric(stop_id)) %>% 
  left_join(trips, by = ("trip_id")) %>% 
  group_by(stop_id, route_id) %>% 
  summarise() %>% 
  ungroup()

gtfs_sf <- gtfs_as_sf(spring_2019_gtfs)
gtfs_sf

route_sf <- get_route_geometry(gtfs_sf)
stop_sf <- gtfs_sf$stops %>% mutate(stop_id = as.numeric(stop_id)) %>% full_join(stops_by_route)

route_42_sf <- route_sf %>% filter(route_id == 42)
route_42_sf <- get_route_geometry(gtfs_sf, route_ids = c(42))
route_42_stops_sf <- stop_sf %>% filter(route_id == 42)

# how to look at one route shape at a time
test_shape <- spring_2019_gtfs$shapes %>% filter(shape_id == 220752) %>% tidytransit::shapes_as_sf()
leaflet(test_shape) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines()

# build df of each shape x route 
new_shapes <- spring_2019_gtfs$shapes %>% tidytransit::shapes_as_sf()
new_route_df <- spring_2019_gtfs$trips %>% left_join(new_shapes)

# need to find frequency of pattern by route
route_shape_frequency <- new_route_df %>% group_by(route_id, direction_id, shape_id) %>% summarise(count = length(unique(trip_id))) %>% arrange(route_id, -count)
num_shapes <- length(route_shape_frequency$shape_id)

# another option: figure out a way to make sure all stops are covered by at least 1 route pattern - probably the most efficient way
#     start by building out a trip x stop x timepoint x shape data base
spring_2019_gtfs$stop_times # there are 2.1million individual stop_times in a day at SEPTA
trip_stop_time_shape <- spring_2019_gtfs$stop_times %>% left_join(new_route_df)
#     now we don't really need to know about time, we want to figure out how to get every stop covered by the least number of shapes

# # try 1: coalesce down to each stop, with a list of each unique shape - interesting question here whether to group by direction or not...
# stop_shapes <- trip_stop_time_shape %>%  group_by(stop_id) %>% summarise(shape_list = (unique(shape_id)))
# 
# # try 2: the opposite
# shape_stops <- trip_stop_time_shape %>%  group_by(shape_id) %>% summarise(stop_list = (unique(stop_id)))
# a <- shape_stops$shape_id
# b <- shape_stops$stop_list
# test <- as.matrix(a, b)

# try 3: incorporate routes
stop_route_shapes <- trip_stop_time_shape %>%  group_by(route_id, stop_id) %>% summarise(shape_list = (unique(shape_id))) %>%  arrange(route_id, stop_id)
route_shape_stops <- trip_stop_time_shape %>%  group_by(route_id, direction_id, shape_id) %>% summarise(stop_list = list(unique(stop_id))) %>%  arrange(route_id, shape_id) %>% 
  mutate(list_length = map(stop_list, length))

all_route_stops <- trip_stop_time_shape %>% group_by(route_id, direction_id) %>% summarise(stop_list = list(unique(stop_id))) %>% 
  mutate(list_length = map(stop_list, length))

# will need to pull just the stops used by that trip/shape to do the analysis

# pull link data from arcgis online
#source("code/segmentation_code.R")
#gis_dat <- pull_arcgis_dat()


#### VISUAL EXPLORATION ####
# vie the raw GIS data
# leaflet(gis_dat) %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addPolylines(data = gis_dat, color = "Pink", weight = 4, #layerId = segmentized$route_id,
#                #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
#                highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)
#   )

# # overlay the split up route file with the GIS
# leaflet(R42_lines) %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addPolylines(data = R42_lines, color = "Blue", weight = 4, #layerId = segmentized$route_id,
#                #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
#                highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)) %>% 
#   addPolylines(data = gis_dat, color = "Pink", weight = 4, #layerId = segmentized$route_id,
#                #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
#                #highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)
#   )


# New Method:
# 
# # for each route line, join the stop to the closest route piece
# #...
# nearest = try(st_nearest_feature(stop_sf %>% filter(route_id == 42), R42_lines))
# ls = st_nearest_points(stop_sf %>% filter(route_id == 42), R42_lines[nearest], pairwise = TRUE)
# 
# # Show the match between route and stops
# leaflet(ls) %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addPolylines(data = R42_lines, color = "Blue", weight = 1, #layerId = segmentized$route_id,
#                #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
#                highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)) %>% 
#   addPolylines(color = "Black", weight = 1, #layerId = segmentized$route_id,
#                #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
#                highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)) %>% 
#   addCircles(data = stop_sf %>% filter(route_id == 42), fill = "Grey", radius = 1)
# 


map_routeline_stop_pairs <- function(route = 42, route_data = route_sf, stop_data = stop_sf) {
  selected_route_sf <- route_data %>% filter(route_id == route)
  
  selected_route_pieces <- selected_route_sf[[2]] %>% st_cast(to = "LINESTRING")
  selected_stops_sf <- stop_data %>% filter(route_id == route)
  
  nearest = try(st_nearest_feature(selected_stops_sf, selected_route_pieces))
  ls = st_nearest_points(selected_stops_sf, selected_route_pieces[nearest], pairwise = TRUE)
  
  # Show the match between route and stops
  leaflet(ls) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolylines(data = selected_route_pieces, color = "Blue", weight = 1, #layerId = segmentized$route_id,
                 #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
                 highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)) %>% 
    addPolylines(data = ls, color = "Black", weight = 1, #layerId = segmentized$route_id,
                 #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
                 highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)) %>% 
    addCircles(data = selected_stops_sf, fill = "Grey", radius = 2)
}

#map_routeline_stop_pairs(route = 21)


# Figuring out why the 21 and 12 are mostly a few long line
# split_route_sf <- route_sf %>% rowwise() %>% 
#   mutate(length = st_length(geometry),
#          split_lines = list(geometry %>% st_cast(to = "LINESTRING"))) %>% 
#   mutate(avg_split_length = mean(st_length(split_lines)),
#          max_split_length = max(st_length(split_lines)))
# 
# #split_route_sf %>%  filter(route_id == 42 | route_id == 21 | route_id == 12)
# shows that the line lengths for the 21 and 12 are much longer than the 42

# df to quickly see which routes are most effected by really long LINESTRINGS making up their MULTISTRINGS
#check <- split_route_sf %>% arrange(avg_split_length) %>% select(route_id, length, avg_split_length, max_split_length)


#### MAIN FUNCTIONS ####
# NEED TO REWRITE TO TAKE IN A SHAPE SF INSTEAD OF A ROUTE SF

test_shape <- route_shape_stops[1, ] %>%  left_join(spring_2019_gtfs$shapes %>% tidytransit::shapes_as_sf())
shape_dat = test_shape

# method to break down a route into smaller lines:
fix_shape_lines <- function(shape_dat = test_shape, line_length = 50) {
  #df_route_x <- route_df %>% filter(route_id == route_number)
  
  # make points along the route shape
  first_points <- shape_dat %>% 
    sf::st_as_sf() %>% 
    st_segmentize(line_length) %>% # change to set the max length of the segments
    sf::st_as_sf() %>%     
    st_cast(to = "POINT") %>% 
    mutate(point_id = 1:n())
  
  # make a dataset of lines between the point - break into "even" and "odd" to make sure lines are drawn between all of the points
  # shape_line_id is a new id field representing those new lines
  even_points <- first_points %>% 
    rowwise() %>% 
    mutate(even = case_when(
      point_id %% 2 == 0 ~ 1,
      TRUE ~ 0
    )) %>% 
    mutate(shape_line_id = point_id - even) %>% 
    ungroup()
  
  odd_points <- first_points %>% 
    rowwise() %>% 
    mutate(even = case_when(
      point_id %% 2 == 0 ~ 0,
      TRUE ~ 1
    )) %>% 
    mutate(shape_line_id = point_id - even) %>% 
    ungroup()
  
  # join those points (with their line geographies) together
  points <- full_join(even_points, odd_points) %>% ungroup() %>% st_as_sf()
  
  # use sf version of dplyr lang. to create a LINESTRING object with the lines from both sets of points  
  new_lines <- points %>%
    group_by(route_id, shape_line_id, shape_id, direction_id, stop_list) %>% 
    summarize(do_union=FALSE) %>% 
    st_cast("LINESTRING") %>% 
    ungroup()
  
  # filter out lines that are very long
  fixed_lines <- new_lines %>% 
    mutate(length = st_length(geometry)) %>% 
    filter(as.numeric(length) < line_length)
  
  return(fixed_lines)
}

test_shape_fix <- fix_shape_lines(test_shape, line_length = 50)

# DEPRECATED - NO LONGER USING ROUTE LINES
# method to break down a route into smaller lines:
fix_route_lines <- function(route_df = route_sf, route_number = 21, line_length = 50) {
  df_route_x <- route_df %>% filter(route_id == route_number)
  
  # make points along the route shape
  first_points <- df_route_x %>% 
    st_segmentize(line_length) %>% # change "500" to set the max length of the segments
    st_cast(to = "POINT") %>% 
    mutate(stop_id = 1:n()) %>% 
    mutate(route_id = route_number)
  
  # make a dataset of lines between the point - break into "even" and "odd" to make sure lines are drawn between all of the points
  even_points <- first_points %>% 
    rowwise() %>% 
    mutate(even = case_when(
      stop_id %% 2 == 0 ~ 1,
      TRUE ~ 0
    )) %>% 
    mutate(route_line_id = stop_id - even) %>% 
    ungroup()
  
  odd_points <- first_points %>% 
    rowwise() %>% 
    mutate(even = case_when(
      stop_id %% 2 == 0 ~ 0,
      TRUE ~ 1
    )) %>% 
    mutate(route_line_id = stop_id - even) %>% 
    ungroup()
  
  # join those points (with their line geographies) together
  points <- full_join(even_points, odd_points) %>% ungroup() %>% st_as_sf()

  # use sf version of dplyr lang. to create a LINESTRING object with the lines from both sets of points  
  new_lines <- points %>%
    group_by(route_id, route_line_id) %>% 
    summarize(do_union=FALSE) %>% 
    st_cast("LINESTRING") %>% 
    ungroup()
  
  # filter out lines that are very long
  fixed_lines <- new_lines %>% 
    mutate(length = st_length(geometry)) %>% 
    filter(as.numeric(length) < line_length)
}
#route_21_fix <- fix_route_lines(route_df = route_sf, route_number = 21, line_length = 50)
#route_42_fix <- fix_route_lines(route_df = route_sf, route_number = 42)
# visualize the "fixed" routes 
# test_new_route_df <- new_route_df %>% filter(route_id == 21) %>% group_by(shape_id, direction_id) %>% 
#   distinct(shape_id, .keep_all = TRUE) %>% 
#   select(route_id, trip_headsign, direction_id,shape_id, geometry) %>% 
#   sf::st_as_sf()


# helper to call fix_route_lines on a route and to map it
fix_and_map <- function(shape_dat) {
  fix <- fix_shape_lines(shape_dat, line_length = 50)
  leaflet(fix) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolylines( color = "Blue", weight = 3, #layerId = row(new_lines),
                  highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE))
}


#### SUMMARY OF STEP 1: SEGMENT THE UNIQUE SHAPE ####
# set a test shape - pull one from the route_shape_stops 
test_shape <- route_shape_stops[1, ] %>%  left_join(spring_2019_gtfs$shapes %>% tidytransit::shapes_as_sf())
# fix that shape
test_shape_fix <- fix_shape_lines(test_shape, line_length = 50)
# visualize that shape
fix_and_map(test_shape)

#### STEP 2: PAIR THE STOPS TO THE CLOSEST SHAPE LINE ####

# Start by pairing the stops to the closest shape line
# This works pretty well. Can be seen with mapping.
# return: stop df with "line_id" = the closest route_line_id

# OLD VERSION - pair to route lines
pair_stops_to_routeline <- function(route_number, fixed_route, stop_data = spring_2019_gtfs$stops) {
  selected_route_sf <- fixed_route
  
  selected_route_pieces <- selected_route_sf %>% st_cast(to = "LINESTRING") %>% st_make_valid()
  selected_stops_sf <- stop_data %>% filter(route_id == route_number) %>% st_make_valid()
  
  #nearest = try(st_nearest_feature(selected_stops_sf, selected_route_sf))
  #ls = st_nearest_points(selected_stops_sf, selected_route_pieces[nearest], pairwise = TRUE)
  
  nearest = try(st_nearest_feature(selected_stops_sf, selected_route_pieces))
  n <- nearest %>% as_tibble()
  
  stops <- selected_stops_sf %>% bind_cols(n) %>% select(stop_id, route_id, "route_line_id" = value)
  
  return(stops)
}

# NEW VERSION - pair to shape lines
# Inputs:
# the fixed shape and gtfs based stop df
pair_stops_to_shapeline <- function(fixed_shape, stop_data = spring_2019_gtfs$stops) {
  
  shape_pieces <- fixed_shape %>% st_cast(to = "LINESTRING") %>% st_make_valid()
  
  stop_data_sf <- stop_data %>% tidytransit::stops_as_sf()
  
  # get all of the stops for the shape
  selected_stops_sf <- shape_pieces %>% group_by(shape_id) %>% 
    summarise(stop_id = list(unique(unlist(stop_list)))) %>%    #stop_data %>% filter(stop_id %in% shape_pieces$stop_list) %>% st_make_valid()
    unnest_longer(col = "stop_id") %>% 
    #mutate(stop_id = as.character(stop_id)) %>% 
    as.tibble() %>% select(-geometry) %>% 
    left_join(stop_data_sf) %>% 
    sf::st_as_sf()
  
  # join the stops to the nearest shape pieces
  nearest = try(st_nearest_feature(selected_stops_sf, shape_pieces))
  n <- nearest %>% as_tibble()
  
  stops <- selected_stops_sf %>% bind_cols(n) %>% select(stop_id, shape_id, "shape_line_id" = value)
  
  return(stops)
}

# EXTRA function to visualize the stop/shape pairings:
map_stop_shape_pairs <- function(fixed_shape, stop_data = spring_2019_gtfs$stops) {
  
  # Fix the Route data
  #fixed_route <- fix_route_lines(route_df = route_data, route_number)
  #selected_stops_sf <- stop_sf %>%  filter(route_id == route_number)
  #validated_route <- fixed_route %>% sf::st_make_valid() 
  
  shape_pieces <- fixed_shape %>% st_cast(to = "LINESTRING") %>% st_make_valid()
  stop_data_sf <- stop_data %>% tidytransit::stops_as_sf()
  
  # get all of the stops for the shape
  selected_stops_sf <- shape_pieces %>% group_by(shape_id) %>% 
    summarise(stop_id = list(unique(unlist(stop_list)))) %>%    #stop_data %>% filter(stop_id %in% shape_pieces$stop_list) %>% st_make_valid()
    unnest_longer(col = "stop_id") %>% 
    #mutate(stop_id = as.character(stop_id)) %>% 
    as.tibble() %>% select(-geometry) %>% 
    left_join(stop_data_sf) %>% 
    sf::st_as_sf()
  
  # join the stops to the nearest shape pieces
  nearest = try(st_nearest_feature(selected_stops_sf, shape_pieces))
  n <- nearest %>% as_tibble()
  
  ls = st_nearest_points(selected_stops_sf , shape_pieces[nearest, ], pairwise = TRUE)
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolylines(data = fixed_shape, color = "Blue", weight = 1, highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)) %>% 
    addPolylines(data = ls, color = "Black", weight = 1) %>% 
    addCircles(data = selected_stops_sf, fill = "Grey", radius = 2)
  
}
#map_stop_relations(route_data = route_sf, stop_data = stop_sf, route_number = 12)]

#### STEP 2 SUMMARY ####
# Run fix_shape_lines to segment the shape into many pieces
fixed_shape <- fix_shape_lines(test_shape, line_length = 50)
# create the stop data frame
stop_data <- spring_2019_gtfs$stops
# pair each stop to the closest shape line
test_stop_shape_pairing <- pair_stops_to_shapeline(fixed_shape, stop_data)
# visualize these pairs:
map_stop_shape_pairs(fixed_shape, stop_data)

#### STEP 3: PAIR SHAPE PIECES TO LINKS (WILL HAVE MULTIPLE LINKS PER SHAPE) ####

# old version that joins the fixed route to the links and pairs stops
pair_stops <- function(fixed_route, stop_data = stop_sf, route_number = 21, gis_dat = gis_dat) {
  
  # Fix the Route data
  #fixed_route <- fix_route_lines(route_df = route_data, route_number, line_length = line_length)
  selected_stops_sf <- stop_sf %>%  filter(route_id == route_number)
  
  # Then buffer the links, saving time by only buffering those w/in 1500m of the route line
  
  # start by finding the links near the route line
  # ...
  nearby <- gis_dat %>% st_is_within_distance(fixed_route, dist = 8)
  nears <- (lengths(nearby) > 0) %>% as.tibble()
  near_to_line <- gis_dat %>% bind_cols(nears) %>%  filter(value == TRUE)
  
  # then make the buffer
  buff <- st_buffer((near_to_line), dist = 0.0003)
  
  # Then select the route pieces that are within the buffer
  
  # returns matrix of route lines and buffer areas 
  validated_route <- fixed_route %>% sf::st_make_valid() 
  within <- validated_route %>% st_within(buff, sparse = FALSE)

  
  # convert matrix to data frame with the row name being the line's id number
  within_df <- as.data.frame(within)
  colnames(within_df) <- buff$FID #make the fromto code the column name
  
  # cleaned up matrix in df format with the line_ids at the front
  df <- within_df %>% 
    mutate(route_line_id = fixed_route$route_line_id) %>% 
    #bind_cols(route_21_fix$line_id) %>% 
    select(route_line_id, everything())
  
  # gather the cleaned up matrix df into long format, then filter to only "matches" and group by route_line_id
  final <- gather(df, key = "link_id", value = "within", 2:ncol(df)) %>% 
    filter(within == TRUE) %>% 
    group_by(route_line_id) %>% 
    summarise(links = list(unique(link_id)))
  
  # create a route level line_string file of each route_line_id joined to their matched links
  route_final <- fixed_route %>% 
    left_join(final, b = "route_line_id") %>% 
    mutate(route_id = as.character(route_id)) %>% 
    rowwise() %>% 
    mutate(link_list = paste(unlist(links), collapse = ", ")) %>% 
    ungroup() %>% 
    st_as_sf()
  
  
  # create a df of stops witht heir route_line_id pairs
  route_stop_pairs = pair_stops_to_routeline(route_number = route_number, fixed_route = route_final, stop_data)
  
  # create final join of stops pairs to routes and links
  join <- route_stop_pairs %>% left_join(route_final %>% as_tibble %>% select(-geometry), by = c("route_line_id", "route_id"))
  return(join)  
}

# new version that should take in the fixed shape and the stop data
pair_stops_to_links <- function(fixed_shape, stop_shape_pairing = test_stop_shape_pairing,  gis_dat = gis_dat, buffer_size = 0.003) {
  
  # Then buffer the links, saving time by only buffering those w/in 1500m of the route line
  
  # Step A: start by finding the links near the shape line
  nearby <- gis_dat %>% st_is_within_distance(fixed_shape, dist = 8) # takes awhile 
  nears <- (lengths(nearby) > 0) %>% as.tibble()
  near_to_line <- gis_dat %>% bind_cols(nears) %>%  filter(value == TRUE)
  
  # then make the buffer
  buff <- st_buffer((near_to_line), dist = buffer_size)
  
  # Then select the route pieces that are within the buffer
  
  # returns matrix of route lines and buffer areas 
  validated_route <- fixed_shape %>% sf::st_make_valid()
  within <- validated_route %>% st_within(buff, sparse = FALSE)
  
  # convert matrix to data frame with the row name being the line's id number
  within_df <- as.data.frame(within)
  colnames(within_df) <- buff$FID #make the fromto code the column name
  
  # cleaned up matrix in df format with the line_ids at the front
  df <- within_df %>% 
    mutate(shape_line_id = fixed_shape$shape_line_id) %>% 
    select(shape_line_id, everything())
  
  # gather the cleaned up matrix df into long format, then filter to only "matches" and group by route_line_id
  final <- gather(df, key = "link_id", value = "within", 2:ncol(df)) %>% 
    filter(within == TRUE) %>% 
    group_by(shape_line_id) %>% 
    summarise(links = list(unique(link_id)))
  
  # create a route level line_string file of each route_line_id joined to their matched links
  shape_final <- fixed_shape %>% 
    left_join(final, b = "shape_line_id") %>% 
    mutate(route_id = as.character(route_id)) %>% 
    rowwise() %>% 
    mutate(link_list = paste(unlist(links), collapse = ", ")) %>% 
    ungroup() %>% 
    st_as_sf()

  # create final join of stops pairs to routes and links
  join <- stop_shape_pairing %>% left_join(shape_final %>% as_tibble %>% select(-c(geometry, stop_list)), by = c("shape_line_id", "shape_id"))
  return(join)  
}

map_stop_link_relations <- function(fixed_shape, paired_stop_link_output, gis_dat) {
  
  # fixed_route <- fix_route_lines(route_df = route_data, route_number = route_num)
  
  # create a shape level line_string file of each shape_line_id joined to their matched links
  shape_final <- fixed_shape %>% 
    left_join(final, b = "shape_line_id") %>% 
    mutate(route_id = as.character(route_id)) %>% 
    rowwise() %>% 
    mutate(link_list = paste(unlist(links), collapse = ", ")) %>% 
    ungroup() %>% 
    st_as_sf()
  
  nearest = try(st_nearest_feature(paired_stop_link_output %>% sf::st_make_valid(), shape_final %>% sf::st_make_valid()))
  n <- nearest %>% as_tibble()
  ls = st_nearest_points(paired_stop_link_output %>% sf::st_make_valid(), (shape_final %>% sf::st_make_valid())[nearest, ], pairwise = TRUE)
  
  good_stops <- paired_stop_link_output %>% filter(link_list != "")
  null_stops <- paired_stop_link_output %>%filter(link_list == "")
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron)%>% 
    #addPolygons(data = buff, color = "Black", weight = 1, label = buff$FID) %>% 
    addPolylines(data = shape_final, color = "Blue", weight = 2.5, highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE), 
                 popup = shape_final$link_list) %>% 
    addPolylines(data = ls, color = "Black", weight = 1) %>% 
    addCircleMarkers(data = null_stops, radius = 8, fill = TRUE, color = "Black", fillColor = "Black", popup = paste0("Stop ID: ", null_stops$stop_id, "<br>", "Link ID: ", null_stops$link_list)) %>% 
    addCircleMarkers(data = good_stops, radius = 8, fill = TRUE, color = "Blue", fillColor = "Blue", popup = paste0(good_stops$stop_id, "<br>", "Link ID: ", good_stops$link_list))
}


#### STEP 3 SUMMARY ####
# Run fix_shape_lines to segment the shape into many pieces
fixed_shape <- fix_shape_lines(test_shape, line_length = 50)
# create the stop data frame
stop_data <- spring_2019_gtfs$stops
# pair each stop to the closest shape line
test_stop_shape_pairing <- pair_stops_to_shapeline(fixed_shape, stop_data)
# visualize these pairs:
map_stop_shape_pairs(fixed_shape, stop_data)
# pair stops to links
test_stop_link_pairing <- pair_stops_to_links(fixed_shape = fixed_shape, 
                                              stop_shape_pairing =  test_stop_shape_pairing, 
                                              gis_dat = gis_dat)
# visualize these pairs
map_stop_link_relations(fixed_shape, paired_stop_link_output = test_stop_link_pairing, gis_dat)

# route_12_fixed <- fix_route_lines(route_sf, 12, line_length = 75)
# route_12_stops_paired <- pair_stops(route_12_fixed, stop_data = stop_sf, route_number = 12, gis_dat = gis_dat, line_length = 75)
# map_stop_link_relations(paired_stops_output = route_12_stops_paired, gis_dat, route_sf, route_num = 12)
# 
# route_1_stops_paired <- pair_stops(route_data = route_sf, stop_data = stop_sf, route_number = 1, gis_dat = gis_dat)
# map_stop_link_relations(paired_stops_output = route_1_stops_paired, gis_dat, route_sf, route_num = 1)
# 
# route_42_stops_paired <- pair_stops(route_data = route_sf, stop_data = stop_sf, route_number = 42, gis_dat = gis_dat, line_length = 75)
# map_stop_link_relations(paired_stops_output = route_42_stops_paired, gis_dat, route_sf, route_num = 42)
# 
# 
# route_47_stops_paired <- pair_stops(route_data = route_sf, stop_data = stop_sf, route_number = 47, gis_dat = gis_dat, line_length = 50)
# map_stop_link_relations(paired_stops_output = route_47_stops_paired, gis_dat, route_sf, route_num = 47)
# 
# # works with lettered routes :) 
# route_H_stops_paired <- pair_stops(route_data = route_sf, stop_data = stop_sf, route_number = "H", gis_dat = gis_dat)
# map_stop_link_relations(paired_stops_output = route_H_stops_paired, gis_dat, route_num = "H")

#### STEP 4 ####

# Match the best stop/route_line_id to link_id
match_stops_links <- function(paired_stop_link_output, gis_dat) {
  
  link_data <- gis_dat %>%  mutate(link_id = as.character(FID)) %>% select(link_id, geometry) %>% distinct(.keep_all = TRUE)
  
  # transform the stop data to a long format to unnest the list of potential links
  long_stop_df <- paired_stop_link_output %>% as.tibble() %>% unnest_longer(col = links) %>% select(-link_list) %>% left_join(link_data, by = c("links" = "link_id"))
  
  # split the geometry columns (x = stops, y = lines) into two lists and call st_distance down them
  g1 = st_geometry(long_stop_df$geometry.x)
  g2 = st_geometry(long_stop_df$geometry.y)
  distance = mapply(st_distance, g1, g2)
  
  # now bind the distance column to the stop file
  output <- long_stop_df %>% bind_cols(distance) %>% rename("distance" = 10) %>% 
    #mutate(distance = round(distance, digits = 8)) %>% 
    filter(as.numeric(distance) >= 0) %>% 
    group_by(stop_id) %>% 
    filter(distance == min(distance)) %>%  # group by stops then filter to keep only the lowest distance jawns
    rename("closest_link_id" = links)
  # output is a df of stops with their matched link associated with them
  return(output)
}

map_matched_stops_links <- function(matched_stop_link_df) {
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron)%>% 
    #addPolygons(data = buff, color = "Black", weight = 1, label = buff$FID) %>% 
    addPolylines(data = st_geometry(matched_stop_link_df$geometry.y), color = "Blue", weight = 3, highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE), 
                 popup = matched_stop_link_df$links) %>% 
    #addPolylines(data = ls, color = "Black", weight = 1) %>% 
    #addCircleMarkers(data = null_stops, radius = 8, fill = TRUE, color = "Black", fillColor = "Black", popup = paste0("Stop ID: ", null_stops$stop_id, "<br>", "Link ID: ", null_stops$link_list)) %>% 
    addCircleMarkers(data =  st_geometry(matched_stop_link_df$geometry.x), radius = 8, fill = TRUE, color = "Blue", fillColor = "Blue", popup = paste0(matched_stop_link_df$stop_id, "<br>", "Link ID: ", matched_stop_link_df$links))
}

#### STEP 4 SUMMARY ####
# Run fix_shape_lines to segment the shape into many pieces
fixed_shape <- fix_shape_lines(test_shape, line_length = 50)
# create the stop data frame
stop_data <- spring_2019_gtfs$stops
# pair each stop to the closest shape line
test_stop_shape_pairing <- pair_stops_to_shapeline(fixed_shape, stop_data)
# visualize these pairs:
map_stop_shape_pairs(fixed_shape, stop_data)
# pair stops to links
test_stop_link_pairing <- pair_stops_to_links(fixed_shape = fixed_shape, 
                                              stop_shape_pairing =  test_stop_shape_pairing, 
                                              gis_dat = gis_dat)
# visualize these pairs
map_stop_link_relations(fixed_shape, paired_stop_link_output = test_stop_link_pairing, gis_dat)
# run matching between stops and links
test_matching <- match_stops_links(test_stop_link_pairing, gis_dat)
# map the matching results
# will not show stops that don't have matches! run map_stop_shape_pairs to see those null values
map_matched_stops_links(test_matching)


# 
# route_12_stops_matched <- match_stops_links(route_12_stops_paired, gis_dat)
# map_matched_stops_links(route_12_stops_matched)
# 
# route_42_stops_matched <- match_stops_links(route_42_stops_paired, gis_dat)
# map_matched_stops_links(route_42_stops_matched)

#### PULL ALL THOSE FUNCTIONS TOGETHER ####

# function to produce matched stop data on a shape, soup to nuts
fully_assign_stops_to_links <- function(shape_dat, stop_data = spring_2019_gtfs$stops, gis_dat = gis_dat, 
                                        line_length = 50) {
  
  # Run fix_shape_lines to segment the shape into many pieces
  fixed_shape <- fix_shape_lines(shape_dat, line_length = 50)
  
  # pair each stop to the closest shape line
  stop_shape_pairing <- pair_stops_to_shapeline(fixed_shape, stop_data)
  # pair stops to links
  stop_link_pairing <- pair_stops_to_links(fixed_shape = fixed_shape, 
                                                stop_shape_pairing =  stop_shape_pairing, 
                                                gis_dat = gis_dat)

  # run matching between stops and links
  matching <- match_stops_links(stop_link_pairing, gis_dat)
  # map the matching results
  # will not show stops that don't have matches! run map_stop_shape_pairs to see those null values
  
  return(matching)
}

# pull a single shape as a row
test_shape <- route_shape_stops[1, ] %>%  left_join(spring_2019_gtfs$shapes %>% tidytransit::shapes_as_sf())
# run the functions on that shape
test_full <- fully_assign_stops_to_links(shape_dat = test_shape, stop_data = spring_2019_gtfs$stops, 
                                         gis_dat = gis_dat, line_length = 50)
# visualize those results
map_matched_stops_links(test_full)

# TEST - run the data as a map over a few shapes
#test <- route_shape_stops[1, ] %>%  left_join(spring_2019_gtfs$shapes %>% tidytransit::shapes_as_sf()) %>% 
#  mutate(match_df <- map(., fully_assign_stops_to_links, spring_2019_gtfs$stops, 
#                            gis_dat = gis_dat, line_length = 50))

shape_id_list <- unique((as.numeric(route_shape_stops$shape_id)))

# Basic function to call functions across a data frame of shapes
shape_data <- route_shape_stops %>%  left_join(spring_2019_gtfs$shapes %>% tidytransit::shapes_as_sf())

# pulls the paired stop data frame
run_shape <- function(shape, stop_data = spring_2019_gtfs$stops, gis_data = gis_dat, line_length = 50){ 
  #print(paste("Analyzing Shape", shape$shape_id, sep = ": "))
  
  # Run fix_shape_lines to segment the shape into many pieces
  fixed_shape <- fix_shape_lines(shape, line_length)
  
  # pair each stop to the closest shape line
  stop_shape_pairing <- pair_stops_to_shapeline(fixed_shape, stop_data)
  
  # pair stops to links
  stop_link_pairing <- pair_stops_to_links(fixed_shape = fixed_shape, 
                                           stop_shape_pairing =  stop_shape_pairing, 
                                           gis_dat = gis_dat)
  
  # run matching between stops and links
  matching <- match_stops_links(stop_link_pairing, gis_dat)
  # map the matching results
  # will not show stops that don't have matches! run map_stop_shape_pairs to see those null values
  
  
  #return(matching)
  
  # fully_assign_stops_to_links(shape, stop_data = spring_2019_gtfs$stops, 
  #                             gis_dat = gis_dat, line_length = 50) %>% 
  output <- matching %>%  
    group_by() %>% 
    nest() %>% 
    as.tibble()
  
  return(output)
}

# functiont to run for a route
run_route <- function(route_num, shape_data) {
  dat <- shape_data %>% filter(route_id == route_num) %>% mutate(id = shape_id) %>%  group_by(id) %>% nest() %>% ungroup()
  
  x <- dat %>% mutate(shape_dat = map(.$data, run_shape))
  
  return(x)
}

route_42 <- run_route(shape_data = shape_data, route_num = 42)

# run for all routes

# df of all routes
f <- function()

routes <- spring_2019_gtfs$routes %>% select(route_id) %>% 
  mutate(route_data = map(.$route_id, run_route, shape_data))

dat <- routes[1, ]
test <- dat %>% mutate(route_data = map(.$route_id, run_route, shape_data))



# Now need to simplify and aggregate at the route level
# Need to figure out how to pick the right shape ids
test$stop_list

# idea: find all of the stops that need to be covered by a route going in a certain direction
# then look up those stops in the lists to see which cover them



# Test using method ti "fix" one route and see how accurate it is compared to original DVRPC output