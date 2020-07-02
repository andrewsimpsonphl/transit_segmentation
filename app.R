#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#next: add a buttom that when clicked runs an function that calculates the analytics for the selected links
# DONE - add button
# - on button click, find the stop IDs associated with the selected links (in the clicked list)
# - pass those stopIDs to the function

library(shiny); library(shinydashboard)
library(leaflet) ; library(leaflet.extras)
library(sf) ; library(sp)
library(htmltools)
library(tidyverse)
library(rgeos)
library(reactable)

source("./code/segmentation_code.R")

# Nice to have: query the traffic count dataset and return a chart of traffic counts in the area, akin to DVRPC's portal
# https://dvrpc-dvrpcgis.opendata.arcgis.com/datasets/dvrpc-traffic-counts


create_links <- function(loaded_links) {
    links <- loaded_links %>% group_by(fromto) %>%  summarise(stop_list = list(unique(stop_id))) %>% 
        mutate(secondLocationID = paste(as.character(fromto), "_selectedLayer", sep="")) %>% 
        rowwise() %>% 
        mutate(stop_str = paste(unlist(stop_list), collapse = ", "))
    
    st_write(links, "./data/links_with_stops.geojson", driver = "GeoJSON", delete_dsn = TRUE)
}
#create_links(links_with_stops)

# import links
links <- st_read("./data/links_with_stops.geojson")
link_stop_data <- links %>% mutate(fromto = as.character(fromto), secondLocationID = as.character(secondLocationID)) %>% 
    mutate(stop_list = as.list(strsplit(as.character(stop_str), ","))) 

coordinates = link_stop_data$geometry
#print(coordinates)
    
# import APC data
apc_data <- read.csv("./data/combined_apc_dataset.csv") %>%  mutate(stop_id = paste0(agency_id, stop_id))

ui <- dashboardPage(
    dashboardHeader(title = "Transit First  Dashboard"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
               box(width = NULL, solidHeader = TRUE, 
                   leafletOutput("link_map", height = 600))
               ),
        fluidRow(
            column(width = 3, 
                   box( 
                       actionButton("calculate", "Calculate Analytics"),
                       p(class = "text-muted",
                         br(),
                         "Calculate analytics for selected segments.",
                         br(),
                         "Caution: this may take awhile."
                       )
                       ) 
            ), 
            column(width = 8, 
               box(tableOutput("analytics_table"), width = NULL),
               box(tableOutput("analyticsB_table"), width = NULL),
               box(reactableOutput("analyticsC_table"), width = NULL)
            ),
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    data_of_click <- reactiveValues(clickedMarker = list())
    
    output$link_map <- renderLeaflet({
        #print(segment_data$FINAL_ID %>%  head())
        
        leaflet(link_stop_data) %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addPolylines(data = link_stop_data, color = "Blue", weight = 4, layerId = link_stop_data$fromto
                         #popup = paste0(link_stop_data$FINAL_ID, " - StopID: ", link_stop_data$stops_str),
                         #highlightOptions = highlightOptions(color = "green", weight = 4, bringToFront = TRUE)) %>% 
            ) %>% 
            addDrawToolbar(
                targetGroup='Selected',
                polylineOptions=FALSE,
                markerOptions = FALSE,
                polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
                rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                      ,color = 'white'
                                                                                      ,weight = 3)),
                circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
                editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
        
    })
    
    observeEvent(input$link_map_shape_click, {
        
        # Leaflet Proxy
        proxy <- leafletProxy("link_map")
        
        # Get Shape Input Click
        
        click = input$link_map_shape_click
        #print(click)
        
        markerID = click$id
        print(markerID)
        
        #print(segment_data %>% filter(FINAL_ID == markerID))
        output$selected_ID = renderText({ 
            paste0("You have selected segment ID: ", markerID)
        })
        
    })
    
    # function for finding the locations inside the shapes we draw
    findLocations <- function(shape, location_coordinates, location_id_colname){
        
        # derive polygon coordinates and feature_type from shape input
        polygon_coordinates <- shape$geometry$coordinates
        #print(shape$geometry)
        
        feature_type <- shape$properties$feature_type
        #print(feature_type)

        #print(location_coordinates[1])
        
        if(feature_type %in% c("rectangle","polygon")) {
            #print("feature is a polygon")
            # transform into a spatial polygon
            drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))
            #print(drawn_polygon)
            #print(list(drawn_polygon))
            #print(sp::Polygons(list(drawn_polygon), "drawn_polygon"))
            sp_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon), "drawn_polygon")), proj4string = CRS("+init=epsg:4326"))
            #print(sp_polygon)
            
            sf_polygon <- sf::st_as_sf(sp_polygon)
            #print(sf_polygon)
            #print(location_coordinates)
            
            selected_locs <- st_intersects(sf_polygon, link_stop_data)
            
            #print("selected location IDs:")
            #print(selected_locs) # get the lines that fall inside the box
            
            # get location ids
            #x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
            x = selected_locs[[1]]
            #print(x)
            
            #selected_loc_id = as.character(x[[location_id_colname]])
            #print(selected_loc_id)
            
            return(x)
            
        } else if (feature_type == "circle") {
            print("feature is a circle")
            
            #center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]]), ncol = 2)
            #print(center_coords)
            
            # get distances to center of drawn circle for all locations in location_coordinates
            # distance is in kilometers
            #dist_to_center <- spDistsN1(location_coordinates, center_coords, longlat=TRUE)
            
            # get location ids
            # radius is in meters
            #x <- location_coordinates[dist_to_center < shape$properties$radius/1000, location_id_colname]
            
            #selected_loc_id = as.character(x[[location_id_colname]])
            
            #return(selected_loc_id)
        }
    }
    
    observeEvent(input$link_map_draw_new_feature,{
        print("Draw new feature event observed")
        
        new_shape = input$link_map_draw_new_feature
        #print(shape)
        
        #location_coordinates = new_shape$geometry$coordinates
        #print(coordinates)
        
        #Only add new layers for bounded locations
        found_in_bounds <- findLocations(shape = new_shape, 
                                         location_coordinates = coordinates, 
                                         location_id_colname = "ID")
        
        #print(found_in_bounds)
            
        for(id in found_in_bounds){
            #print("ID: ")
            #print(id)
            if(id %in% data_of_click$clickedMarker){
                # don't add id
            } else {
                # add id
                data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
                #print(data_of_click$clickedMarker)
            }
        }
        
        #print("data_of_click$clickedMarker: ")
        #print(data_of_click$clickedMarker %>% unlist())
        
        list <- data_of_click$clickedMarker %>% unlist()
        #print(list)
        
        # take the list, which are the rownumbers, and subset
        selected <- link_stop_data %>% slice(list)
        
        # print FINAL_ID into the text box
        output$selected_ID = renderText({ 
            #paste0("You have selected segment ID: ", selected$FINAL_ID)
        })
        
        proxy <- leafletProxy("link_map")
        proxy %>% addPolylines(data = selected, color = "Green", weight = 8, opacity = 1,
                               layerId = as.character(selected$secondLocationID),
                               highlightOptions = highlightOptions(color = "hotpink",
                                                                   opacity = 1.0,
                                                                   weight = 2,
                                                                   bringToFront = TRUE)
                             )
        
    })
    
    observeEvent(input$link_map_draw_deleted_features,{
        print("Deleted feature event observed")
        
        # loop through list of one or more deleted features/ polygons
        for(feature in input$link_map_draw_deleted_features$features){
            
            # get ids for locations within the bounding shape
            bounded_layer_ids <- findLocations(shape = feature, location_id_colname = "secondLocationID")
            #print(bounded_layer_ids)
            
            unselected <- link_stop_data %>% slice(bounded_layer_ids)
            #print(unselected)
            
            list_unselected <- (unselected$secondLocationID)
            #print(list_unselected)
            
            # remove second layer representing selected locations
            proxy <- leafletProxy("link_map")
            proxy %>% removeShape(list_unselected)
            
            first_layer_fromtos <- (link_stop_data %>% slice(bounded_layer_ids))$fromto
            #print(first_layer_fromtos)
            
            # returns a full link stop data frame with a boolean column whether it is in the first_layer_fromto
            x <- link_stop_data %>% 
                mutate(in_list = case_when(
                    fromto %in% first_layer_fromtos == TRUE ~ TRUE,
                    TRUE ~ FALSE
                    )
                )
            #print(x)
            
            # See the features that are TRUE
            #x_in_list <- x %>%  filter(in_list == TRUE)
            #print(x_in_list)
            
            # get the ID numbers (i.e. row numbers) of the features that had been selected
            first_layer_ids <- which(x$in_list == TRUE)
            #print("First Layer IDS:")
            #print(first_layer_ids)
            
            # Remove those IDs from the running data_of_click list so that it doesn't reproject them
            data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker %in% first_layer_ids]
            #print("Clicked Data:")
            #print(data_of_click$clickedMarker %>% unlist())
        }
    })
    
    observeEvent(input$calculate, {
        print("Button click detected")
        
        # get the ids of the currently selected values
        list <- data_of_click$clickedMarker %>% unlist()
        #print(list)
        
        # subset the data based on list
        data <- link_stop_data %>%  slice(list)
        #print(data)
        
        # get the list of stops associated with the data
        stop_list <- data$stop_list %>% unlist() %>% as.character() %>% unique()
        #print(stop_list)
        
        trip_dat <- suppressWarnings(find_trip_dat_v2(apc_data, stop_list))
        #print(trip_dat)
        
        analytics <- analyze_segment(trip_dat)
        print(analytics)
        
        output$analytics_table <- renderTable(analytics)
        
        binned_analytics <- suppressWarnings(analyze_segment_hourbin(trip_dat))
        print(binned_analytics)
        
        output$analyticsB_table <- renderTable(binned_analytics)
        
        hourly_route_analytics <- suppressWarnings(analyze_segment_route_hourly(trip_dat))
        
        output$analyticsC_table <- renderReactable({
            reactable(hourly_route_analytics)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
