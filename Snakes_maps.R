rm(list = ls())

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)  # Modern spatial data handling

# File paths
snake_data_path <- "D:/GIS/Snakes/App/Snake database_cleaned_Sept 2024.csv"
grid_shapefile <- "D:/GIS/Snakes/App/Grid_8thdegree_sz.shp"
boundary_shapefile <- "D:/GIS/Snakes/App/Sd_bound.shp"

# Load and process snake occurrence data
if (file.exists(snake_data_path)) {
  data <- read_csv(snake_data_path)
} else {
  stop("⚠ Snake data file not found. Check the path.")
}

# Combine Genus and Species into a single identifier
data <- data |> 
  unite(group, c(Genus, Species), sep = " ", remove = FALSE) |> 
  rename(lat = Latitude, lng = Longitude) |> 
  drop_na(lat, lng)

# Load grid shapefile (polygon boundaries)
if (file.exists(grid_shapefile)) {
  grids <- st_read(grid_shapefile, quiet = TRUE)
} else {
  stop("⚠ Grid shapefile not found. Check the path.")
}

# Load Eswatini boundary shapefile
if (file.exists(boundary_shapefile)) {
  eswatini_boundary <- st_read(boundary_shapefile, quiet = TRUE)
} else {
  stop("⚠ Eswatini boundary shapefile not found. Check the path.")
}

# Ensure CRS consistency for all spatial data
grids <- st_transform(grids, crs = 4326)
eswatini_boundary <- st_transform(eswatini_boundary, crs = 4326)

# Convert snake occurrence data to spatial points
snake_points <- st_as_sf(data, coords = c("lng", "lat"), crs = 4326)

# UI
ui <- fluidPage(
  titlePanel("Snake Distribution in Eswatini"),
  
  fluidRow(
    column(3,  # Sidebar panel width
           wellPanel(
             selectInput(
               "group",
               "Select Species",
               choices = unique(data$group),
               selected = NULL,  # Start with nothing selected
               multiple = FALSE  # Only one species at a time
             )
           )
    ),
    
    column(9,  # Map pane width (larger portion of the screen)
           leafletOutput("map", height = "100vh")  # Increase map height
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive Data: Identify which grids contain the selected snake species
  grid_presence <- reactive({
    if (is.null(input$group) || input$group == "") {
      return(grids |> mutate(present = FALSE))  # No selection, no highlights
    }
    
    selected_snakes <- data |> filter(group == input$group)
    
    if (nrow(selected_snakes) == 0) {
      return(grids |> mutate(present = FALSE))
    }
    
    # Convert filtered occurrences to spatial points
    selected_snake_points <- st_as_sf(selected_snakes, coords = c("lng", "lat"), crs = 4326)
    
    # Spatial join: Identify grids where selected species occur
    grids |> 
      mutate(present = lengths(st_intersects(grids, selected_snake_points)) > 0)
  })
  
  # Render Leaflet Map
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      
      # Add the Eswatini boundary as a black outline
      addPolygons(
        data = eswatini_boundary,
        color = "black",
        weight = 2,
        fillOpacity = 0,
        group = "Eswatini Boundary"
      ) |> 
      
      # Add grid cells where species occur (default is empty)
      addPolygons(
        data = grid_presence(),
        fillColor = ~ifelse(present, "grey", "transparent"), 
        weight = 1, 
        color = "black", 
        fillOpacity = 0.6,
        group = "grid_layer"
      )
  })
  
  # Update Grid Map on Selection Change
  observe({
    leafletProxy("map", data = grid_presence()) |> 
      clearGroup("grid_layer") |> 
      addPolygons(
        fillColor = ~ifelse(present, "grey", "transparent"), 
        weight = 1, 
        color = "black", 
        fillOpacity = 0.6,
        group = "grid_layer"
      )
  })
}

# Run the App
shinyApp(ui = ui, server = server)
