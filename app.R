library(dplyr);library(shiny);library(leaflet)

input <- list(route = 60)

ui <- navbarPage("webmap", id="nav",
                 tabPanel("Interactive map",
                          tags$head(
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          div(class="outer",
                              leafletOutput("map", width="100%", height="100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("People Mover"),
                                            selectInput("route", label = h3("route"), 
                                                        choices = list(1,8,60), 
                                                        selected = 60),
                                            selectInput("direction", label = h3("direction"), 
                                                        choices = list(1,8,60), 
                                                        selected = 60)
                              ))
                          ),
                 tabPanel("Data explorer")
)

server <- function(input, output) {
  get_data <- reactive({
    load("tidy_gps_obj.rda")#gps
    load("my_daily_gtfs.rda")#gtfs
    (get_data <- list(tidy_gps_obj, my_daily_gtfs))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% setView(lat = 61.15, lng = -149.8,zoom = 11) %>% addTiles()  
  })

  observe({
    dat_list <- get_data()
    
    gps_points <- dat_list[[1]][dat_list[[1]]$route == input$route,]
    
    trip_ids <- dat_list[[2]]$todays_trip_departures %>% ungroup() %>% mutate(route_id = as.numeric(route_id)) %>% 
      filter(route_id == input$route) %>% .$trip_id
    
    route_to_map <- dat_list[[2]]$today_stop_times %>% filter(trip_id == trip_ids[1]) %>% inner_join(dat_list[[2]]$stops, by = "stop_id")
    
    leafletProxy("map") %>%
      clearShapes() %>% clearMarkers() %>%
      addMarkers(data = gps_points, lng = ~lon, lat = ~lat) %>%
      addPolylines(dat = route_to_map, lng = ~stop_lon, lat = ~stop_lat, color = "green")
  })
  
  
  }

shinyApp(ui = ui, server = server)
