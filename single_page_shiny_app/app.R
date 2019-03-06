library(shiny)
library(leaflet)
library(RColorBrewer)
library(readr)
library(dplyr)

log0 <- function(x){ifelse(x<=0,0,log(x))}

world_geojson <- geojsonio::geojson_read("countries.geo.json", what = "sp")

trade_tbl <- read_csv("import_export_clean.csv") %>%
  filter(country_code %in% world_geojson$id)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  titlePanel("Ethiopian Imports and Exports from 2014-2017"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, draggable = TRUE, fixed=TRUE,
                sliderInput("selected_year","Year", min = 2013, max = 2017, value = 2017, sep = ''),
                radioButtons("selected_trade_direction","Export/Import", choices=c("export","import"),
                             selected = "export")
  )
)

server <- function(input, output, session) {
  
  filteredTradeTbl <- reactive({
    trade_tbl %>% 
      filter(Year == input$selected_year, trade_direction == input$selected_trade_direction) %>% 
      slice(match(world_geojson$id, country_code))
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(world_geojson) %>% addTiles() %>% addPolygons(stroke = FALSE)
  })
  
  # Incremental changes to the choropleth are performed in
  # an observer.
  observe({
    
    filtered_trade_tbl <- filteredTradeTbl()
    
    filtered_world_geojson <- world_geojson[world_geojson$id %in% filtered_trade_tbl$country_code, ]
    
    #create color palette for choropleth
    pal <- colorNumeric("Greens", domain = log0(filtered_trade_tbl$total_trade))
    
    #create labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g US Dollars <sup></sup>",
      filtered_trade_tbl$country_name, filtered_trade_tbl$total_trade) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("map", data = filtered_world_geojson) %>%#, data = filtered_trade_tbl) %>%
      addPolygons(fillColor = ~pal(log0(filtered_trade_tbl$total_trade)),
                  weight = 2,
                  opacity = 1,
                  color = 'white',
                  dashArray = '3',
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  })

}

shinyApp(ui, server)

#this code largely hijacked from examples here: https://rstudio.github.io/leaflet/shiny.html
