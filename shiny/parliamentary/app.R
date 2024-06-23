library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(here)


precinct_sf <- st_read(str_glue("{here::here()}/data/maps/2024_22_Elec_simplify.json"))

precinct_viz_sf <-  
  read_rds(str_glue("{here::here()}/data/제22대_국회의원선거_지도.rds"))

sf::sf_use_s2(FALSE)

precinct_sf <- st_make_valid(precinct_sf)

sido_sf <- precinct_sf %>% 
  group_by(SIDO) %>%
  summarise(geometry = st_union(geometry))

# Define UI
ui <- fluidPage(
  titlePanel("제23대 국회의원 선거"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sido", "시도:", 
                  choices = unique(precinct_viz_sf$SIDO),
                  selected = unique(precinct_viz_sf$SIDO)[1])
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )  )
)

# Define server
server <- function(input, output) {
  
  # Reactive expression for the selected 시도
  selected_sido <- reactive({
    precinct_viz_sf %>% filter(SIDO == input$sido)
  })
  
  # Render the map
  output$map <- renderLeaflet({
    colors <- c("#8B0000", "#FF0000", "#FFA07A", "white", "#ADD8E6", "#0000FF", "#00008B")
    bins <- c(-Inf, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, Inf)
    color_function <- colorBin(colors, domain = precinct_viz_sf$차이, bins = bins)
    
    map <- leaflet(selected_sido()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color_function(차이),
        weight = 1.5,
        opacity = 0.5,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 1.5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = lapply(
          sprintf(
            "<b>%s</b><br/>득표차: %2.1f%%<br/>당선정당: %s<br/>당선후보: %s<br/>낙선정당: %s<br/>낙선후보: %s",
            selected_sido()$SIDO_SGG, 100*selected_sido()$차이, 
            selected_sido()$당선정당, selected_sido()$당선후보,
            selected_sido()$낙선정당, selected_sido()$낙선후보
          ),
          htmltools::HTML
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto",
          sticky = TRUE
        )
      ) %>%
      addPolylines(
        data = sido_sf,
        weight = 5,
        opacity = 1,
        color = "black"
      ) %>%
      addLegend(
        pal = color_function,
        values = ~차이,
        labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x), 
        opacity = 0.7,
        title = "득표율 차이",
        position = "bottomright"
      )
    
    # Fit the map bounds to the selected 시도
    map %>% fitBounds(
      lng1 = st_bbox(selected_sido())[["xmin"]],
      lat1 = st_bbox(selected_sido())[["ymin"]],
      lng2 = st_bbox(selected_sido())[["xmax"]],
      lat2 = st_bbox(selected_sido())[["ymax"]]
    )
  })
  
}

# Run the app
shinyApp(ui, server)