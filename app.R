library(shiny)
library(leaflet)
library(sf)
library(readr)
library(tidyverse)
library(DT)

# libpoppler-cpp-dev
# cmake
# libgdal-dev
# libudunits2-dev


base_path <- "/run/media/xavier/datascience/code/csdta/poc-shiny-meteocat-xema-daily/"

stations <- read_csv(paste0(base_path,"xemadata/stations_metadata.csv")) |> 
  mutate(
    lon = as.numeric(str_extract(Georeferència, "(?<=\\()\\s*-?\\d+\\.\\d+")),
    lat = as.numeric(str_extract(Georeferència, "-?\\d+\\.\\d+\\s*(?=\\))"))
  )

stations[stations$CODI_ESTACIO == "V5",]$CODI_COMARCA <- 24
stations[stations$CODI_ESTACIO == "V5",]$NOM_COMARCA <- "Osona"

borders <- st_read(paste0(base_path,"geodata/comarques-compressed.geojson"))

data <- read_csv(paste0(base_path,"xemadata/daily_stations_meteo_data.csv")) |> 
  mutate(
    valor = str_remove(VALOR,"\\.") |> 
      str_replace("\\,","\\.") |> 
      as.numeric() 
  )

variables <- data |> 
  select(CODI_VARIABLE,NOM_VARIABLE) |> 
  distinct()

ui <- fluidPage(
  fluidRow(
    column(4,
      leafletOutput("cat_map")
    ),
    column(4,
      DTOutput("stations_table")
    ),
    column(4,
      textOutput("clicked_station"),
      DTOutput("variables_table")
    )
  ),
  fluidRow(
    textOutput("text"),
    textOutput("clicked_variable")
  ),
  fluidRow(
    
  )
)

server <- function(input, output, session){
  
  output$cat_map <-  renderLeaflet({
    
    leaflet(borders) |> 
      addProviderTiles("Esri.WorldImagery") |> 
      addPolygons(
        data = borders,
        layerId = ~nom_comar,
        color = "red",
        fillOpacity = 0.0,
        weight = 2,
        highlightOptions = highlightOptions(color = "white", weight = 3,
                                            bringToFront = TRUE)
      ) 
  })
  
  # Update map with the station selected in the table and print on the right 
  # the average measurments which can be again clicked and then a time grpah appears
  # at the bottom
  
  getStations <- function(comarca){
    stations |> 
      filter(NOM_COMARCA == comarca) |> 
      select(CODI_ESTACIO,NOM_ESTACIO,lat,lon)
  }
  
  getStationVariables <- function(codi_estacio){
    data |> 
      filter(CODI_ESTACIO == codi_estacio) |> 
      select(CODI_VARIABLE,NOM_VARIABLE) |> 
      distinct()
  }
  
  clicked_border <- reactiveVal(NULL)
  clicked_station <- reactiveVal(NULL)
  

  observeEvent(input$cat_map_shape_click, { 
    click <- input$cat_map_shape_click
    if(!is.null(clicked_border())){
      leafletProxy("cat_map") %>%
        addPolygons(
          data = borders[borders$nom_comar == clicked_border(), ],
          layerId = clicked_border(),
          fillColor = "transparent",
          fillOpacity = 0.0,
          color = "red",
          weight = 2,
          highlightOptions = highlightOptions(color = "white", weight = 3,
                                              bringToFront = TRUE)
        )
    }
    clicked_border(click$id)

    output$text <- renderText({
      paste("Comarca:", clicked_border())
    })
    
    # Highlight new selection
    leafletProxy("cat_map") %>%
      addPolygons(
        data = borders[borders$nom_comar == clicked_border(), ],
        layerId = clicked_border(),
        fillColor = "blue",
        fillOpacity = 0.5,          
        color = "red",
        weight = 2
      )
    
    output$stations_table <- renderDataTable({
      getStations(clicked_border()) |> 
        datatable(selection = "single")
    })
    
    # Reset variables_table
    output$variables_table <- renderDataTable({
      NULL
    })
    
    # Reset clicked_station
    clicked_station(NULL)
    
  })
  
  observeEvent(input$stations_table_rows_selected, {
    selected_row <- input$stations_table_rows_selected
    if (length(selected_row) > 0) {
      selectedStation <- getStations(clicked_border())[selected_row,]
      clicked_station(selectedStation$CODI_ESTACIO)
      
      # output$clicked_station <- renderText({
      #   paste("Selected row:", selected_row,
      #         "Comarca", clicked_border()
      #         ,"Codi estacio:",clicked_station()
      #         )
      # })
      
      output$variables_table <- renderDataTable({
        getStationVariables(clicked_station()) |> 
          datatable(selection = "single")
      })
    }
  })
  
  observeEvent(input$variables_table_rows_selected, {
    selected_row <- input$variables_table_rows_selected
    if (length(selected_row) > 0) {
      selectedVariable <- getStationVariables(clicked_station())[selected_row,]
      output$clicked_variable <- renderText({
        paste("Selected row:", selected_row,
              "Codi estacio:", clicked_station()
              ,"Codi variable",selectedVariable$CODI_VARIABLE
        )
      })
    }
  })
  
}


shinyApp(ui, server)
