library(shiny)
library(leaflet)
library(sf)
library(readr)
library(stringr)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)

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

getStations <- function(comarca){
  stations |> 
    filter(NOM_COMARCA == comarca) |> 
    select(CODI_ESTACIO,NOM_ESTACIO,NOM_MUNICIPI,EMPLACAMENT,lat,lon)
}

getStationVariables <- function(codi_estacio){
  data |> 
    filter(CODI_ESTACIO == codi_estacio) |> 
    select(CODI_VARIABLE,NOM_VARIABLE) |> 
    distinct()
}

ui <- fluidPage(
  fluidRow(
    column(4,
      leafletOutput("cat_map")
    ),
    column(4,
      DTOutput("stations_table")
    ),
    column(4,
      DTOutput("variables_table")
    )
  ),
  fluidRow(
    column(4,
      plotOutput("distribution")   
    ),
    column(8,
      plotOutput("ts")     
    )
  )
)

server <- function(input, output, session){
  
  output$cat_map <-  renderLeaflet({
    
    leaflet(borders) |> 
      addProviderTiles("Esri.WorldImagery") |> 
      addPolygons(
        data = borders,
        layerId = ~nom_comar,
        fillColor = "blue",
        fillOpacity = 0.3,  
        color = "red",
        weight = 2,
        highlightOptions = highlightOptions(color = "white", weight = 3,
                                            bringToFront = TRUE)
      ) 
  })
  
  clicked_border <- reactiveVal(NULL)
  clicked_station <- reactiveVal(NULL)

  observeEvent(input$cat_map_shape_click, { 
    click <- input$cat_map_shape_click
    # Delete previous comarca selection before saving the new one
    if(!is.null(clicked_border())){
      leafletProxy("cat_map") |> 
        addPolygons(
          data = borders[borders$nom_comar == clicked_border(), ],
          layerId = clicked_border(),
          fillColor = "blue",
          fillOpacity = 0.3,  
          color = "red",
          weight = 2,
          highlightOptions = highlightOptions(color = "white", weight = 3,
                                              bringToFront = TRUE)
        )
    }
    clicked_border(click$id)
    
    # Highlight new selection
    leafletProxy("cat_map") |> 
      addPolygons(
        data = borders[borders$nom_comar == clicked_border(), ],
        layerId = clicked_border(),
        fillColor = "transparent",
        fillOpacity = 0.0,
        color = "red",
        weight = 2
      )
    
    output$stations_table <- renderDataTable({
      getStations(clicked_border()) |> 
        select(CODI_ESTACIO,NOM_ESTACIO,lat,lon) |> 
        datatable(selection = "single")
    })
    
    # Reset variables_table
    output$variables_table <- renderDataTable({
      NULL
    })
    
    # Reset plots
    output$distribution <- renderPlot(NULL)
    output$ts <- renderPlot(NULL)
    
    # Reset clicked_station
    clicked_station(NULL)
    
    # Reset markers
    leafletProxy("cat_map") |> 
      clearMarkers()
    
  })
  
  observeEvent(input$stations_table_rows_selected, {
    selected_row <- input$stations_table_rows_selected
    if (length(selected_row) > 0) {
      selectedStation <- getStations(clicked_border())[selected_row,]
      clicked_station(selectedStation$CODI_ESTACIO)
      
      output$variables_table <- renderDataTable({
        getStationVariables(clicked_station()) |> 
          datatable(selection = "single")
      })
    }
    
    # Add station's marker
    leafletProxy("cat_map") |> 
      clearMarkers()
    station_popup <- as.character(tagList(
      tags$h4(selectedStation$NOM_ESTACIO),
      glue("Codi estació: {selectedStation$CODI_ESTACIO}"), 
      tags$br(),
      glue("Municipi: {selectedStation$NOM_MUNICIPI}"), 
      tags$br(),
      glue("Emplaçament: {selectedStation$EMPLACAMENT}")
    ))
    leafletProxy("cat_map") |> 
      addMarkers(lat = selectedStation$lat, lng = selectedStation$lon,
                 popup = station_popup)
    
    # Reset plots
    output$distribution <- renderPlot(NULL)
    output$ts <- renderPlot(NULL)
    
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
    
    selected_data <- data |> 
      filter(CODI_VARIABLE == selectedVariable$CODI_VARIABLE,
             CODI_ESTACIO == clicked_station()) |> 
      select(DATA_LECTURA,UNITAT,valor)
    
    selectedStation <- stations |> 
      filter(CODI_ESTACIO == clicked_station())
    
    set_theme(theme_minimal())
    update_theme(
      plot.title = element_text(size=22),
      plot.subtitle = element_text(size=16)
    )
    
    output$distribution <- renderPlot({
      selected_data |> 
        ggplot(aes(x = valor)) +
          geom_histogram(bins = 50) +
          labs(
            title = glue("Distribució de {selectedVariable$NOM_VARIABLE} a {clicked_border()}"),
            subtitle = glue("Estació {selectedStation$NOM_ESTACIO} entre els dies {min(selected_data$DATA_LECTURA)} i {max(selected_data$DATA_LECTURA)}"),
            x = selectedVariable$NOM_VARIABLE, y = "Freqüència"
          )
    }) 
    
    output$ts <- renderPlot({
      selected_data |> 
        ggplot(aes(x = DATA_LECTURA,y = valor,fill = valor)) +
          geom_col() +
          theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            
          ) +
          labs(
            title = glue("Evolució de {selectedVariable$NOM_VARIABLE} a {clicked_border()}"),
            subtitle = glue("Estació {selectedStation$NOM_ESTACIO} entre els dies {min(selected_data$DATA_LECTURA)} i {max(selected_data$DATA_LECTURA)}"),
            x = "Data (dia)", y = selectedVariable$NOM_VARIABLE
          )
    }) 
    
  })
  
}


shinyApp(ui, server)
