library(shiny)
library(leaflet)
library(sf)
library(readr)
library(stringr)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)
# box

# system installs
# libpoppler-cpp-dev
# cmake
# libgdal-dev
# libudunits2-dev

# Load configuration. Check if local exists, so both can coexist in the local environment
if (file.exists(box::file("config.R.local"))) {
  message("Loading local config file...")
  source(box::file("config.R.local"))
} else if (file.exists(box::file("config.R"))) {
  source(box::file("config.R"))
} else {
  stop("Neither config.R nor config.R.local found!")
}


borders <- st_read(file.path(getConfig()$geojson_data_path,"comarques-compressed.geojson"))

stations <- read_csv(file.path(getConfig()$xema_data_path,"stations_metadata.csv")) |> 
  mutate(
    lon = as.numeric(str_extract(Georeferència, "(?<=\\()\\s*-?\\d+\\.\\d+")),
    lat = as.numeric(str_extract(Georeferència, "-?\\d+\\.\\d+\\s*(?=\\))"))
  )

# The comarca Lluçanès does not exists in the borders used
stations[stations$CODI_ESTACIO == "V5",]$CODI_COMARCA <- 24
stations[stations$CODI_ESTACIO == "V5",]$NOM_COMARCA <- "Osona"

data <- read_csv(file.path(getConfig()$xema_data_path,"daily_stations_meteo_data.csv"),col_select = -c(ID,NOM_ESTACIO,`HORA _TU`)) |> 
  mutate(
    valor = str_remove(VALOR,"\\.") |> 
      str_replace("\\,","\\.") |> 
      as.numeric(),
    .keep = "unused"
  )

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

leaflet_styles <- list(
  fillColor = "blue",
  fillOpacity = 0.3,  
  color = "red",
  weight = 2
)

# ggplot theme global settings
set_theme(theme_minimal())
update_theme(
  plot.title = element_text(size=22),
  plot.subtitle = element_text(size=16)
)

ui <- fluidPage(
  h1("Consulta de dades diàries de la xarxa XEMA"),
  h4("Escull una comarca, una estació i una variable."),
  fluidRow(
    column(6,
      leafletOutput("cat_map", height = "600px")
    ),
    column(4,
      DTOutput("stations_table")
    ),
    column(2,
      DTOutput("variables_table")
    )
  ),
  fluidRow(
    h3(textOutput("plots_title"))
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
        fillColor = leaflet_styles$fillColor,
        fillOpacity = leaflet_styles$fillOpacity,  
        color = leaflet_styles$color,
        weight = leaflet_styles$weight,
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
          fillColor = leaflet_styles$fillColor,
          fillOpacity = leaflet_styles$fillOpacity,  
          color = leaflet_styles$color,
          weight = leaflet_styles$weight,
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
        color = "yellow",
        weight = 4
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
    output$plots_title <- renderText(NULL)
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
          select(NOM_VARIABLE) |> 
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
    output$plots_title <- renderText(NULL)
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
    
    output$plots_title <- renderText({
      glue("Distribució i evolució de {selectedVariable$NOM_VARIABLE} a l'estació {selectedStation$NOM_ESTACIO} a {clicked_border()} entre els dies {min(selected_data$DATA_LECTURA)} i {max(selected_data$DATA_LECTURA)}")
    })
    
    output$distribution <- renderPlot({
      selected_data |> 
        ggplot(aes(x = valor)) +
          geom_histogram(bins = 50) +
          labs(
            title = glue("Distribució"),
            #subtitle = glue("Estació {selectedStation$NOM_ESTACIO} entre els dies {min(selected_data$DATA_LECTURA)} i {max(selected_data$DATA_LECTURA)}"),
            x = selectedVariable$NOM_VARIABLE, y = "Freqüència"
          )
    }) 
    
    output$ts <- renderPlot({
      selected_data |> 
        ggplot(aes(x = DATA_LECTURA,y = valor,fill = valor)) +
          geom_col() +
          theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          ) +
          scale_fill_continuous(name = selectedVariable$NOM_VARIABLE) +
          labs(
            title = glue("Evolució"),
            # subtitle = glue("Estació {selectedStation$NOM_ESTACIO} entre els dies {min(selected_data$DATA_LECTURA)} i {max(selected_data$DATA_LECTURA)}"),
            x = "Data (dia)", y = selectedVariable$NOM_VARIABLE
          )
    }) 
    
  })
  
}


shinyApp(ui, server)
