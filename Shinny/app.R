##### Init #####
library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)
library(sf)
library(leaflet)
library(RColorBrewer)
library(DT)

# Cargar datos
Ubicacion <- readRDS(file.path("Ubicacion.rds"))
Rutas     <- readRDS(file.path("Rutas.rds"))
Puntos    <- readRDS(file.path("Puntos.rds"))
Camaras   <- readRDS(file.path("Camaras.rds"))

##### UI #####
ui <- navbarPage("Rutas de viajes", id = "nav",
  # Mapa -----------------------------------------------------------------------
  tabPanel("Mapa",
    div(class = "outer",
      tags$head(includeCSS("styles.css")),
      leafletOutput("mapa", width = "100%", height = "100%"),
      # Filtros ----------------------------------------------------------------
      absolutePanel(id = "filtros", class = "panel panel-default",
        fixed = TRUE, draggable = TRUE, width = 330, height = "auto",
        top = 60, bottom = "auto", left = "auto", right = 20,
        h2("Explorar viajes"),
        selectInput("ViajeId", "Viaje", selected = "",
          choices = c("Todos" = "", set_names(str_sub(Rutas$ViajeId, 2, 9),
                                              # Rutas$Cod_Viaje))),
                                              Rutas$ViajeId))),
        selectInput("OrigenId", "Origen:", selected = "",
          choices = c("Todos" = "", set_names(str_sub(Ubicacion$Id, 2, 9),
                                              Ubicacion$Nombre))),
        selectInput("Zona_Origen", "Zona de Origen:", selected = "",
          choices = c("Todas" = "", unique(Ubicacion$Zona))),
        selectInput("DestinoId", "Destino:", selected = "",
          choices = c("Todos" = "", set_names(str_sub(Ubicacion$Id, 2, 9),
                                              Ubicacion$Nombre))),
        selectInput("Camaras", "Camaras:", selected = "",
          choices = c("Todos" = "",  unique(Camaras$ID))),
                                            
        selectInput("Zona_Destino", "Zona de Destino:", selected = "",
          choices = c("Todas" = "", unique(Ubicacion$Zona))),
        selectInput("Dia", "Dia:", selected = "",
          choices = c("Todos" = "", levels(Rutas$Dia))),
        selectInput("Hora", "Hora:", selected = "",
          choices = c("Todos" = "", levels(Rutas$Meridiano))),
        selectInput("Horario", "Horario:", selected = "",
          choices = c("Todos" = "", levels(Rutas$Horario))),
        selectInput("Clima", "Clima:", selected = "",
          choices = c("Todos" = "", levels(Rutas$Clima))),
        selectInput("Congestion", "Nivel de congestion:", selected = "",
          choices = c("Todos" = "", levels(Rutas$Congestion))),
        selectInput("Pavimento", "Pavimento:", selected = "",
          choices = c("Todos" = "", levels(Rutas$Pavimento))),
        selectInput("Incidente", "Incidente:", selected = "",
          choices = c("Todos" = "", levels(Rutas$Incidente)))),
      uiOutput("GraficarPuntos"))),
  
  # Diagramas ------------------------------------------------------------------
  tabPanel("Diagramas",
           fluidRow(align = "center",
                    column(width = 6,
                           plotOutput("scatterPlot", height = 500),
                           fluidRow(
                             column(width = 6,
                                    selectInput("x", "Eje X:", selected = "Costo",
                                                choices = c("Costo",
                                                            "Duracion" = "Duracion",
                                                            "Distancia",
                                                            "Velocidad media" = "V_promedio",
                                                            "Velocidad total" = "V_total"))),
                             column(width = 6,
                                    selectInput("y", "Eje Y:", selected = "Duracion",
                                                choices = c("Costo",
                                                            "Duracion" = "Duracion",
                                                            "Distancia",
                                                            "Velocidad media" = "V_promedio",
                                                            "Velocidad total" = "V_total"))))),
                    column(width = 6,
                           plotOutput("boxPlot", height = 500),
                           fluidRow(
                             column(width = 6,
                                    selectInput("var", "Variable:", selected = "Distancia",
                                                choices = c("Costo",
                                                            "Duracion" = "Duracion",
                                                            "Distancia",
                                                            "Velocidad media" = "V_promedio",
                                                            "Velocidad total" = "V_total"))),
                             column(width = 6,
                                    selectInput("cat", "Categoria:", selected = "",
                                                choices = c(Todas = "",
                                                            "Zona de Origen" = "Zona_Origen",
                                                            "Zona de Destino" = "Zona_Destino",
                                                            "Dia",
                                                            "Hora" = "Meridiano",
                                                            "Horario",
                                                            "Clima",
                                                            "Nivel de congestion" = "Congestion",
                                                            "Pavimento",
                                                            "Incidente"))))))),
  # Datos ----------------------------------------------------------------------
  tabPanel("Datos", DTOutput("datos")))

##### Server #####
server <- function(input, output, session) {
  # Datos reactivos de Rutas (lineas) ------------------------------------------
  datosRutas <- reactive({
    filter(Rutas,
           str_detect(ViajeId, input$ViajeId),
           str_detect(OrigenId, input$OrigenId),
           str_detect(Zona_Origen, input$Zona_Origen),
           str_detect(DestinoId, input$DestinoId),
           str_detect(Zona_Destino, input$Zona_Destino),
           str_detect(Dia, input$Dia),
           str_detect(Meridiano, input$Hora),
           str_detect(Horario, input$Horario),
           str_detect(Clima, input$Clima),
           str_detect(Congestion, input$Congestion),
           str_detect(Pavimento, input$Pavimento),
           str_detect(Incidente, input$Incidente))
  })
  # Datos reactivos de Puntos --------------------------------------------------
  datosPuntos <- reactive({
    filter(Puntos,
           str_detect(ViajeId, input$ViajeId))
  })
  # Permitir seleccionar puntos unicamente si se selecciona un viaje -----------
  output$GraficarPuntos <- renderUI({
    if (nrow(datosRutas()) == 1) {
      absolutePanel(id = "checkboxPuntos", class = "panel panel-default",
                    fixed = TRUE, width = 150, height = "auto",
                    top = "auto", bottom = 10, left = 10, right = "auto",
                    checkboxInput("Puntos", "Graficar puntos"))
    }
  })
  # Crear una paleta de colores para los puntos del viaje seleccionado ---------
  colorpal <- reactive({
    if (isTruthy(input$Puntos)) {
      pts <- datosPuntos()
      colorNumeric("RdYlBu", pts$Velocidad)
    } else {
      rts <- datosRutas()
      colorNumeric("RdYlBu", rts$V_total)
    }
  })
  
  # Crear una paleta de colores para los puntos del viaje seleccionado ---------
  colorpalCam <- reactive({
    if (isTruthy(input$Camaras)) {
    ptscam <- datosCamaras()
    colorNumeric("RdYlBu", ptscam$VelMaxKmH)
  } else {
    rtscam <- datosCamaras()
    colorNumeric("RdYlBu", rtscam$VelMaxKmH)
  }
  })
  
  # Renderizar el mapa ---------------------------------------------------------
  output$mapa <- renderLeaflet({
    leaflet() %>%
      # Default basemap
      # addTiles() %>%
      # OpenStreetMap.DE
      # addProviderTiles(providers$OpenStreetMap.DE) %>%
      # CartoDB
      addProviderTiles(providers$CartoDB) %>%
      # Thunderforest.Transport
      # addProviderTiles(providers$Thunderforest.Transport,
      #                  options = providerTileOptions(apikey = Sys.getenv("api_key"))) %>%
      # Esri.WorldTopoMap
      # addProviderTiles(providers$Esri.WorldTopoMap) %>%
      fitBounds(lng1 = -75.61, lat1 = 6.18,
                lng2 = -75.55, lat2 = 6.30)
  })
  # Dibujar las lÃ­neas o puntos segÃÂºn los filtros ------------------------------
  observe({
    # Crear la paleta a partir de los datos existentes
    pal <- colorpal()
    # Dibujar las lineas de las rutas actuales
    leafletProxy("mapa") %>%
      clearShapes() %>%
      addPolylines(data    = datosRutas(),
                   color   = ~pal(V_promedio),
                   # color   = ~pal(V_total),
                   opacity = 0.7,
                   popup   = ~paste0("<b>Viaje:</b> ", ViajeId, "<br/>",
                                     "<b>Desde:</b> ", Nombre_Origen, "<br/>",
                                     "<b>Hasta:</b> ", Nombre_Destino, "<br/>",
                                     "<b>Costo:</b> $", Costo, "<br/>",
                                     "<b>Duracion:</b> ",
                                     round(Duracion, 1), "min", "<br/>",
                                     "<b>Distancia:</b> ",
                                     round(Distancia, 1), "km", "<br/>",
                                     "<b>Velocidad media:</b> ",
                                     round(V_promedio, 1), "km/h", "<br/>",
                                     "<b>Velocidad total:</b> ",
                                     round(V_total, 1), "km/h", "<br/>"))
    if (isTruthy(input$Puntos)) {
      leafletProxy("mapa") %>%
        clearShapes() %>%
        addCircles(data        = datosPuntos(),
                   radius      = 15,
                   color       = ~pal(Velocidad),
                   opacity     = 0.7,
                   fillColor   = ~pal(Velocidad),
                   fillOpacity = 0.7,
                   stroke      = FALSE,
                   popup       = ~paste0("<b>ID:</b> ", Id, "<br/>",
                                         "<b>Velocidad:</b> ",round(Velocidad, 2), "km/h"))
    }
  })
  
  
  
  # Funcion para poner las unidades en los plots -------------------------------
  unidades <- function(var) {
    switch(var,
           "Costo"      = number_format(prefix = "$"),
           "Duracion"   = unit_format(unit = "min"),
           "Distancia"  = unit_format(unit = "km"),
           "V_promedio" = unit_format(unit = "km/h"),
           "V_total"    = unit_format(unit = "km/h"))
  }
  # Renderizar el diagrama de dispersion----------------------------------------
  output$scatterPlot <- renderPlot({
    ggplot(datosRutas(),
           aes_string(x = input$x,
                      y = input$y)) +
      geom_point() +
      scale_x_continuous(labels = unidades(input$x)) +
      scale_y_continuous(labels = unidades(input$y))
  })
  # Renderizar el diagrama de violin con puntos dispersos ----------------------
  output$boxPlot <- renderPlot({
    if (input$cat == "") {
      boxPlot <- ggplot(datosRutas(),
                        aes_string(x = factor(0),
                                   y = input$var)) +
        theme(axis.title.y = element_blank(),
              axis.text.y  = element_blank(),
              axis.ticks.y = element_blank())
    } else {
      boxPlot <- ggplot(datosRutas(),
                        aes_string(x    = input$cat,
                                   y    = input$var,
                                   fill = input$cat))
    }
    boxPlot +
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
      geom_jitter(width = 0.25) +
      scale_y_continuous(labels = unidades(input$var))
  })
  # Renderizar los datos -------------------------------------------------------
  output$datos <- renderDT({
    datosRutas() %>%
      as.data.frame() %>%
      select(Cod_Viaje,
             Nombre_Origen, Zona_Origen,
             Nombre_Destino, Zona_Destino,
             Dia, Meridiano, Horario,
             Clima, Congestion,
             Pavimento, Incidente,
             Costo, Duracion, Distancia,
             V_promedio, V_total) %>%
      datatable(rownames = FALSE,
                options = list(dom = "Bltip",
                               buttons = I("colvis")),
                extensions = c("Responsive",
                               "Buttons"),
                colnames = c("Codigo Viaje"    = 1,
                             "Origen"          = 2,
                             "Zona de Origen"  = 3,
                             "Destino"         = 4,
                             "Zona de Destino" = 5,
                             "Congestion"      = 10,
                             "Duracion"        = 14,
                             "Velocidad media" = 16,
                             "Velocidad total" = 17),
                selection = "none") %>%
      formatCurrency("Costo") %>%
      formatRound("Duracion", 1) %>%
      formatString("Duracion", suffix = " min") %>%
      formatRound("Distancia", 1) %>%
      formatString("Distancia", suffix = " km") %>%
      formatRound(c("Velocidad media", "Velocidad total"), 0) %>%
      formatString(c("Velocidad media", "Velocidad total"), suffix = " km/h")
  })
}


##### Run #####
shinyApp(ui, server)
