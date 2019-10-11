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
#Camaras   <- readRDS(file.path("Camaras.rds"))

##### UI #####
ui <- tabPanel("Diagramas",
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
                                                                "Velocidad total" = "V_total")))
                                 ))))
  
# Datos ----------------------------------------------------------------------
tabPanel("Datos", DTOutput("datos"))

server <- function(input, output) {
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
  
  # Renderizar el diagrama de dispersion----------------------------------------
  output$scatterPlot <- renderPlot({
    ggplot(datosRutas(),
           aes_string(x = input$x,
                      y = input$y)) +
      geom_point() +
      scale_x_continuous(labels = unidades(input$x)) +
      scale_y_continuous(labels = unidades(input$y))
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
