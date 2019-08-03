
# Paquetes
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)

# Leer la DB
DB <- read_xlsx("C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/Analisis en R/DBRutasNormalizado.xlsx")

group_by()

table(DB$Origen,DB$Destino)

hist(DB$`Distancia Km`)

hist(DB$`Tiempo Min`)

scatte

table(DB$`Ruta 1`)/length(DB$`Ruta 1`)

summary(DB$`Distancia Km`,  na.rm = TRUE)

summary(DB$`Tiempo Min`,  na.rm = TRUE)

summary(DB$Velocidad,  na.rm = TRUE)

summary(DB$Velocidad,  na.rm = TRUE)

ggplot(data = DB) + 
  geom_point(mapping = aes(y = DB$Velocidad, x = DB$`Distancia Km`, size= DB$`Ruta 1`))

ggplot(data = DB) + 
  geom_point(mapping = aes(y = DB$Destino, x = DB$Origen, color=DB$`Rutas GoogleMaps`, size=DB$Velocidad))


ggplot(data = DB) + 
  geom_point(mapping = aes(x = DB$Origen, y = DB$Destino)) + 
  facet_wrap(~ DB$`Ruta 1`, nrow = 2)

ggplot(data = DB) + 
  stat_summary(
    mapping = aes(x = DB$`Rutas GoogleMaps`, y = DB$`Tiempo Min`),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = DB) + 
  geom_bar(mapping = aes(x = DB$Velocidad, colour = DB$`Distancia Km`))

ggplot(data = DB) + 
  geom_bar(mapping = aes(x = DB$Velocidad, fill = DB$`Distancia Km`))

ggplot(data = DB, mapping = aes(x=DB$`Rutas GoogleMaps`, y=DB$Velocidad)) + 
  geom_boxplot()

Box=ggplot(data = DB, mapping = aes(x=DB$`Rutas GoogleMaps`, y=DB$`Tiempo Min`)) + 
  geom_boxplot()

Grafico2 = Box + 
  theme (text = element_text(size=8)) + # Tamaño de fuente del grafico por defecto
  ggtitle ("Boxplot Opciones de Rutas GoogleMaps") + # Título del gráfico
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(2), #Tamaño relativo de la letra del título
                                   vjust=2, #Justificación vertical, para separarlo del gráfico
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="blue", #Color del texto
                                   lineheight=1.5)) + #Separación entre líneas
  labs(x = "Rutas",y = "Tiempo(min)") + # Etiquetas o títulos de los ejes
  #theme(axis.title = element_text(face="italic", colour="brown", size=rel(1.5))) # Tamaño de los títulos de los ejes
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="blue", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1.5))) 

Grafico2

ggplot(data = DB) + 
  geom_histogram(mapping = aes(x = DB$Velocidad))

# A scatter plot has been made for you
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()

# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

ggplot(diamonds, aes(x = carat, y = price, color=clarity))  +
  geom_point(alpha=0.4)

