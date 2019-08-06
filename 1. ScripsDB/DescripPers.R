
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)
library(dplyr)

library(readxl)
library(xlsx)
library(sqldf)


Personalidad <-read_xlsx("C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Base de datos/Rasgos Personalidad/Personalidad1.xlsx")



d.f <- Personalidad

head(d.f)

table(d.f$`Usa dispositivos moviles`)

prop.table(table(d.f$`Usa dispositivos moviles`))

#Satisfaccion con los dispositivos mobiles

table(d.f$`Satisfaccion con dm`)

Y=prop.table(table(d.f$`Satisfaccion con dm`))



hist(Y)


table(d.f$SerioConvervador)


prop.table(table(d.f$SerioConvervador))

prop.table(table(d.f$RelajadoTenso))

prop.table(table(d.f$AmableAntipatico))

prop.table(table(d.f$PresentacionOrdenadoDesordenado))

prop.table(table(d.f$AmbienteOrdenadoDesordenado))


