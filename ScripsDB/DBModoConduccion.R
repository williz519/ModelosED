
#Installing the Psych package and loading it
install.packages("psych")
library(psych)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)
library(sqldf)
require(reshape)
require(dplyr)
require(psych)
require(GGally)
library(corrplot)
library(corrr)

#Leer la dataset

DB_file <- "/Users/williz/Desktop/rutasviajes/DB_Viajes.xlsx"

DB <- DB_file %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = DB_file)

# Zonificar las ubicaciones
Ubicacion <- DB$Ubicacion %>%
  separate(Nombre, c("Nombre", "Zona"),sep = " - Zona ") %>%
  arrange(Nombre) %>%
  mutate(Cod = 1:n()) %>%
  st_as_sf(coords = c("Longitud","Latitud")) %>%
  st_set_crs(4326)

Viaje <- DB$Viajes %>%
  select(ViajeId:CostoCarrera) %>%
  # Origen
  inner_join(Ubicacion %>%
               rename_all(paste, "Origen", sep = "_"),
             by = c("OrigenId" = "Id_Origen")) %>%
  # Destino
  inner_join(Ubicacion %>%
               rename_all(paste, "Destino", sep = "_"),
             by = c("DestinoId" = "Id_Destino")) %>%
  # Estado tráfico y condiciones
  inner_join(DB$EstadoTraficoCondiciones %>%
               select(ViajeId:TipoIncidente),
             by = "ViajeId") %>%
  # Modo conducción
  inner_join(DB$ModoConduccion %>%
               select(ViajeId:UsoCelular),
             by = "ViajeId") %>%
  # Caracterización taxista
  inner_join(DB$CaracterizacionTaxista %>%
               select(ViajeId:Edad),
             by = "ViajeId") %>%
  # Personalidad conductor
  inner_join(DB$PersonalidadConductor %>%
               select(ViajeId:AmbienteOrdenadoDesordenado),
             by = "ViajeId")

DBModo <- DB$Viajes %>%
  select(ViajeId) %>%
  # Modo conducción
  inner_join(DB$ModoConduccion %>%
               select(ViajeId:RespetuosaIrrespetuosa,UsoCelular),
             by = "ViajeId")
#View(DBModo)
#head(DBModo)
names(DBModo)
attach(DBModo)

#Definimos las variables
X <- data.frame(DBModo$ViajeId, DBModo$CinturonDeSeguridad,DBModo$PasoPeatones, DBModo$UsaPito, DBModo$FrenoRapidoBrusco,
           DBModo$UsaDireccionales, DBModo$EnfadoConOtroConductor, DBModo$AceleraFrenaBruscamenteSemaforo,
           DBModo$CulebreaConFrecuencia, DBModo$OmiteLimiteVelocidad, DBModo$IgnoraSenhalPare,
           DBModo$UsoCelular)

names(X)
X <- rename(X, replace =c(DBModo.ViajeId = "ViajeId",
                          DBModo.CinturonDeSeguridad = "CinturonDeSeguridad",
                          DBModo.PasoPeatones = "PasoPeatones",
                          DBModo.UsaPito = "UsaPito", 
                          DBModo.FrenoRapidoBrusco = "FrenoRapidoBrusco",
                          DBModo.UsaDireccionales = "UsaDireccionales", 
                          DBModo.EnfadoConOtroConductor = "EnfadoConOtroConductor", 
                          DBModo.AceleraFrenaBruscamenteSemaforo = "AceleraFrenaBruscamenteSemaforo",
                          DBModo.CulebreaConFrecuencia = "CulebreaConFrecuencia", 
                          DBModo.OmiteLimiteVelocidad = "OmiteLimiteVelocidad", 
                          DBModo.IgnoraSenhalPare = "IgnoraSenhalPare",
                          DBModo.UsoCelular = "UsoCelular"))

view(X)

# Cinturon de Seguridad
# X$CinturonDeSeguridad[X$CinturonDeSeguridad == 2] <-3
X$UsoCelular[X$UsoCelular == 4] <-1
X$UsoCelular[X$UsoCelular == 0] <-1

# Freno Rapido y Brusco
X$FrenoRapidoBrusco[X$FrenoRapidoBrusco ==4 & X$AceleraFrenaBruscamenteSemaforo == 2] <-2
X$FrenoRapidoBrusco[X$FrenoRapidoBrusco == 4 & X$OmiteLimiteVelocidad == 2] <- 2
X$FrenoRapidoBrusco[X$FrenoRapidoBrusco == 4 & X$OmiteLimiteVelocidad == 3] <- 3
X$FrenoRapidoBrusco[X$FrenoRapidoBrusco ==4 & X$AceleraFrenaBruscamenteSemaforo == 4] <-1
X$FrenoRapidoBrusco[X$FrenoRapidoBrusco == 4 & X$AceleraFrenaBruscamenteSemaforo ==1 & X$CulebreaConFrecuencia == 2] <- 2
X$FrenoRapidoBrusco[X$FrenoRapidoBrusco == 4 & X$AceleraFrenaBruscamenteSemaforo ==1 & X$CulebreaConFrecuencia == 1] <- 1


# Acelera o frena bruscamente a la salida (llegada) de un semaforo
X$AceleraFrenaBruscamenteSemaforo[X$AceleraFrenaBruscamenteSemaforo == 4 & X$FrenoRapidoBrusco == 1] <- 1
X$AceleraFrenaBruscamenteSemaforo[X$AceleraFrenaBruscamenteSemaforo == 4 & X$FrenoRapidoBrusco == 2] <- 2

# Omite limite de Velocidad
X$OmiteLimiteVelocidad[X$OmiteLimiteVelocidad == 4 & X$CulebreaConFrecuencia == 2] <- 2
X$OmiteLimiteVelocidad[X$OmiteLimiteVelocidad == 4 & X$AceleraFrenaBruscamenteSemaforo == 2] <- 3
X$OmiteLimiteVelocidad[X$OmiteLimiteVelocidad == 4 & X$AceleraFrenaBruscamenteSemaforo == 1]<- 2

#Ignora señal de pare
X$IgnoraSenhalPare[X$IgnoraSenhalPare == 4 & X$AceleraFrenaBruscamenteSemaforo == 1] <- 1
X$IgnoraSenhalPare[X$IgnoraSenhalPare == 4 & X$AceleraFrenaBruscamenteSemaforo == 2] <- 2
X$IgnoraSenhalPare[X$IgnoraSenhalPare == 4 & X$AceleraFrenaBruscamenteSemaforo == 3] <- 3


#Usa las direccionales
X$UsaDireccionales[X$UsaDireccionales == 4 & X$CulebreaConFrecuencia == 2] <- 2

# Se enfada con otro conductor
X$EnfadoConOtroConductor[X$EnfadoConOtroConductor == 4 & (X$UsaPito == 1 | X$AceleraFrenaBruscamenteSemaforo == 1)] <- 1
X$EnfadoConOtroConductor[X$EnfadoConOtroConductor == 4 & (X$UsaPito == 2 | X$AceleraFrenaBruscamenteSemaforo == 2)] <- 2
X$EnfadoConOtroConductor[X$EnfadoConOtroConductor == 4 & (X$UsaPito == 3 | X$AceleraFrenaBruscamenteSemaforo == 3)] <- 3


table(X$CinturonDeSeguridad)
table(X$PasoPeatones)
table(X$UsaPito)
table(X$FrenoRapidoBrusco)
table(X$UsaDireccionales)
table(X$EnfadoConOtroConductor)
table(X$AceleraFrenaBruscamenteSemaforo)
table(X$CulebreaConFrecuencia)
table(X$OmiteLimiteVelocidad)
table(X$IgnoraSenhalPare)
table(X$UsoCelular)



saveRDS(DBPersonalidad, file="/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModoConduccion.rds")

                       
                       