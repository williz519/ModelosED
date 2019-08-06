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

DB_file <- "/Users/williz/Desktop/ModelosED/Database/DB_Viajes.xlsx"

DB <- DB_file %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = DB_file)

DBPersonalidad <- DB$Viajes %>%
  select(ViajeId, IdViaje) %>%
  # Caracterización taxista
  inner_join(DB$CaracterizacionTaxista %>%
               select(ViajeId, #Edad,#TiempoProfesion,#HorasAlDia,
                      NivelEducativo),
             by = "ViajeId") %>%
  # Personalidad conductor
  inner_join(DB$PersonalidadConductor %>%
               select(ViajeId:AmbienteOrdenadoDesordenado),
             by = "ViajeId") %>%
  # Modo de Conduccion
  inner_join(DB$ModoConduccion %>%
               select(ViajeId,TranquilaTensionada,RespetuosaIrrespetuosa),
             by = "ViajeId")

View(DBPersonalidad)
names(DBPersonalidad)

DBPersonalidad <- rename(DBPersonalidad, replace =c(Accidente = "DispMobiles", 
                                                    Atraco = "SatisfDispMob",
                                                    SerioConservador = "ComunicVerbal",
                                                    RelajadoTenso = "Ansiedad",
                                                    AmableAntipatico = "ComunicAfectiva",
                                                    OrdenadoDesordenado = "PresPersonal",
                                                    AmbienteOrdenadoDesordenado = "AmbTrabajo",
                                                    TranquilaTensionada = "StressAlCond",
                                                    RespetuosaIrrespetuosa = "ConsidCliente"))

names(DBPersonalidad)

DBPersonalidad <- DBPersonalidad%>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(ViajeId %in% c("{3F7B556D-577A-E811-B124-74867AD5B714}",
                          "{C7437B00-427A-E811-B124-74867AD5B714}",
                          "{7CBCE319-CD3D-E811-9CE5-74867AD5B714}",
                          "{6C895145-D23D-E811-9CE5-74867AD5B714}",
                          "{DC497116-05A7-E811-BDF0-74867AD5B714}",
                          "{D17203CB-4ACE-E811-914C-74867AD5B714}",
                          "{0FB33E62-4ECE-E811-914C-74867AD5B714}",
                          "{3C43C21E-76DB-E811-8FB7-74867AD5B714}",
                          "{3D43C21E-76DB-E811-8FB7-74867AD5B714}",
                          "{39877BE2-6B1D-E811-9CE5-74867AD5B714}")))

view(DBPersonalidad)

# Eliminar Variables 
DBPersonalidad <- DBPersonalidad[ ,!colnames(DBPersonalidad)=="IdViaje"]

#Reemplazar Valores Faltantes

DBPersonalidad$ComunicVerbal[is.na(DBPersonalidad$ComunicVerbal)] <- 5
DBPersonalidad$Ansiedad[is.na(DBPersonalidad$Ansiedad)] <- 5
DBPersonalidad$ComunicAfectiva[is.na(DBPersonalidad$ComunicAfectiva)] <- 5
DBPersonalidad$PresPersonal[is.na(DBPersonalidad$PresPersonal)] <- 5
DBPersonalidad$AmbTrabajo[is.na(DBPersonalidad$AmbTrabajo)] <- 5
DBPersonalidad$StressAlCond[is.na(DBPersonalidad$StressAlCond)] <- 5
DBPersonalidad$Ansiedad[is.na(DBPersonalidad$Ansiedad)] <- 5
DBPersonalidad$ConsidCliente[is.na(DBPersonalidad$ConsidCliente)] <- 5



#CAMBIO DE ESCALA
# Comunicacion Verbal
#DBPersonalidad$ComunicVerbal[DBPersonalidad$ComunicVerbal == 1 | DBPersonalidad$ComunicVerbal == 2 | DBPersonalidad$ComunicVerbal == 3] <- 1
#DBPersonalidad$ComunicVerbal[DBPersonalidad$ComunicVerbal == 4 | DBPersonalidad$ComunicVerbal == 5 | DBPersonalidad$ComunicVerbal == 6 | DBPersonalidad$ComunicVerbal == 7 ] <- 2
#DBPersonalidad$ComunicVerbal[DBPersonalidad$ComunicVerbal == 8 | DBPersonalidad$ComunicVerbal == 9 | DBPersonalidad$ComunicVerbal == 10] <- 3

#Manejo del Stress
#DBPersonalidad$Ansiedad[DBPersonalidad$Ansiedad == 1 | DBPersonalidad$Ansiedad == 2 | DBPersonalidad$Ansiedad == 3] <- 1
#DBPersonalidad$Ansiedad[DBPersonalidad$Ansiedad == 4 | DBPersonalidad$Ansiedad == 5 | DBPersonalidad$Ansiedad == 6 | DBPersonalidad$Ansiedad == 7 ] <- 2
#DBPersonalidad$Ansiedad[DBPersonalidad$Ansiedad == 8 | DBPersonalidad$Ansiedad == 9 | DBPersonalidad$Ansiedad == 10] <- 3

#Comunicacion Afectiva
#DBPersonalidad$ComunicAfectiva[DBPersonalidad$ComunicAfectiva == 1 | DBPersonalidad$ComunicAfectiva == 2 | DBPersonalidad$ComunicAfectiva == 3] <- 1
#DBPersonalidad$ComunicAfectiva[DBPersonalidad$ComunicAfectiva == 4 | DBPersonalidad$ComunicAfectiva == 5 | DBPersonalidad$ComunicAfectiva == 6 | DBPersonalidad$ComunicAfectiva == 7 ] <- 2
#DBPersonalidad$ComunicAfectiva[DBPersonalidad$ComunicAfectiva == 8 | DBPersonalidad$ComunicAfectiva == 9 | DBPersonalidad$ComunicAfectiva == 10] <- 3

#Presentacion Personal
#DBPersonalidad$PresPersonal[DBPersonalidad$PresPersonal == 1 | DBPersonalidad$PresPersonal == 2 | DBPersonalidad$PresPersonal == 3] <- 1
#DBPersonalidad$PresPersonal[DBPersonalidad$PresPersonal == 4 | DBPersonalidad$PresPersonal == 5 | DBPersonalidad$PresPersonal == 6 | DBPersonalidad$PresPersonal == 7 ] <- 2
#DBPersonalidad$PresPersonal[DBPersonalidad$PresPersonal == 8 | DBPersonalidad$PresPersonal == 9 | DBPersonalidad$PresPersonal == 10] <- 3

# Ambiente de Trabajo
#DBPersonalidad$AmbTrabajo[DBPersonalidad$AmbTrabajo == 1 | DBPersonalidad$AmbTrabajo == 2 | DBPersonalidad$AmbTrabajo == 3] <- 1
#DBPersonalidad$AmbTrabajo[DBPersonalidad$AmbTrabajo == 4 | DBPersonalidad$AmbTrabajo == 5 | DBPersonalidad$AmbTrabajo == 6 | DBPersonalidad$AmbTrabajo == 7] <- 2
#DBPersonalidad$AmbTrabajo[DBPersonalidad$AmbTrabajo == 8 | DBPersonalidad$AmbTrabajo == 9 | DBPersonalidad$AmbTrabajo == 10] <- 3

# Stress Al Conducir
#DBPersonalidad$StressAlCond[DBPersonalidad$StressAlCond == 1 | DBPersonalidad$StressAlCond == 2 | DBPersonalidad$StressAlCond == 3] <- 1
#DBPersonalidad$StressAlCond[DBPersonalidad$StressAlCond == 4 | DBPersonalidad$StressAlCond == 5 | DBPersonalidad$StressAlCond == 6 | DBPersonalidad$StressAlCond == 7] <-2
#DBPersonalidad$StressAlCond[DBPersonalidad$StressAlCond == 8 | DBPersonalidad$StressAlCond == 9 | DBPersonalidad$StressAlCond == 10] <- 3

#Habilidades Prosociales
#DBPersonalidad$ConsidCliente[DBPersonalidad$ConsidCliente == 1 | DBPersonalidad$ConsidCliente == 2 | DBPersonalidad$ConsidCliente == 3] <- 1
#DBPersonalidad$ConsidCliente[DBPersonalidad$ConsidCliente == 4 | DBPersonalidad$ConsidCliente == 5 | DBPersonalidad$ConsidCliente == 6 | DBPersonalidad$ConsidCliente == 7] <- 2
#DBPersonalidad$ConsidCliente[DBPersonalidad$ConsidCliente == 8 | DBPersonalidad$ConsidCliente == 9 | DBPersonalidad$ConsidCliente == 10] <- 3


summary(DBPersonalidad)

str(DBPersonalidad)


saveRDS(DBPersonalidad, file="/Users/williz/Desktop/ModelosED/Database/DBPersonalidad.rds")
