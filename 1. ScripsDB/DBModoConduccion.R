
#Installing the Psych package and loading it
#install.packages("psych")
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

names(Viaje)

DBModo <- DB$Viajes %>%
  select(ViajeId) %>%
  # Modo conducción
  inner_join(DB$ModoConduccion %>%
               select(ViajeId:IgnoraSenhalPare,UsoCelular),
             by = "ViajeId") %>%
  inner_join(DB$CaracterizacionTaxista %>%
               select(ViajeId,Genero,InformacionPreviaDelTrafico),
             by = "ViajeId") %>%
  inner_join(DB$PersonalidadConductor %>%
               select(ViajeId,Accidente),
             by = "ViajeId")

#View(DBModo)
#head(DBModo)
names(DBModo)
attach(DBModo)
str(DBModo)

#Definimos las variables

DBModo <- rename(DBModo, replace = c(CinturonDeSeguridad = "CinSeg",
                                     PasoPeatones = "PasoPeaton",
                                     UsaPito = "UsoPito",
                                     FrenoRapidoBrusco = "FRbr",
                                     UsaDireccionales = "UsoDirec",
                                     EnfadoConOtroConductor = "EnfCond",
                                     AceleraFrenaBruscamenteSemaforo = "AFrSem",
                                     CulebreaConFrecuencia = "CulFr",
                                     OmiteLimiteVelocidad = "OmLmVel", 
                                     IgnoraSenhalPare = "IgPare",
                                     TranquilaTensionada = "StrC",
                                     RespetuosaIrrespetuosa = "ConCl",
                                     UsoCelular = "UsoCel",
                                     Accidente = "DispMob", 
                                     InformacionPreviaDelTrafico="INFOTRAFICO"))
                                   
names(DBModo)
str(DBModo$DispMob)

# Uso Celular 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Uso Celular cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$UsoCel[DBModo$UsoCel == 0] <-1
DBModo$UsoCel[DBModo$UsoCel == 4 & DBModo$DispMob > 0] <-2


# Freno Rapido y Brusco 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Freno Rapido y Brusco Cambia a: 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 2] <-2
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$OmLmVel == 2] <- 2
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$OmLmVel == 3] <- 3
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 4] <-1
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem ==1 & DBModo$CulFr == 2] <- 2
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem ==1 & DBModo$CulFr == 1] <- 1
DBModo$FRbr[DBModo$FRbr == 1 & DBModo$AFrSem ==2 ] <- 2
DBModo$FRbr[DBModo$FRbr == 1 & DBModo$AFrSem ==3 ] <- 3
DBModo$FRbr[DBModo$FRbr == 1 & DBModo$CulFr == 2] <- 2
DBModo$FRbr[DBModo$FRbr == 1 & DBModo$CulFr == 3] <- 3



# Acelera o frena bruscamente a la salida (llegada) de un semaforo 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Acelera o frena bruscamente a la salida (llegada) de un semaforo Cambi a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$AFrSem[DBModo$AFrSem == 4 & DBModo$FRbr == 1] <- 1
DBModo$AFrSem[DBModo$AFrSem == 4 & DBModo$FRbr == 2] <- 2
DBModo$AFrSem[DBModo$AFrSem == 1 & DBModo$FRbr == 2] <- 2
DBModo$AFrSem[DBModo$AFrSem == 1 & DBModo$FRbr == 3] <- 3
DBModo$AFrSem[DBModo$AFrSem == 1 & DBModo$CulFr == 2] <- 2
DBModo$AFrSem[DBModo$AFrSem == 1 & DBModo$FRbr == 3] <- 3

# Omite limite de Velocidad 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$OmLmVel[DBModo$OmLmVel == 4 & DBModo$CulFr == 2] <- 2
DBModo$OmLmVel[DBModo$OmLmVel == 4 & DBModo$AFrSem == 2] <- 3
DBModo$OmLmVel[DBModo$OmLmVel == 4 & DBModo$AFrSem == 1]<- 2
DBModo$OmLmVel[DBModo$OmLmVel == 1 & DBModo$AFrSem == 1]<- 2
DBModo$OmLmVel[DBModo$OmLmVel == 4 & DBModo$CulFr == 3]<- 3


#Ignora señal de pare 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$IgPare[DBModo$IgPare == 4 & DBModo$AFrSem == 1] <- 1
DBModo$IgPare[DBModo$IgPare == 4 & DBModo$AFrSem == 2] <- 2
DBModo$IgPare[DBModo$IgPare == 4 & DBModo$AFrSem == 3] <- 3
DBModo$IgPare[DBModo$IgPare == 1 & DBModo$AFrSem == 3] <- 3
DBModo$IgPare[DBModo$IgPare == 1 & DBModo$PasoPeaton == 1] <- 3
DBModo$IgPare[DBModo$IgPare == 1 & DBModo$PasoPeaton == 2] <- 2

#Usa las direccionales 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$UsoDirec[DBModo$UsoDirec == 4 & DBModo$CulFr == 3] <- 1
DBModo$UsoDirec[DBModo$UsoDirec == 4 & DBModo$CulFr == 2] <- 2

# Se enfada con otro conductor 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$EnfCond[DBModo$EnfCond == 4 & (DBModo$UsoPito == 1 | DBModo$AFrSem == 1)] <- 1
DBModo$EnfCond[DBModo$EnfCond == 4 & (DBModo$UsoPito == 2 | DBModo$AFrSem == 2)] <- 2
DBModo$EnfCond[DBModo$EnfCond == 4 & (DBModo$UsoPito == 3 | DBModo$AFrSem == 3)] <- 3

DBModo$UsoPito[DBModo$UsoPito == 1 & DBModo$EnfCond == 2] <- 2
DBModo$UsoPito[DBModo$UsoPito == 1 & DBModo$EnfCond == 3] <- 3
DBModo$UsoPito[DBModo$UsoPito == 1 & DBModo$AFrSem == 3] <- 3
DBModo$UsoPito[DBModo$UsoPito == 1 & DBModo$AFrSem == 2] <- 2
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$AFrSem == 2] <- 2
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$EnfCond == 2] <- 2
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$EnfCond == 3] <- 3
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$AFrSem == 3] <- 3


DBModo$INFOTRAFICO = factor(DBModo$INFOTRAFICO,
                              levels = 1:3,
                              labels = c("No",
                                         "SI",
                                         "No Responde"))

table(DBModo$INFOTRAFICO)
tab.Infotrafico <-as.data.frame(prop.table(table(DBModo$INFOTRAFICO))*100)
tab.Infotrafico

table(DBModo$CinSeg)
tab.Cinturon<-as.data.frame(prop.table(table(DBModo$CinSeg))*100)
tab.Cinturon

table(DBModo$PasoPeaton)
tab.Peaton<-as.data.frame(prop.table(table(DBModo$PasoPeaton))*100)
tab.Peaton

table(DBModo$UsoPito)
tab.UsaPito<-as.data.frame(prop.table(table(DBModo$UsoPito))*100)
tab.UsaPito

table(DBModo$FRbr)
tab.FRBr<-as.data.frame(prop.table(table(DBModo$FRbr))*100)
tab.FRBr


table(DBModo$UsoDirec)
tab.UsoDir<-as.data.frame(prop.table(table(DBModo$UsoDirec))*100)
tab.UsoDir

table(DBModo$EnfCond)
tab.EnfCond<-as.data.frame(prop.table(table(DBModo$EnfCond))*100)
tab.EnfCond

table(DBModo$AFrSem)
tab.AFrSem<-as.data.frame(prop.table(table(DBModo$AFrSem))*100)
tab.AFrSem

table(DBModo$CulFr)
tab.CulFr<-as.data.frame(prop.table(table(DBModo$CulFr))*100)
tab.CulFr

table(DBModo$OmLmVel)
tab.OmLmVel<-as.data.frame(prop.table(table(DBModo$OmLmVel))*100)
tab.OmLmVel

table(DBModo$IgPare)
tab.IgPare<-as.data.frame(prop.table(table(DBModo$IgPare))*100)
tab.IgPare

table(DBModo$UsoCel)
tab.UsoCel<-as.data.frame(prop.table(table(DBModo$UsoCel))*100)
tab.UsoCel


saveRDS(DBModo, file="/Users/williz/Desktop/ModelosED/Database/DBModoConduccion.rds")

                       
                       