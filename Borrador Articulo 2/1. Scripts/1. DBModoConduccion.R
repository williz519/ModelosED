
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

# Limpiar Entorno de trabajo
rm(list = ls())

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
               select(ViajeId,Accidente,Atraco),
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
                                     Atraco = "Satisf_DispMob",
                                     InformacionPreviaDelTrafico="INFOTRAFICO"))
                                   
names(DBModo)
str(DBModo)
str(DBModo$DispMob)

# Uso Celular 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Uso Celular cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$UsoCel[DBModo$UsoCel == 0] <-1
DBModo$UsoCel[DBModo$UsoCel == 4 & DBModo$DispMob > 0] <-2
DBModo$UsoCel[DBModo$UsoCel == 4 & DBModo$DispMob == 0] <-1
table(DBModo$UsoCel)


table(DBModo$FRbr)
# Freno Rapido y Brusco 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Freno Rapido y Brusco Cambia a: 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 2 & DBModo$CulFr == 2] <- 3
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 2 & DBModo$CulFr == 1] <- 2
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 1 & DBModo$CulFr == 2] <- 2
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 1 & DBModo$CulFr == 1] <- 1
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 2] <-2
DBModo$FRbr[DBModo$FRbr == 4 & DBModo$AFrSem == 4] <-1
table(DBModo$FRbr)

table(DBModo$AFrSem)
# Acelera o frena bruscamente a la salida (llegada) de un semaforo 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Acelera o frena bruscamente a la salida (llegada) de un semaforo Cambi a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$AFrSem[DBModo$AFrSem == 4 & DBModo$FRbr == 2] <- 2
DBModo$AFrSem[DBModo$AFrSem == 4 & DBModo$FRbr == 1] <- 1
table(DBModo$AFrSem)

table(DBModo$OmLmVel)
# Omite limite de Velocidad 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$OmLmVel[DBModo$OmLmVel == 4 & DBModo$AFrSem == 2] <- 3
DBModo$OmLmVel[DBModo$OmLmVel == 4 & DBModo$CulFr == 2] <- 2
DBModo$OmLmVel[DBModo$OmLmVel == 4 & DBModo$AFrSem == 1]<- 2
table(DBModo$OmLmVel)

table(DBModo$IgPare)
#Ignora señal de pare 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$IgPare[DBModo$IgPare == 4 & DBModo$OmLmVel == 3] <- 3
DBModo$IgPare[DBModo$IgPare == 4 & DBModo$OmLmVel == 2] <- 2
DBModo$IgPare[DBModo$IgPare == 4 & DBModo$AFrSem == 2] <- 2
table(DBModo$IgPare)

table(DBModo$UsoDirec)
#Usa las direccionales 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$UsoDirec[DBModo$UsoDirec == 4 & DBModo$CulFr == 2] <- 2
DBModo$UsoDirec[DBModo$UsoDirec == 4 ] <- 2
table(DBModo$UsoDirec)

table(DBModo$EnfCond)
# Se enfada con otro conductor 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$EnfCond[DBModo$EnfCond == 4 & (DBModo$UsoPito == 3 | DBModo$AFrSem == 3)] <- 3
DBModo$EnfCond[DBModo$EnfCond == 4 & (DBModo$UsoPito == 2 | DBModo$AFrSem == 2)] <- 2
DBModo$EnfCond[DBModo$EnfCond == 4 & (DBModo$UsoPito == 1 | DBModo$AFrSem == 1)] <- 1
table(DBModo$EnfCond)

table(DBModo$CulFr)
# Culebrea con frecuencia 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$CulFr[DBModo$CulFr == 4 ] <- 3
table(DBModo$CulFr)

table(DBModo$UsoPito)
# Culebrea con frecuencia 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$PasoPeaton == 1] <- 3
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$PasoPeaton == 2] <- 2
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$PasoPeaton == 3] <- 1
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$IgPare == 1] <- 1
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$OmLmVel == 2] <- 2
DBModo$UsoPito[DBModo$UsoPito == 4 & DBModo$AFrSem == 2] <- 2
DBModo$UsoPito[DBModo$UsoPito == 4 ] <- 3
table(DBModo$UsoPito)

table(DBModo$PasoPeaton)
# Paso Peaton 1: Nunca, 2: Algunas veces, 3: Siempre, 4: No se observo
# Cambia a 1: Nunca, 2: Algunas veces, 3: Siempre
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$UsoPito == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$AFrSem == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$OmLmVel == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$FRbr == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$IgPare == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$CulFr == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$EnfCond == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$IgPare == 2] <- 2
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$UsoPito == 2] <- 2
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$AFrSem == 2] <- 2
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$OmLmVel == 2] <- 2
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$CulFr == 2] <- 2
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$EnfCond == 3] <- 1
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$EnfCond == 2] <- 2
DBModo$PasoPeaton[DBModo$PasoPeaton == 4 & DBModo$AFrSem == 1] <- 3
table(DBModo$PasoPeaton)

table(DBModo$DispMob)
DBModo$DispMob[DBModo$DispMob == 0 & DBModo$Satisf_DispMob == 0] <- 1
DBModo$DispMob[DBModo$DispMob == 0 & DBModo$Satisf_DispMob > 0] <- 2
table(DBModo$DispMob)



summary(DBModo)

#DBModo[c("ViajeId")]<-NULL
#cor(DBModo, use = "pairwise.complete.obs")


summary(DBModo)
names(DBModo)


#tibble::as_tibble(DBModo) 

saveRDS(DBModo, file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBModoConduccion.rds")


# Rutina Cambio de escala de 1-5

DBModoCE <-read_xlsx("/Users/williz/Desktop/ModelosED/Bases de Datos Ordenada/DBCEscala.xlsx",
                     sheet = "ModoCond")

DBCE <- DBModo %>%
  select(ViajeId:Satisf_DispMob) %>%
  # Modo conducción
  inner_join(DBModoCE %>%
               select(ViajeId,aleatorio),
             by = "ViajeId")


#Paso a peatones
DBCE$PasoPeaton <-ifelse(DBCE$PasoPeaton == 3,
                       ifelse(DBCE$aleatorio<0.5,4,5),
                       ifelse(DBCE$PasoPeaton ==2,
                              ifelse(DBCE$aleatorio<0.33, 2,
                                     ifelse(DBCE$aleatorio>0.66,4,3)),
                              ifelse(DBCE$PasoPeaton == 1,
                                     ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))
    

DBCE$PasoPeaton 
table(DBCE$PasoPeaton)
table(DBCE$PasoPeaton)
tab.PasoPeaton <-as.data.frame(prop.table(table(DBCE$PasoPeaton))*100)
tab.PasoPeaton

# Uso Pito
DBCE$UsoPito <-ifelse(DBCE$UsoPito == 3,
                         ifelse(DBCE$aleatorio<0.5,4,5),
                         ifelse(DBCE$UsoPito ==2,
                                ifelse(DBCE$aleatorio<0.33, 2,
                                       ifelse(DBCE$aleatorio>0.66,4,3)),
                                ifelse(DBCE$UsoPito == 1,
                                       ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))


DBCE$UsoPito 
table(DBCE$UsoPito)
tab.UsoPito <-as.data.frame(prop.table(table(DBCE$UsoPito))*100)
tab.UsoPito

#FRbr
DBCE$FRbr <-ifelse(DBCE$FRbr == 3,
                      ifelse(DBCE$aleatorio<0.5,4,5),
                      ifelse(DBCE$FRbr ==2,
                             ifelse(DBCE$aleatorio<0.33, 2,
                                    ifelse(DBCE$aleatorio>0.66,4,3)),
                             ifelse(DBCE$FRbr == 1,
                                    ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))


DBCE$FRbr
table(DBCE$FRbr)
tab.FRbr <-as.data.frame(prop.table(table(DBCE$FRbr))*100)
tab.FRbr

#Uso direccionales
DBCE$UsoDirec <-ifelse(DBCE$UsoDirec == 3,
                   ifelse(DBCE$aleatorio<0.5,4,5),
                   ifelse(DBCE$UsoDirec ==2,
                          ifelse(DBCE$aleatorio<0.33, 2,
                                 ifelse(DBCE$aleatorio>0.66,4,3)),
                          ifelse(DBCE$UsoDirec == 1,
                                 ifelse(DBCE$aleatorio < 0.7, 1, 2),2)))


DBCE$UsoDirec
table(DBCE$UsoDirec)
tab.UsoDirec <-as.data.frame(prop.table(table(DBCE$UsoDirec))*100)
tab.UsoDirec

# Enfada con otros conductores
DBCE$EnfCond <-ifelse(DBCE$EnfCond == 3,
                       ifelse(DBCE$aleatorio<0.5,4,5),
                       ifelse(DBCE$EnfCond ==2,
                              ifelse(DBCE$aleatorio<0.33, 2,
                                     ifelse(DBCE$aleatorio>0.66,4,3)),
                              ifelse(DBCE$EnfCond == 1,
                                     ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))


DBCE$EnfCond
table(DBCE$EnfCond)
tab.EnfCond <-as.data.frame(prop.table(table(DBCE$EnfCond))*100)
tab.EnfCond

#Acelera o frena brusco en semaforo
DBCE$AFrSem <-ifelse(DBCE$AFrSem == 3,
                       ifelse(DBCE$aleatorio<0.5,4,5),
                       ifelse(DBCE$AFrSem ==2,
                              ifelse(DBCE$aleatorio<0.33, 2,
                                     ifelse(DBCE$aleatorio>0.66,4,3)),
                              ifelse(DBCE$AFrSem == 1,
                                     ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))


DBCE$AFrSem
table(DBCE$AFrSem)
tab.AFrSem <-as.data.frame(prop.table(table(DBCE$AFrSem))*100)
tab.AFrSem

# Culebrea con frecuencia
DBCE$CulFr <-ifelse(DBCE$CulFr == 3,
                       ifelse(DBCE$aleatorio<0.5,4,5),
                       ifelse(DBCE$CulFr ==2,
                              ifelse(DBCE$aleatorio<0.33, 2,
                                     ifelse(DBCE$aleatorio>0.66,4,3)),
                              ifelse(DBCE$CulFr == 1,
                                     ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))


DBCE$CulFr
table(DBCE$CulFr)
tab.CulFr <-as.data.frame(prop.table(table(DBCE$CulFr))*100)
tab.CulFr

# Omite Limite de Velocidad
DBCE$OmLmVel <-ifelse(DBCE$OmLmVel == 3,
                       ifelse(DBCE$aleatorio<0.5,4,5),
                       ifelse(DBCE$OmLmVel ==2,
                              ifelse(DBCE$aleatorio<0.33, 2,
                                     ifelse(DBCE$aleatorio>0.66,4,3)),
                              ifelse(DBCE$OmLmVel == 1,
                                     ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))


DBCE$OmLmVel
table(DBCE$OmLmVel)
tab.OmLmVel <-as.data.frame(prop.table(table(DBCE$OmLmVel))*100)
tab.OmLmVel

# Ignora Señales de Pare
DBCE$IgPare <-ifelse(DBCE$IgPare == 3,
                       ifelse(DBCE$aleatorio<0.5,4,5),
                       ifelse(DBCE$IgPare ==2,
                              ifelse(DBCE$aleatorio<0.33, 2,
                                     ifelse(DBCE$aleatorio>0.66,4,3)),
                              ifelse(DBCE$IgPare == 1,
                                     ifelse(DBCE$aleatorio < 0.5, 1, 2),2)))


DBCE$IgPare
table(DBCE$IgPare)
tab.IgPare <-as.data.frame(prop.table(table(DBCE$IgPare))*100)
tab.IgPare

#
DBCE$UsoCel <-ifelse(DBCE$UsoCel == 3,
                       ifelse(DBCE$aleatorio<0.4,4,5),
                       ifelse(DBCE$UsoCel ==2,
                              ifelse(DBCE$aleatorio<0.25, 2,
                                     ifelse(DBCE$aleatorio>0.6,4,3)),
                              ifelse(DBCE$UsoCel == 1,
                                     ifelse(DBCE$aleatorio < 0.4, 1, 2),2)))


DBCE$UsoCel
table(DBCE$UsoCel)
tab.UsoCel <-as.data.frame(prop.table(table(DBCE$UsoCel))*100)
tab.UsoCel

tab.CinSeg <-as.data.frame(prop.table(table(DBCE$CinSeg))*100)
tab.CinSeg


names(DBCE)

DBCE[c("aleatorio")]<-NULL

saveRDS(DBCE, file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBModoCondCE.rds")





                       
                       