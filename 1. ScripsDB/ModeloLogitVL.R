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
require(reshape)
require(dplyr)
require(psych)
require(GGally)

#Cargas las Bases de Datos
rm(list = ls())

Google <-read_xlsx("/Users/williz/Desktop/ModelosED/Database/DBRutasGoogle.xlsx")

DBCompleta <- read_xlsx("/Users/williz/Desktop/ModelosED/Bases de Datos Ordenada/ViajesCompleta_VersionFelipe.xlsx")

RutasGoogle <- Google %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(ViajeId %in% c("{3E49E5E4-D7D2-E811-8FB7-74867AD5B714}",
                          "{FBDD1371-1AC1-E811-914C-74867AD5B714}")))

Rutas200 <-readRDS("/Users/williz/Desktop/ModelosED/Database/RutasVelocidades/Rutas200.rds")

#Base de datos creada en DBModoConduccion.r Escala 1-3
#DBModoConduccion <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModoConduccion.rds")

#Base de datos creada en DBModoConduccion.r Escala 1-5
DBModoConduccion <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModoCondCE.rds")


#Base de datos creada en DBPersonalidad.r
DBPersonalidad <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBPersonalidad.rds")


names(Rutas200)
names(RutasGoogle)
names(DBModoConduccion)
names(DBPersonalidad)


RutasGoogle <- rename(RutasGoogle, replace = c("A1_Distancia Km" ="DISTAlt1", 
                      "A1_Tiempo Min"="TIEMPOAlt1",
                      A1_Color="CONG_A1", 
                      "A2_Distancia Km" ="DISTAlt2", 
                      "A2_Tiempo Min" ="TIEMPOAlt2", 
                      A2_Color="CONG_A2", 
                      "A3_Distancia Km"="DISTAlt3", 
                      "A3_Tiempo Min" ="TIEMPOAlt3",
                      A3_Color="CONG_A3", 
                      AT_Distancia="DISTEC", 
                      AT_Tiempo="TIEMPOEC", 
                      Costo="COSTO", 
                      Choice="CHOICE"))

RutasGoogle$CONG_A1 <- factor(RutasGoogle$CONG_A1,
                    levels = c("SinInf","Verde","Amarillo","Rojo","Negro"),
                    labels = c('0','1','2','3','4'))
RutasGoogle$CONG_A2 <- factor(RutasGoogle$CONG_A2,
                              levels = c("SinInf","Verde","Amarillo","Rojo","Negro"),
                              labels = c('0','1','2','3','4'))
RutasGoogle$CONG_A3 <- factor(RutasGoogle$CONG_A3,
                              levels = c("SinInf","Verde","Amarillo","Rojo","Negro"),
                              labels = c('0','1','2','3','4'))



Datos <- RutasGoogle %>%
  select(ViajeId,"DISTAlt1", "TIEMPOAlt1","CONG_A1",
         "DISTAlt2", "TIEMPOAlt2", "CONG_A2", 
         "DISTAlt3", "TIEMPOAlt3","CONG_A3", 
         "DISTEC", "TIEMPOEC",
         "COSTO", "CHOICE") %>%
  # Rutas Google
  inner_join(Rutas200 %>%
               select(ViajeId,Clima,Congestion,Pavimento,Incidente,
                      Meridiano,Horario),
             by = "ViajeId") %>%
  # Base de Datos Completa
  inner_join(DBCompleta %>%
               select(ViajeId,Semaf_A1:MtrP_EC,MODELO,Acc_viajes:Acc_rutas_3,
                      Paneles_viajes:Paneles_rutas_3),
             by = "ViajeId") %>%
  # Modo conducción
  inner_join(DBModoConduccion %>%
               select(ViajeId:DispMob),
             by = "ViajeId") %>%
  # Personalidad conductor
  inner_join(DBPersonalidad %>%
               select(ViajeId:Experiencia),
             by = "ViajeId")

names(Datos)

#Eliminamos 
Datos <- Datos[ ,!colnames(Datos)=="geometry"]
names(Datos)

#Cambiar Nombres de las Variables

Datos <- rename(Datos, replace = c(Clima="CLIMA", 
                               Congestion ="CONGESTION",
                               Pavimento="PAVIMENTO", 
                               Incidente="INCIDENTE", 
                               Meridiano = "MERIDIANO",
                               Horario ="HPICOHVALLE",
                               TiempoProf = "TIEMPO_PROFESION",
                               Edad = "EDAD",
                               NivEdu = "NIVEL_EDUCATIVO",
                               HorTrDia = "HORAS_TRABAJO",
                               Genero = "GENERO",
                               Acc_viajes = "Acc_EC",
                               Paneles_viajes = "Paneles_EC",
                               Semf_EC = "Semaf_EC"))

names(Datos)

# Ordenamos la Base de datos

head(Datos)

#Convertir Factores en Variables Numericas
str(Datos$HPICOHVALLE)

Datos$CLIMA <- factor(Datos$CLIMA,
                      levels = c("Despejado", "Lluvioso"),
                      labels = c('1','2'))

Datos$CONGESTION <- factor(Datos$CONGESTION,
                           levels = c("Nivel A: flujo libre",
                                      "Nivel B: flujo libre con restricciones",
                                      "Nivel C: flujo medio",
                                      "Nivel D: flujo estable",
                                      "Nivel E: flujo bajo",
                                      "Nivel F: flujo congestionado"),
                           labels = c('1','2','3','4','5','6'))

Datos$PAVIMENTO <- factor(Datos$PAVIMENTO,
                          levels = c("Óptimas condiciones",
                                     "Regular estado",
                                     "Mal estado"),
                          labels = c('1','2','3'))

Datos$INCIDENTE <- factor(Datos$INCIDENTE,
                          levels = c("No", "Si"),
                          labels = c('1','2'))

Datos$MERIDIANO <- factor(Datos$MERIDIANO,
                          levels = c("AM","PM"),
                          labels = c('1','2'))

Datos$HPICOHVALLE <- factor(Datos$HPICOHVALLE,
                            levels = c("Valle","Pico"),
                            labels = c('1','2'))


names(Datos)

#ordenamos la base de datos

DBModelo <- Datos %>%
  select("ViajeId", "GENERO", "TIEMPO_PROFESION", "HORAS_TRABAJO","NIVEL_EDUCATIVO",
         "EDAD","DISTAlt1", "TIEMPOAlt1","CONG_A1",                        
         "DISTAlt2","TIEMPOAlt2","CONG_A2" ,                       
         "DISTAlt3", "TIEMPOAlt3", "CONG_A3",
         "DISTEC","TIEMPOEC", "Semaf_A1","Semaf_A2","Semaf_A3","Semaf_EC","CamFD_A1",
         "CamFD_A2","CamFD_A3","CamFD_EC", "ZER_A1","ZER_A2","ZER_A3","ZER_EC","MtrP_A1",
         "MtrP_A2", "MtrP_A3", "MtrP_EC", "Acc_EC", "Acc_rutas_1", "Acc_rutas_2", "Acc_rutas_3",
         "Paneles_EC", "Paneles_rutas_1", "Paneles_rutas_2", "Paneles_rutas_3" , "MODELO",
         "INFOTRAFICO","CLIMA","CONGESTION","PAVIMENTO",
         "INCIDENTE", "MERIDIANO", "HPICOHVALLE","COSTO", "CHOICE",
         "Experiencia", "PasoPeaton", "UsoPito",
         "CinSeg","FRbr", "UsoDirec",
         "EnfCond", "AFrSem", "CulFr",
         "OmLmVel" , "IgPare", "UsoCel",
         "ComVrb", "Ans", "ComAfec", "PrPer", "AmbTr","StrC","ConCl","DispMob")

names(DBModelo)
view(DBModelo)

#Eliminar Variables en el DB

write.table(DBModelo, 
            file="/Users/williz/Desktop/ModelosED/Database/ModeloLogitVL.csv", sep="\t", dec=".")

# EL ANALISIS CONTINUA EN EL SCRIPT AFACTORIALCONJUNTO con ModeloLogitVL.csv


# ORGANIZACION DE VARIABLES DUMMY

MCond <- DBModelo

# EDAD JOVEN - ADULTO - MAYOR_60

MCond$JOVEN30 <- (ifelse((MCond$EDAD == 1), 1,0)) 
MCond$ADULTO40 <- (ifelse((MCond$EDAD == 2),1,0))
MCond$ADULTO60 <- (ifelse((MCond$EDAD == 3),1,0))
MCond$ADULTOMAYOR <- (ifelse((MCond$EDAD == 4),1,0))


# NIVEL EDUCATIVO EDUBASICA - EDUSUP
MCond$EDUBASICA <- (ifelse((MCond$NIVEL_EDUCATIVO == 1), 1,0))
MCond$EDUSUP <- (ifelse((MCond$NIVEL_EDUCATIVO == 2 | MCond$NIVEL_EDUCATIVO == 3),1,0))


# HORA DEL RECORRIDO HPICO - HVALLE
MCond$HPICO <- (ifelse((MCond$HPICOHVALLE == 1), 1,0))
MCond$HVALLE <- (ifelse((MCond$HPICOHVALLE == 2), 1,0))


# CONDICIONES CLIMATICAS CSECO - CLLUVIA
MCond$CSECO <- (ifelse((MCond$CLIMA == 1),1,0))
MCond$CLLUVIA <- (ifelse((MCond$CLIMA == 2),1,0))

# NIVEL DE CONGESTION
MCond$CONG_AB <- (ifelse((MCond$CONGESTION == 1 | MCond$CONGESTION == 2), 1,0))
MCond$CONG_CD <- (ifelse((MCond$CONGESTION == 3 | MCond$CONGESTION == 4), 1,0))
MCond$CONG_EF <- (ifelse((MCond$CONGESTION == 5 | MCond$CONGESTION == 6), 1,0))


# INFORMACION TRAFICO DE LA CIUDAD
MCond$SININFOTRF <- (ifelse((MCond$INFOTRAFICO == 1),1,0))
MCond$CONINFOTRF <- (ifelse((MCond$INFOTRAFICO == 2),1,0))

#Uso del cinturon
tapply(MCond$CinSeg,MCond[,c("JOVEN30","ADULTO40","ADULTO60","ADULTOMAYOR")])

MCond$USOCINTURON <-(ifelse((MCond$CinSeg == 2),1,0))
MCond$NOUSOCINTURON <-(ifelse((MCond$CinSeg == 1),1,0))

#Uso dispositivos
MCond$USODISPMOB <- (ifelse((MCond$DispMob != 1 ),1,0))
MCond$NOUSODISPMOB <- (ifelse((MCond$DispMob ==1),1,0))

# Ver la base de datos
head(MCond)

# LLenar Datos Faltantes
MCond$Semaf_A1 <- as.numeric(MCond$Semaf_A1)
MCond$Semaf_A1[is.na(MCond$Semaf_A1)] <- 999

MCond$Semaf_A2 <- as.numeric(MCond$Semaf_A2)
MCond$Semaf_A2[is.na(MCond$Semaf_A2)] <- 999

MCond$Semaf_A3 <- as.numeric(MCond$Semaf_A3)
MCond$Semaf_A3[is.na(MCond$Semaf_A3)] <- 999

MCond$Semaf_EC <- as.numeric(MCond$Semaf_EC)
MCond$Semaf_EC[is.na(MCond$Semaf_EC)] <- 999

MCond$CamFD_A1 <- as.numeric(MCond$CamFD_A1)
MCond$CamFD_A1[is.na(MCond$CamFD_A1)] <- 999

MCond$CamFD_A2 <- as.numeric(MCond$CamFD_A2)
MCond$CamFD_A2[is.na(MCond$CamFD_A2)] <- 999

MCond$CamFD_A3 <- as.numeric(MCond$CamFD_A3)
MCond$CamFD_A3[is.na(MCond$CamFD_A3)] <- 999

MCond$CamFD_EC <- as.numeric(MCond$CamFD_EC)
MCond$CamFD_EC[is.na(MCond$CamFD_EC)] <- 999

MCond$ZER_A1 <- as.numeric(MCond$ZER_A1)
MCond$ZER_A1[is.na(MCond$ZER_A1)] <- 999

MCond$ZER_A2 <- as.numeric(MCond$ZER_A2)
MCond$ZER_A2[is.na(MCond$ZER_A2)] <- 999

MCond$ZER_A3 <- as.numeric(MCond$ZER_A3)
MCond$ZER_A3[is.na(MCond$ZER_A3)] <- 999

#MCond$ZER_EC <- as.numeric(MCond$ZER_EC)
MCond$ZER_EC[is.na(MCond$ZER_EC)] <- 999

#MCond$MtrP_A1 <- as.numeric(MCond$MtrP_A1)
MCond$MtrP_A1[is.na(MCond$MtrP_A1)] <- 999

MCond$MtrP_A2 <- as.numeric(MCond$MtrP_A2)
MCond$MtrP_A2[is.na(MCond$MtrP_A2)] <- 999

MCond$MtrP_A3 <- as.numeric(MCond$MtrP_A3)
MCond$MtrP_A3[is.na(MCond$MtrP_A3)] <- 999

#MCond$MtrP_EC <- as.numeric(MCond$MtrP_EC)
MCond$MtrP_EC[is.na(MCond$MtrP_EC)] <- 999


MCond$Acc_rutas_2 <- as.numeric(MCond$Acc_rutas_2)
MCond$Acc_rutas_2[is.na(MCond$Acc_rutas_2)] <- 999

MCond$Acc_rutas_3 <- as.numeric(MCond$Acc_rutas_3)
MCond$Acc_rutas_3[is.na(MCond$Acc_rutas_3)] <- 999

MCond$Paneles_rutas_2 <- as.numeric(MCond$Paneles_rutas_2)
MCond$Paneles_rutas_2[is.na(MCond$Paneles_rutas_2)] <- -5

MCond$Paneles_rutas_3 <- as.numeric(MCond$Paneles_rutas_3)
MCond$Paneles_rutas_3[is.na(MCond$Paneles_rutas_3)] <- -5


summary(MCond)

# ?Quiero orden!
tibble::as_tibble(MCond) 

names(MCond) 

# Listado y propiedades de variables

dplyr::glimpse(MCond)   

summary(MCond)

write.table(MCond, 
            file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.csv", sep="\t", dec=".")



