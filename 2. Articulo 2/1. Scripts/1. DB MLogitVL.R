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

DBCompleta <- read_xlsx("/Users/williz/Desktop/ModelosED/Bases de Datos Ordenada/DBViajesCompleta_VF.xlsx")

 
RutasGoogle <- Google %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(ViajeId %in% c("{3E49E5E4-D7D2-E811-8FB7-74867AD5B714}",
                          "{FBDD1371-1AC1-E811-914C-74867AD5B714}",
                          "{CD4C129D-8BC6-E811-914C-74867AD5B714}",
                          "{E2CCCECA-74C3-E811-914C-74867AD5B714}",
                          "{0B431AE3-7DBE-E811-914C-74867AD5B714}")))

Rutas200 <-readRDS("/Users/williz/Desktop/ModelosED/Database/RutasVelocidades/Rutas200.rds")

#Base de datos creada en DBModoConduccion.r Escala 1-3
#DBModoConduccion <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModoConduccion.rds")

#Base de datos creada en DBModoConduccion.r Escala 1-5
DBModoConduccion <- readRDS("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBModoCondCE.rds")


#Base de datos creada en DBPersonalidad.r
DBPersonalidad <- readRDS("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBPersonalidad.rds")


names(Rutas200)
names(RutasGoogle)
names(DBModoConduccion)
names(DBPersonalidad)
names(DBCompleta)


# Arreglos Rutas Google
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

# Arreglos DBCompleta

DBCompleta$Semaf_A1 <- as.numeric(DBCompleta$Semaf_A1)
DBCompleta$Semaf_A1[is.na(DBCompleta$Semaf_A1)]<- 999

DBCompleta$Semaf_A2 <- as.numeric(DBCompleta$Semaf_A2)
DBCompleta$Semaf_A2[is.na(DBCompleta$Semaf_A2)]<- 999

DBCompleta$Semaf_A3 <- as.numeric(DBCompleta$Semaf_A3)
DBCompleta$Semaf_A3[is.na(DBCompleta$Semaf_A3)]<- 999

DBCompleta$Semf_EC <- as.numeric(DBCompleta$Semf_EC)
DBCompleta$Semf_EC[is.na(DBCompleta$Semf_EC)]<- 999

DBCompleta$MtrP_A1 <- as.numeric(DBCompleta$MtrP_A1)
DBCompleta$MtrP_A1[is.na(DBCompleta$MtrP_A1)]<- 999

DBCompleta$MtrP_A2 <- as.numeric(DBCompleta$MtrP_A2)
DBCompleta$MtrP_A2[is.na(DBCompleta$MtrP_A2)]<- 999

DBCompleta$MtrP_A3 <- as.numeric(DBCompleta$MtrP_A3)
DBCompleta$MtrP_A3[is.na(DBCompleta$MtrP_A3)]<- 999

DBCompleta$MtrP_EC <- as.numeric(DBCompleta$MtrP_EC)
DBCompleta$MtrP_EC[is.na(DBCompleta$MtrP_EC)]<- 999

DBCompleta$ZER_A1 <- as.numeric(DBCompleta$ZER_A1)
DBCompleta$ZER_A1[is.na(DBCompleta$ZER_A1)]<- 999

DBCompleta$ZER_A2 <- as.numeric(DBCompleta$ZER_A2)
DBCompleta$ZER_A2[is.na(DBCompleta$ZER_A2)]<- 999

DBCompleta$ZER_A3 <- as.numeric(DBCompleta$ZER_A3)
DBCompleta$ZER_A3[is.na(DBCompleta$ZER_A3)]<- 999

DBCompleta$ZER_EC <- as.numeric(DBCompleta$ZER_EC)
DBCompleta$ZER_EC[is.na(DBCompleta$ZER_EC)]<- 999

DBCompleta$Acc_A1 <- as.numeric(DBCompleta$Acc_A1)
DBCompleta$Acc_A1[is.na(DBCompleta$Acc_A1)]<- 999

DBCompleta$Acc_A2 <- as.numeric(DBCompleta$Acc_A2)
DBCompleta$Acc_A2[is.na(DBCompleta$Acc_A2)]<- 999

DBCompleta$Acc_A3 <- as.numeric(DBCompleta$Acc_A3)
DBCompleta$Acc_A3[is.na(DBCompleta$Acc_A3)]<- 999

DBCompleta$Acc_EC <- as.numeric(DBCompleta$Acc_EC)
DBCompleta$Acc_EC[is.na(DBCompleta$Acc_EC)]<- 999

DBCompleta$CamFD_A1 <- as.numeric(DBCompleta$CamFD_A1)
DBCompleta$CamFD_A1[is.na(DBCompleta$CamFD_A1)]<- 999

DBCompleta$CamFD_A2 <- as.numeric(DBCompleta$CamFD_A2)
DBCompleta$CamFD_A2[is.na(DBCompleta$CamFD_A2)]<- 999

DBCompleta$CamFD_A3 <- as.numeric(DBCompleta$CamFD_A3)
DBCompleta$CamFD_A3[is.na(DBCompleta$CamFD_A3)]<- 999

DBCompleta$CamFD_EC <- as.numeric(DBCompleta$CamFD_EC)
DBCompleta$CamFD_EC[is.na(DBCompleta$CamFD_EC)]<- 999

DBCompleta$Paneles_A1 <- as.numeric(DBCompleta$Paneles_A1)
DBCompleta$Paneles_A1[is.na(DBCompleta$Paneles_A1)]<- 999

DBCompleta$Paneles_A2 <- as.numeric(DBCompleta$Paneles_A2)
DBCompleta$Paneles_A2[is.na(DBCompleta$Paneles_A2)]<- 999

DBCompleta$Paneles_A3 <- as.numeric(DBCompleta$Paneles_A3)
DBCompleta$Paneles_A3[is.na(DBCompleta$Paneles_A3)]<- 999

DBCompleta$Paneles_EC <- as.numeric(DBCompleta$Paneles_EC)
DBCompleta$Paneles_EC[is.na(DBCompleta$Paneles_EC)]<- 999

summary(DBCompleta)



# Datos Agrupados

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
               select(ViajeId:Paneles_A3),
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
  select("ViajeId","MUESTRA","VALIDAR", "GENERO", "TIEMPO_PROFESION", "HORAS_TRABAJO","NIVEL_EDUCATIVO",
         "EDAD","DISTAlt1", "TIEMPOAlt1","CONG_A1",                        
         "DISTAlt2","TIEMPOAlt2","CONG_A2" ,                       
         "DISTAlt3", "TIEMPOAlt3", "CONG_A3",
         "DISTEC","TIEMPOEC", "Semaf_A1","Semaf_A2","Semaf_A3","Semaf_EC","CamFD_A1",
         "CamFD_A2","CamFD_A3","CamFD_EC", "ZER_A1","ZER_A2","ZER_A3","ZER_EC","MtrP_A1",
         "MtrP_A2", "MtrP_A3", "MtrP_EC", "Acc_EC", "Acc_A1", "Acc_A2", "Acc_A3",
         "Paneles_EC", "Paneles_A1", "Paneles_A2", "Paneles_A3",
         "INFOTRAFICO","CLIMA","CONGESTION","PAVIMENTO",
         "INCIDENTE", "MERIDIANO", "HPICOHVALLE","COSTO", "CHOICE",
         "Experiencia", "PasoPeaton", "UsoPito",
         "CinSeg","FRbr", "UsoDirec",
         "EnfCond", "AFrSem", "CulFr",
         "OmLmVel" , "IgPare", "UsoCel",
         "ComVrb", "Ans", "ComAfec", "PrPer", "AmbTr","StrC","ConCl","DispMob")

names(DBModelo)
#view(DBModelo)

#Eliminar Variables en el DB

write.table(DBModelo, 
            file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/ModeloLogitVL.csv", sep="\t", dec=".")

# EL ANALISIS CONTINUA EN EL SCRIPT AFACTORIALCONJUNTO con ModeloLogitVL.csv

DBModelo = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/ModeloLogitVL.csv", sep="\t", dec=".")


# ORGANIZACION DE VARIABLES DUMMY

MCond <- DBModelo
dplyr::glimpse(MCond)  


# EDAD JOVEN - ADULTO - MAYOR_60

MCond$JOVEN30 <- (ifelse((MCond$EDAD == 1), 1,0)) 
MCond$ADULTO40 <- (ifelse((MCond$EDAD == 2),1,0))
MCond$ADULTO60 <- (ifelse((MCond$EDAD == 3),1,0))
MCond$ADULTOMAYOR <- (ifelse((MCond$EDAD == 4),1,0))


# NIVEL EDUCATIVO EDUBASICA - EDUSUP
MCond$EDUBASICA <- (ifelse((MCond$NIVEL_EDUCATIVO == 0 | MCond$NIVEL_EDUCATIVO == 1), 1,0))
MCond$EDUSUP <- (ifelse((MCond$NIVEL_EDUCATIVO == 2 | MCond$NIVEL_EDUCATIVO == 3),1,0))
MCond$EDUNR <- (ifelse((MCond$NIVEL_EDUCATIVO == 4), 1,0))


# HORA DEL RECORRIDO HPICO - HVALLE
MCond$HPICO <- (ifelse((MCond$HPICOHVALLE == 1), 1,0))
MCond$HVALLE <- (ifelse((MCond$HPICOHVALLE == 2), 1,0))


# CONDICIONES CLIMATICAS CSECO - CLLUVIA
MCond$CSECO <- (ifelse((MCond$CLIMA == 1),1,0))
MCond$CLLUVIA <- (ifelse((MCond$CLIMA == 2),1,0))

# NIVEL DE CONGESTION
MCond$CONG_A1 <- as.numeric(MCond$CONG_A1)
MCond$CONG_AB_A1 <- (ifelse((MCond$CONG_A1 == 1), 1,0))
MCond$CONG_CD_A1 <- (ifelse((MCond$CONG_A1 == 2 | MCond$CONG_A1 == 0), 1,0))
MCond$CONG_EF_A1 <- (ifelse((MCond$CONG_A1 >= 3), 1,0))

MCond$CONG_A2 <- as.numeric(MCond$CONG_A2)
MCond$CONG_AB_A2 <- (ifelse((MCond$CONG_A2 == 1), 1,0))
MCond$CONG_CD_A2 <- (ifelse((MCond$CONG_A2 == 2 | MCond$CONG_A2 == 0), 1,0))
MCond$CONG_EF_A2 <- (ifelse((MCond$CONG_A2 >= 3), 1,0))

MCond$CONG_A3 <- as.numeric(MCond$CONG_A3)
MCond$CONG_AB_A3 <- (ifelse((MCond$CONG_A3 == 1), 1,0))
MCond$CONG_CD_A3 <- (ifelse((MCond$CONG_A3 == 2 | MCond$CONG_A3 == 0), 1,0))
MCond$CONG_EF_A3 <- (ifelse((MCond$CONG_A3 >= 3), 1,0))

MCond$CONG_AB_EC <- (ifelse((MCond$CONGESTION == 1 | MCond$CONGESTION == 2), 1,0))
MCond$CONG_CD_EC <- (ifelse((MCond$CONGESTION == 3 | MCond$CONGESTION == 4), 1,0))
MCond$CONG_EF_EC <- (ifelse((MCond$CONGESTION == 5 | MCond$CONGESTION == 6), 1,0))


# INFORMACION TRAFICO DE LA CIUDAD
MCond$SININFOTRF <- (ifelse((MCond$INFOTRAFICO == 1),1,0))
MCond$CONINFOTRF <- (ifelse((MCond$INFOTRAFICO == 2),1,0))

# Uso de cinturon de seguridad
MCond$USOCINTURON <-(ifelse((MCond$CinSeg == 2),1,0))
MCond$NOUSOCINTURON <-(ifelse((MCond$CinSeg == 1),1,0))

#Uso dispositivos
MCond$USODISPMOB <- (ifelse((MCond$DispMob != 1 ),1,0))
MCond$NOUSODISPMOB <- (ifelse((MCond$DispMob ==1),1,0))

MCond$NO_MTRP_A1 <- (ifelse((MCond$MtrP_A1 == 0),1,0))
MCond$NO_MTRP_A2 <- (ifelse((MCond$MtrP_A2 == 0),1,0))
MCond$NO_MTRP_A3 <- (ifelse((MCond$MtrP_A3 == 0),1,0))
MCond$NO_MTRP_EC <- (ifelse((MCond$MtrP_EC == 0),1,0))

MCond$SI_MTRP_A1 <- (ifelse((MCond$MtrP_A1 >= 1 ),1,0))
MCond$SI_MTRP_A2 <- (ifelse((MCond$MtrP_A2 >= 1 ),1,0))
MCond$SI_MTRP_A3 <- (ifelse((MCond$MtrP_A3 >= 1 ),1,0))
MCond$SI_MTRP_EC <- (ifelse((MCond$MtrP_EC >= 1 ),1,0))


MCond$NO_ZER_A1 <- (ifelse((MCond$ZER_A1 == 0),1,0))
MCond$NO_ZER_A2 <- (ifelse((MCond$ZER_A2 == 0),1,0))
MCond$NO_ZER_A3 <- (ifelse((MCond$ZER_A3 == 0),1,0))
MCond$NO_ZER_EC <- (ifelse((MCond$ZER_EC == 0),1,0))

MCond$SI_ZER_A1 <- (ifelse((MCond$ZER_A1 >= 1),1,0))
MCond$SI_ZER_A2 <- (ifelse((MCond$ZER_A2 >= 1 ),1,0))
MCond$SI_ZER_A3 <- (ifelse((MCond$ZER_A3 >= 1 ),1,0))
MCond$SI_ZER_EC <- (ifelse((MCond$ZER_EC >= 1 ),1,0))

#Horas de trabajo al día
MCond$HTRB_1 <- (ifelse((MCond$HORAS_TRABAJO <= 6),1,0))
MCond$HTRB_2 <- (ifelse((MCond$HORAS_TRABAJO > 6 & MCond$HORAS_TRABAJO <= 10),1,0))
MCond$HTRB_3 <- (ifelse((MCond$HORAS_TRABAJO > 10),1,0))


MCond$SEMF_A1_1 <- (ifelse((MCond$Semaf_A1 <= 7),1,0))
MCond$SEMF_A1_2 <- (ifelse((MCond$Semaf_A1 >= 8 & MCond$Semaf_A1 <= 15),1,0))
MCond$SEMF_A1_3 <- (ifelse((MCond$Semaf_A1 > 16),1,0))

MCond$SEMF_A2_1 <- (ifelse((MCond$Semaf_A2 <= 7),1,0))
MCond$SEMF_A2_2 <- (ifelse((MCond$Semaf_A2 >= 8 & MCond$Semaf_A2 <= 15),1,0))
MCond$SEMF_A2_3 <- (ifelse((MCond$Semaf_A2 > 16),1,0))


MCond$SEMF_A3_1 <- (ifelse((MCond$Semaf_A3 <= 7),1,0))
MCond$SEMF_A3_2 <- (ifelse((MCond$Semaf_A3 >= 8 & MCond$Semaf_A3 <= 15),1,0))
MCond$SEMF_A3_3 <- (ifelse((MCond$Semaf_A3 > 16),1,0))

MCond$SEMF_EC_1 <- (ifelse((MCond$Semaf_EC <= 7),1,0))
MCond$SEMF_EC_2 <- (ifelse((MCond$Semaf_EC >= 8 & MCond$Semaf_EC <= 15),1,0))
MCond$SEMF_EC_3 <- (ifelse((MCond$Semaf_EC > 16),1,0))


MCond$NO_CAMFD_A1 <- (ifelse((MCond$CamFD_A1 == 0),1,0))
MCond$SI_CAMFD_A1 <- (ifelse((MCond$CamFD_A1 >= 1),1,0))

MCond$NO_CAMFD_A2 <- (ifelse((MCond$CamFD_A2 == 0),1,0))
MCond$SI_CAMFD_A2 <- (ifelse((MCond$CamFD_A2 >= 1),1,0))

MCond$NO_CAMFD_A3 <- (ifelse((MCond$CamFD_A3 == 0),1,0))
MCond$SI_CAMFD_A3 <- (ifelse((MCond$CamFD_A3 >= 1),1,0))

MCond$NO_CAMFD_EC <- (ifelse((MCond$CamFD_EC == 0),1,0))
MCond$SI_CAMFD_EC <- (ifelse((MCond$CamFD_EC >= 1),1,0))


MCond$ACC_A1_0 <- (ifelse((MCond$Acc_A1 == 0),1,0))
MCond$ACC_A1_1 <- (ifelse((MCond$Acc_A1 == 1),1,0))
MCond$ACC_A1_2 <- (ifelse((MCond$Acc_A1 >= 2),1,0))

MCond$ACC_A2_0 <- (ifelse((MCond$Acc_A2 == 0),1,0))
MCond$ACC_A2_1 <- (ifelse((MCond$Acc_A2 == 1),1,0))
MCond$ACC_A2_2 <- (ifelse((MCond$Acc_A2 >= 2),1,0))

MCond$ACC_A3_0 <- (ifelse((MCond$Acc_A3 == 0),1,0))
MCond$ACC_A3_1 <- (ifelse((MCond$Acc_A3 == 1),1,0))
MCond$ACC_A3_2 <- (ifelse((MCond$Acc_A3 >= 2),1,0))

MCond$ACC_EC_0 <- (ifelse((MCond$Acc_EC == 0),1,0))
MCond$ACC_EC_1 <- (ifelse((MCond$Acc_EC == 1),1,0))
MCond$ACC_EC_2 <- (ifelse((MCond$Acc_EC >= 2),1,0))

#MCond$Paneles_A1[MCond$Paneles_A1 == 999]<-0
MCond$NO_PANEL_A1 <- (ifelse((MCond$Paneles_A1 == 0 ),1,0))
MCond$SI_PANEL_A1 <- (ifelse((MCond$Paneles_A1 >= 1),1,0))

MCond$Paneles_A2[MCond$Paneles_A2 == 999]<-1
MCond$NO_PANEL_A2 <- (ifelse((MCond$Paneles_A2 == 0 ),1,0))
MCond$SI_PANEL_A2 <- (ifelse((MCond$Paneles_A2 >= 1),1,0))

MCond$Paneles_A3[MCond$Paneles_A3 == 999]<-1
MCond$NO_PANEL_A3 <- (ifelse((MCond$Paneles_A3 == 0 ),1,0))
MCond$SI_PANEL_A3 <- (ifelse((MCond$Paneles_A3 >= 1),1,0))


MCond$NO_PANEL_EC <- (ifelse((MCond$Paneles_EC == 0 ),1,0))
MCond$SI_PANEL_EC <- (ifelse((MCond$Paneles_EC >= 1),1,0))


MCond$EXP_1 <- ifelse((MCond$Experiencia == 1),1,0)
MCond$EXP_2 <- ifelse((MCond$Experiencia == 2),1,0)
MCond$EXP_3 <- ifelse((MCond$Experiencia == 3),1,0)
MCond$EXP_4 <- ifelse((MCond$Experiencia == 4 ),1,0)
MCond$EXP_5 <- ifelse((MCond$Experiencia == 5 ),1,0)



# ?Quiero orden!
tibble::as_tibble(MCond) 

names(MCond) 

# Listado y propiedades de variables

dplyr::glimpse(MCond)   

summary(MCond)
names(MCond)

#MCond[c("TIEMPO_PROFESION", "HORAS_TRABAJO","NIVEL_EDUCATIVO","EDAD","CLIMA", "MERIDIANO",
#        "HPICOHVALLE","CONGESTION","INCIDENTE")] <- NULL

# ?Quiero orden!
tibble::as_tibble(MCond) 

names(MCond) 

# Listado y propiedades de variables

dplyr::glimpse(MCond)   

summary(MCond)

# DB Completa
write.table(MCond, 
            file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBCompleta_ModeloLogitVL.csv", sep="\t", dec=".")


#######################################################################################################

