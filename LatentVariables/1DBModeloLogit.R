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


DB_file <- "/Users/williz/Desktop/rutasviajes/DB_Viajes.xlsx"

DB <- DB_file %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = DB_file)


DBModo <- DB$Viajes %>%
  select(ViajeId) %>%
  # Modo conducción
  inner_join(DB$ModoConduccion %>%
               select(ViajeId:RespetuosaIrrespetuosa,UsoCelular),
             by = "ViajeId")
#View(DBModo)

DBCarac <- DB$Viajes %>%
  select(ViajeId) %>%
  # Modo conducción
  inner_join(DB$CaracterizacionTaxista %>%
               select(ViajeId:InformacionPreviaDelTrafico,Edad),
             by = "ViajeId")

# RutasInf <-readRDS("C:/Users/sin definir/Desktop/Rutas/RutasInf.rds")

# Rutas850 <-readRDS("C:/Users/sin definir/Desktop/Rutas/Rutas850.rds")

# Rutas500 <-readRDS("C:/Users/sin definir/Desktop/Rutas/Rutas500.rds")

# Rutas350 <-readRDS("C:/Users/sin definir/Desktop/Rutas/Rutas350.rds")

# Rutas150 <-readRDS("C:/Users/sin definir/Desktop/Rutas/Rutas150.rds")

#RutasDistancia <- data.frame(RutasInf$ViajeId,RutasInf$Distancia,Rutas850$Distancia, Rutas500$Distancia,
#Rutas350$Distancia, Rutas200$Distancia,Rutas150$Distancia)

#RutasDuracion <- data.frame(RutasInf$ViajeId,RutasInf$Duracion,Rutas850$Duracion, Rutas500$Duracion,
#                            Rutas350$Duracion, Rutas200$Duracion,Rutas150$Duracion)

#RutasPromedio <- data.frame(RutasInf$ViajeId,RutasInf$V_promedio,Rutas850$V_promedio, Rutas500$V_promedio,
#                            Rutas350$V_promedio, Rutas200$V_promedio,Rutas150$V_promedio)

#RutasV_Total <- data.frame(RutasInf$ViajeId,RutasInf$V_total,Rutas850$V_total, Rutas500$V_total,
#                          Rutas350$V_total, Rutas200$V_total,Rutas150$V_total)

#RutasV_Max <- data.frame(RutasInf$ViajeId,RutasInf$V_Max,Rutas850$V_Max, Rutas500$V_Max,
#                         Rutas350$V_Max, Rutas200$V_Max,Rutas150$V_Max)

#RutasComparadas <- data.frame(RutasDistancia,RutasDuracion, RutasPromedio, RutasV_Total, RutasV_Max)

#RutaModeloLogit <-data.frame(Rutas200$ViajeId,Rutas200$Distancia, Rutas200$Duracion, Rutas200$V_promedio, Rutas200$V_total)


#Cargar datos desde DROPBOX
Rutas200 <-readRDS("/Users/williz/Desktop/ModelosED/Database/RutasVelocidades/Rutas200.rds")


Google <-read_xlsx("/Users/williz/Desktop/ModelosED/Database/DBRutasGoogle.xlsx")

RutasGoogle <- Google %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(ViajeId %in% c("{3E49E5E4-D7D2-E811-8FB7-74867AD5B714}",
                          "{FBDD1371-1AC1-E811-914C-74867AD5B714}")))


Datos <- data.frame(DBCarac %>%
  select(ViajeId:Edad) %>%
  inner_join(RutasGoogle %>%
               select(ViajeId,"A1_Distancia Km",
                      "A1_Tiempo Min",A1_Velocidad, "A2_Distancia Km", 
                      "A2_Tiempo Min", A2_Velocidad,"A3_Distancia Km", 
                      "A3_Tiempo Min",A3_Velocidad,AT_Distancia, AT_Tiempo, 
                      AT_VelMed, AT_VelTotal,Costo, Choice),
             by = "ViajeId"))
  

#Datos1<- data.frame(Datos %>%
  # Filtrar los puntos a excluir
  #filter(Choice!=0))

Datos1 <- data.frame(Datos)

View(Datos1)

DBModLog <- data.frame(Rutas200 %>%
  select(ViajeId,Cod_Viaje, Clima,Congestion,Pavimento,Incidente,
         Meridiano,Horario) %>%
  inner_join(Datos1 %>%
               select(ViajeId:Choice),
             by ="ViajeId"))


#Cambiar Nombres de las Variables

DBModLog <- rename(DBModLog, replace = c(Cod_Viaje="CODVIAJE",Clima="CLIMA", 
                                         Congestion ="CONGESTION",
                                         Pavimento="PAVIMENTO", 
                                         Incidente="INCIDENTE", 
                                         Meridiano = "MERIDIANO",
                                         Horario ="HPICOHVALLE", 
                                         Genero = "GENERO",
                                         TiempoProfesion = "TIEMPO_PROFESION", 
                                         HorasAlDia="HORASTRABAJO",
                                         NivelEducativo = "NIVELEDUCATIVO", 
                                         InformacionPreviaDelTrafico="INFOTRAFICO",
                                         Edad="EDAD",
                                         A1_Distancia.Km="DISTAlt1", 
                                         A1_Tiempo.Min="TIEMPOAlt1",
                                         A1_Velocidad="VELAlt1", 
                                         A2_Distancia.Km="DISTAlt2", 
                                         A2_Tiempo.Min="TIEMPOAlt2", 
                                         A2_Velocidad="VELAlt2", 
                                         A3_Distancia.Km="DISTAlt3", 
                                         A3_Tiempo.Min="TIEMPOAlt3",
                                         A3_Velocidad="VELAlt3", 
                                         AT_Distancia="DISTEC", 
                                         AT_Tiempo="TIEMPOEC", 
                                         AT_VelMed="VELPROMEC", 
                                         AT_VelTotal="VELTOTALEC", 
                                         Costo="COSTO", 
                                         Choice="CHOICE"))
#View(DBModLog)

str(DBModLog)

#Convertir Factores en Variables Numericas

levels(DBModLog$CLIMA) <- c('1','2')

#1: Nivel A, 2: Nivel B, 3: Nivel C, 4:Nivel D, 5:Nivel E, 6: Nivel F

levels(DBModLog$CONGESTION) <- c('1','2','3','4','5','6') 

# #1:Optimas Condiciones, 2: 

levels(DBModLog$PAVIMENTO) <- c('1','2','3')  

#1:NO, 2:Si
levels(DBModLog$INCIDENTE) <- c('1','2')

#1: AM, 2: PM
levels(DBModLog$MERIDIANO) <- c('1','2')

#1:Valle, 2:Pico
levels(DBModLog$HPICOHVALLE) <- c('1','2')


#Cambio de los valores faltantes por -1

DBModLog$TIEMPO_PROFESION[is.na(DBModLog$TIEMPO_PROFESION)] <- -1

DBModLog$HORASTRABAJO[is.na(DBModLog$HORASTRABAJO)]<- -1

View(DBModLog)


#Eliminar Variables en el DB

DBModLog <- DBModLog[ ,!colnames(DBModLog)=="geometry"]

#DBModLog <- DBModLog[ ,!colnames(DBModLog)=="ViajeId"]

str(DBModLog)
View(DBModLog)


saveRDS(DBModLog, file = "/Users/williz/Desktop/ModelosED/Database/DBModLog1.rds")


#Para guardar la DB para el modelo MLN

#write.csv2(DBModeloLogit, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/Analisis en R/DBModeloLogit.csv",
#           sheetName = "Rutas", col.names=TRUE, row.names=TRUE, append=FALSE)

#write.table(DBModLog,file="/Users/williz/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/Analisis en R/Modelo Logit DB/DBModLog.csv", sep="\t", dec=".")


#write.xlsx(RutasComparadas, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/Analisis en R/RutasComparadas.xlsx",
#           sheetName = "Rutas", col.names=TRUE, row.names=TRUE, append=FALSE)
