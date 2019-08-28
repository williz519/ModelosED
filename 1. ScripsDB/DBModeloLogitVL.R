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

DB_file <- "/Users/williz/Desktop/ModelosED/Database/DB_Viajes.xlsx"

DB <- DB_file %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = DB_file)


Rutas200 <-readRDS("/Users/williz/Desktop/ModelosED/Database/RutasVelocidades/Rutas200.rds")

Google <-read_xlsx("/Users/williz/Desktop/ModelosED/Database/DBRutasGoogle.xlsx")

RutasGoogle <- Google %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(ViajeId %in% c("{3E49E5E4-D7D2-E811-8FB7-74867AD5B714}",
                          "{FBDD1371-1AC1-E811-914C-74867AD5B714}")))
names(Rutas200)
names(RutasGoogle)
names(DB$ModoConduccion)
names(DB$CaracterizacionTaxista)
names(DB$PersonalidadConductor)

Datos <- DB$Viajes %>%
  select(ViajeId) %>%
  # Rutas 200
  inner_join(Rutas200 %>%
               select(ViajeId, Clima,Congestion,Pavimento,Incidente,
                             Meridiano,Horario),
             by ="ViajeId") %>%
  # Rutas Google
  inner_join(RutasGoogle %>%
               select(ViajeId,"A1_Distancia Km",
                      "A1_Tiempo Min",A1_Velocidad, "A2_Distancia Km", 
                      "A2_Tiempo Min", A2_Velocidad,"A3_Distancia Km", 
                      "A3_Tiempo Min",A3_Velocidad,AT_Distancia, AT_Tiempo, 
                      AT_VelMed, AT_VelTotal,Costo, Choice),
             by = "ViajeId") %>%
  # Modo conducción
  inner_join(DB$ModoConduccion %>%
               select(ViajeId:RespetuosaIrrespetuosa,UsoCelular),
             by = "ViajeId") %>%
  # Caracterización taxista
  inner_join(DB$CaracterizacionTaxista %>%
               select(ViajeId:InformacionPreviaDelTrafico,Edad),
             by = "ViajeId") %>%
  # Personalidad conductor
  inner_join(DB$PersonalidadConductor %>%
               select(ViajeId:AmbienteOrdenadoDesordenado),
             by = "ViajeId")

names(Datos)

#Eliminamos 
Datos <- Datos[ ,!colnames(Datos)=="geometry"]

#Cambiar Nombres de las Variables

Datos1 <- rename(Datos, replace = c(Clima="CLIMA", 
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
                               "A1_Distancia Km" ="DISTAlt1", 
                               "A1_Tiempo Min"="TIEMPOAlt1",
                               A1_Velocidad="VELAlt1", 
                               "A2_Distancia Km" ="DISTAlt2", 
                               "A2_Tiempo Min" ="TIEMPOAlt2", 
                               A2_Velocidad="VELAlt2", 
                               "A3_Distancia Km"="DISTAlt3", 
                               "A3_Tiempo Min" ="TIEMPOAlt3",
                               A3_Velocidad="VELAlt3", 
                               AT_Distancia="DISTEC", 
                               AT_Tiempo="TIEMPOEC", 
                               AT_VelMed="VELPROMEC", 
                               AT_VelTotal="VELTOTALEC", 
                               Costo="COSTO", 
                               Choice="CHOICE",
                               Accidente = "DispMob", 
                               Atraco = "SatDispMob",
                               SerioConservador = "ComVrb",
                               RelajadoTenso = "Ans",
                               AmableAntipatico = "ComAfec",
                               OrdenadoDesordenado = "PrPer",
                               AmbienteOrdenadoDesordenado = "AmbTr",
                               TranquilaTensionada = "StrC",
                               RespetuosaIrrespetuosa = "ConCl",
                               FrenoRapidoBrusco = "FRbr",
                               EnfadoConOtroConductor = "EnfCond",
                               AceleraFrenaBruscamenteSemaforo = "AFrSem",
                               CulebreaConFrecuencia = "CulFr",
                               OmiteLimiteVelocidad = "OmLmVel", 
                               IgnoraSenhalPare = "IgPare",
                               CinturonDeSeguridad = "CinSeg",
                               UsaDireccionales = "UsoDirec",
                               UsoCelular = "UsoCel"))

names(Datos1)

# Ordenamos la Base de datos

X <- Datos1 %>%
  select("ViajeId", "GENERO", "TIEMPO_PROFESION", "HORASTRABAJO","NIVELEDUCATIVO", "INFOTRAFICO", 
         "EDAD","CLIMA","CONGESTION","PAVIMENTO","INCIDENTE", "MERIDIANO", "HPICOHVALLE",
         "DISTAlt1", "TIEMPOAlt1","VELAlt1",                        
         "DISTAlt2","TIEMPOAlt2","VELAlt2" ,                       
         "DISTAlt3", "TIEMPOAlt3", "VELAlt3",
         "DISTEC","TIEMPOEC", "VELPROMEC", "VELTOTALEC", "COSTO", "CHOICE",
         "DispMob", "SatDispMob", "PasoPeatones", "UsaPito",
         "CinSeg","FRbr", "UsoDirec",
         "EnfCond", "AFrSem", "CulFr",
         "OmLmVel" , "IgPare", "StrC","ConCl", "UsoCel",
         "ComVrb", "Ans", "ComAfec", "PrPer", "AmbTr")



head(X)

names(X)

View(X)

str(X)

# Cinturon de Seguridad
# Datos$CinturonDeSeguridad[X$CinturonDeSeguridad == 2] <-3
X$UsoCel[X$UsoCel == 4] <-1
X$UsoCel[X$UsoCel == 0] <-1

# Freno Rapido y Brusco
X$FRbr[X$FRbr ==4 & X$AFrSem == 2] <-2
X$FRbr[X$FRbr == 4 & X$OmLmVel == 2] <- 2
X$FRbr[X$FRbr == 4 & X$OmLmVel == 3] <- 3
X$FRbr[X$FRbr ==4 & X$AFrSem == 4] <-1
X$FRbr[X$FRbr == 4 & X$AFrSem ==1 & X$CulFr == 2] <- 2
X$FRbr[X$FRbr == 4 & X$AFrSem ==1 & X$CulFr == 1] <- 1


# Acelera o frena bruscamente a la salida (llegada) de un semaforo
X$AFrSem[X$AFrSem == 4 & X$FRbr == 1] <- 1
X$AFrSem[X$AFrSem == 4 & X$FRbr == 2] <- 2

# Omite limite de Velocidad
X$OmLmVel[X$OmLmVel == 4 & X$CulFr == 2] <- 2
X$OmLmVel[X$OmLmVel == 4 & X$AFrSem == 2] <- 3
X$OmLmVel[X$OmLmVel == 4 & X$AFrSem == 1]<- 2

#Ignora señal de pare
X$IgPare[X$IgPare == 4 & X$AFrSem == 1] <- 1
X$IgPare[X$IgPare == 4 & X$AFrSem == 2] <- 2
X$IgPare[X$IgPare == 4 & X$AFrSem == 3] <- 3

#Usa las direccionales
X$UsoDirec[X$UsoDirec == 4 & X$CulFr == 2] <- 2

# Se enfada con otro conductor
X$EnfCond[X$EnfCond == 4 & (X$UsaPito == 1 | X$AFrSem == 1)] <- 1
X$EnfCond[X$EnfCond == 4 & (X$UsaPito == 2 | X$AFrSem == 2)] <- 2
X$EnfCond[X$EnfCond == 4 & (X$UsaPito == 3 | X$AFrSem == 3)] <- 3

X$UsoDirec[X$UsoDirec == 4] <- 1
X$CulFr[X$CulFr == 4] <- 3


#Reemplazar Valores Faltantes

X$ComVrb[is.na(X$ComVrb)] <- 5
X$Ans[is.na(X$Ans)] <- 5
X$ComAfec[is.na(X$ComAfec)] <- 5
X$PrPer[is.na(X$PrPer)] <- 5
X$AmbTr[is.na(X$AmbTr)] <- 5
X$StrC[is.na(X$StrC)] <- 5
X$ConCl[is.na(X$ConCl)] <- 5
X$HORASTRABAJO[is.na(X$HORASTRABAJO)] <- 8
X$TIEMPO_PROFESION[is.na(X$TIEMPO_PROFESION)] <- 0.5
X$EDAD[X$EDAD == 0 ] <- 2
X$TIEMPO_PROFESION[X$TIEMPO_PROFESION == 0] <- 0.5
X$HORASTRABAJO[X$HORASTRABAJO == 0] <- 1

X$NIVELEDUCATIVO[X$NIVELEDUCATIVO == 0 & X$TIEMPO_PROFESION < 15] <- 2
X$NIVELEDUCATIVO[X$NIVELEDUCATIVO == 0 & X$TIEMPO_PROFESION > 15] <- 1
X$NIVELEDUCATIVO[X$NIVELEDUCATIVO == 4 & X$TIEMPO_PROFESION < 15] <- 2
X$NIVELEDUCATIVO[X$NIVELEDUCATIVO == 4 & X$TIEMPO_PROFESION > 15] <- 1

X$NIVELEDUCATIVO[X$NIVELEDUCATIVO == 0 | X$NIVELEDUCATIVO == 4] <- 2




str(X)
summary(X, na.rm = TRUE)


#Convertir Factores en Variables Numericas

levels(X$CLIMA) <- c('1','2')

#1: Nivel A, 2: Nivel B, 3: Nivel C, 4:Nivel D, 5:Nivel E, 6: Nivel F

levels(X$CONGESTION) <- c('1','2','3','4','5','6') 

# #1:Optimas Condiciones, 2: 

levels(X$PAVIMENTO) <- c('1','2','3')  

#1:NO, 2:Si
levels(X$INCIDENTE) <- c('1','2')

#1: AM, 2: PM
levels(X$MERIDIANO) <- c('1','2')

#1:Valle, 2:Pico
levels(X$HPICOHVALLE) <- c('1','2')


#Eliminar Variables en el DB

X <- X[ ,!colnames(X)=="ViajeId"]

View(X)

#Creacion de la base de datos para el modelo logit multivariado
write.table(X, 
            file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL2.csv", sep="\t", dec=".")

names(X)



# Cambio de escala
X1 <- select(X, -("FRbr":"AmbTr"))

names(X1)

CEscalaPerso <-read_xlsx("/Users/williz/Desktop/ModelosED/Database/CambioEscalaVL.xlsx", 
                         sheet = "Personalidad")

CEscalaCond <-read_xlsx("/Users/williz/Desktop/ModelosED/Database/CambioEscalaVL.xlsx", 
                         sheet = "ModoCond")

CEscala <- cbind(X1,CEscalaPerso,CEscalaCond)

write.table(CEscala, 
            file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVLCE.csv", sep="\t", dec=".")

