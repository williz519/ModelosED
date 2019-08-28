

library(lavaan)
library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)
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
library(semPlot)

# Cargar Datos desde MAC
DBVL <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.rds")

names(DBVL)

# ORGANIZACION DE VARIABLES DUMMY

# EDAD JOVEN - ADULTO - MAYOR_60

DBVL$JOVEN <- (ifelse((DBVL$EDAD == 1), 1,0)) 
DBVL$ADULTO <- (ifelse((DBVL$EDAD == 2 | DBVL$EDAD == 3),1,0))
DBVL$MAYOR_60 <- (ifelse((DBVL$EDAD == 4),1,0))


# NIVEL EDUCATIVO EDUBASICA - EDUSUP
DBVL$EDUBASICA <- (ifelse((DBVL$NIVELEDUCATIVO == 1 | DBVL$NIVELEDUCATIVO == 4), 1,0))
DBVL$EDUSUP <- (ifelse((DBVL$NIVELEDUCATIVO == 2 | DBVL$NIVELEDUCATIVO == 3),1,0))


# HORA DEL RECORRIDO HPICO - HVALLE
DBVL$HPICO <- (ifelse((DBVL$HPICOHVALLE == 1), 1,0))
DBVL$HVALLE <- (ifelse((DBVL$HPICOHVALLE == 2), 1,0))


# CONDICIONES CLIMATICAS CSECO - CLLUVIA
DBVL$CSECO <- (ifelse((DBVL$CLIMA == 1),1,0))
DBVL$CLLUVIA <- (ifelse((DBVL$CLIMA == 2),1,0))

# NIVEL DE CONGESTION
DBVL$CONG_AB <- (ifelse((DBVL$CONGESTION == 1 | DBVL$CONGESTION == 2), 1,0))
DBVL$CONG_CD <- (ifelse((DBVL$CONGESTION == 3 | DBVL$CONGESTION == 4), 1,0))
DBVL$CONG_EF <- (ifelse((DBVL$CONGESTION == 5 | DBVL$CONGESTION == 6), 1,0))


# INFORMACION TRAFICO DE LA CIUDAD
DBVL$SININFOTRF <- (ifelse((DBVL$INFOTRAFICO == 1),1,0))
DBVL$CONINFOTRF <- (ifelse((DBVL$INFOTRAFICO == 2),1,0))


# Ver la base de datos

head(DBVL)

# ?Quiero orden!
tibble::as_tibble(DBVL) 

names(DBVL) 

# Listado y propiedades de variables

dplyr::glimpse(DBVL)   

summary(DBVL)

#Renombrar las Variables
DBVL <- rename(DBVL, replace =c(ComunicVerbal = "ComVrb",
                                Ansiedad = "Ans",
                                ComunicAfectiva = "ComAfec",
                                PresPersonal = "PrPers",
                                AmbTrabajo = "AmbT",
                                StressAlCond = "StrC",
                                ConsidCliente = "ConsCl",
                                CinturonDeSeguridad = "CinSeg",
                                FrenoRapidoBrusco = "FRbr",
                                UsaDireccionales = "UsDirec",
                                EnfadoConOtroConductor = "EnfCond",
                                AceleraFrenaBruscamenteSemaforo = "AFrSem",
                                CulebreaConFrecuencia = "CulFr",
                                OmiteLimiteVelocidad = "OmLmVel",
                                IgnoraSenhalPare = "IgPare",
                                UsoCelular = "UsCel"))


#CAMBIO DE ESCALA
# Comunicacion Verbal
DBVL$ComVrb[DBVL$ComVrb == 1 | DBVL$ComVrb == 2] <- 1
DBVL$ComVrb[DBVL$ComVrb == 3 | DBVL$ComVrb == 4] <- 2
DBVL$ComVrb[DBVL$ComVrb == 5 | DBVL$ComVrb == 6] <- 3
DBVL$ComVrb[DBVL$ComVrb == 7 | DBVL$ComVrb == 8] <- 4
DBVL$ComVrb[DBVL$ComVrb == 9 | DBVL$ComVrb == 10] <- 5

# Ansiedad
DBVL$Ans[DBVL$Ans == 1 | DBVL$Ans == 2] <- 1
DBVL$Ans[DBVL$Ans == 3 | DBVL$Ans == 4] <- 2
DBVL$Ans[DBVL$Ans == 5 | DBVL$Ans == 6] <- 3
DBVL$Ans[DBVL$Ans == 7 | DBVL$Ans == 8] <- 4
DBVL$Ans[DBVL$Ans == 9 | DBVL$Ans == 10] <- 5

#Comunicacion Afectiva
DBVL$ComAfec[DBVL$ComAfec == 1 | DBVL$ComAfec == 2]<- 1
DBVL$ComAfec[DBVL$ComAfec == 3 | DBVL$ComAfec == 4]<- 2
DBVL$ComAfec[DBVL$ComAfec == 5 | DBVL$ComAfec == 6]<- 3
DBVL$ComAfec[DBVL$ComAfec == 7 | DBVL$ComAfec == 8]<- 4
DBVL$ComAfec[DBVL$ComAfec == 9 | DBVL$ComAfec == 10]<- 5

#Presentacion Personal
DBVL$PrPers[DBVL$PrPers == 1 | DBVL$PrPers == 2] <- 1
DBVL$PrPers[DBVL$PrPers == 3 | DBVL$PrPers == 4] <- 2
DBVL$PrPers[DBVL$PrPers == 5 | DBVL$PrPers == 6] <- 3
DBVL$PrPers[DBVL$PrPers == 7 | DBVL$PrPers == 8] <- 4
DBVL$PrPers[DBVL$PrPers == 9 | DBVL$PrPers == 10] <- 5

# Ambiente de Trabajo
DBVL$AmbT[DBVL$AmbT == 1 | DBVL$AmbT == 2] <- 1
DBVL$AmbT[DBVL$AmbT == 3 | DBVL$AmbT == 4] <- 2
DBVL$AmbT[DBVL$AmbT == 5 | DBVL$AmbT == 6] <- 3
DBVL$AmbT[DBVL$AmbT == 7 | DBVL$AmbT == 8] <- 4
DBVL$AmbT[DBVL$AmbT == 9 | DBVL$AmbT == 10] <- 5

# Stress Al Conducir
DBVL$StrC[DBVL$StrC == 1 | DBVL$StrC == 2] <- 1
DBVL$StrC[DBVL$StrC == 2 | DBVL$StrC == 4] <- 2
DBVL$StrC[DBVL$StrC == 3 | DBVL$StrC == 6] <- 3
DBVL$StrC[DBVL$StrC == 4 | DBVL$StrC == 8] <- 4
DBVL$StrC[DBVL$StrC == 5 | DBVL$StrC == 10] <- 5

#Consideracion con el Cliente
DBVL$ConsCl[DBVL$ConsCl == 1 | DBVL$ConsCl == 2] <- 1
DBVL$ConsCl[DBVL$ConsCl == 3 | DBVL$ConsCl == 4] <- 2
DBVL$ConsCl[DBVL$ConsCl == 5 | DBVL$ConsCl == 6] <- 3
DBVL$ConsCl[DBVL$ConsCl == 7 | DBVL$ConsCl == 8] <- 4
DBVL$ConsCl[DBVL$ConsCl == 9 | DBVL$ConsCl == 10] <- 5




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






#Creacion de la base de datos para el modelo logit multivariado
write.table(DBVL, 
            file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL1.csv", sep="\t", dec=".")

#write.csv2(DBVL, 
#           file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL1.csv")

names(DBVL)