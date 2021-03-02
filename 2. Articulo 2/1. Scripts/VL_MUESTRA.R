
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
library(MVN)
library(mvtnorm)

### Limpiar memoria
rm(list = ls())

## Semilla
set.seed(1234)

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/3. Resultados"
setwd(workingDirectory)

# Cargar Datos desde MAC

database = readRDS("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Bases de datos/DBCOMPLETA.rds")

#DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Bases de datos/DBMuestra_ModeloLogitVL.csv", header = TRUE, sep = "\t")


MCond <- database
names(MCond)

MCond[c("ViajeId")]<-NULL
names(MCond)

# ESPECIFICACION DEL MODELO DE VARIABLES LATENTES.


# Nota: Se restringe todas las covarianzas de las variables latentes en el modelo
# CFA para que sean ortogonales con la opcion orthogonal = TRUE

# Si se desea corregir las variaciones de todas las variables latentes en un modelo
# CFA a la unidad, se utiliza el argumento std.lv = TRUE, pero las cargas factoriales
# del primer indicador de cada variable latente ya no se fijan en 1.

# Indices de ajuste más comunes: CFI (>= .95), TLI (>= .95), RMSEA (<= .05) y SRMR (<= .06).

# Los indices de modificación son valores que nos brindarán una orientación acerca de la 
# re-especificación de la estructura factorial evaluada inicialmente.

############### ##########  ########### ########### ############# ############# ############
############### ##########  ########### ########### ############# ############# ############

#MODELO MODO DE CONDUCCION 3 FACTORES

model_3F_0 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + UsoPito + CulFr 
FA_2 =~ IgPare + OmLmVel  
FA_3 =~ PasoPeaton + UsoDirec 


# Regresiones
FA_1 ~ EDUBASICA + EDUSUP + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_2 ~ EDUBASICA + EDUSUP + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_3 ~ EDUBASICA + EDUSUP + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit_0 <- cfa(model_3F_0, data = MCond, orthogonal = TRUE)
summary(fit_0, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit_0, sort = TRUE, maximum.number = 10) 


#MODELO MODO DE CONDUCCION 3 FACTORES

model_1 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + UsoPito + CulFr 
FA_2 =~ IgPare + OmLmVel  
FA_3 =~ PasoPeaton + UsoDirec 


# Regresiones
FA_1 ~ EDUBASICA + EDUSUP + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_2 ~ EDUBASICA + EDUSUP + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_3 ~ EDUBASICA + EDUSUP + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit_1 <- cfa(model_1, data = MCond, orthogonal = TRUE)
summary(fit_1, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit_1, sort = TRUE, maximum.number = 10) 


model_2 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + UsoPito + CulFr 
FA_2 =~ IgPare + OmLmVel  
FA_3 =~ PasoPeaton + UsoDirec 


# Regresiones
FA_1 ~ EDUBASICA + EDUSUP + EXP_2 + EXP_5 + HTRB_2  + HPICO  + CSECO + 
SININFOTRF + FA_2

FA_2 ~  EXP_3 +  EXP_4 + EXP_5 + HPICO + USODISPMOB 

FA_3 ~ EDUBASICA + EDUSUP + JOVEN30 + ADULTO40  + EXP_2 + HTRB_2 + HTRB_3 + 
SININFOTRF  + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit_2 <- cfa(model_2, data = MCond, orthogonal = TRUE)
summary(fit_2, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit_2, sort = TRUE, maximum.number = 10) 


model_3 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + UsoPito + CulFr 
FA_2 =~ IgPare + OmLmVel  
FA_3 =~ PasoPeaton + UsoDirec 

# Regresiones
FA_1 ~ EDUBASICA + EDUSUP + EXP_5 + HPICO + SININFOTRF + FA_2

FA_2 ~  EXP_4 + HPICO + USODISPMOB 

FA_3 ~ EDUBASICA  + JOVEN30 + ADULTO40  + EXP_2 + HTRB_2 + HTRB_3 + 
SININFOTRF

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit_3 <- cfa(model_3, data = MCond, orthogonal = TRUE)
summary(fit_3, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit_3, sort = TRUE, maximum.number = 10) 




############### ##########  ########### ########### ############# ############# ############
############### ##########  ########### ########### ############# ############# ############

model_4 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + UsoPito + CulFr 
FA_2 =~ IgPare + OmLmVel  
FA_3 =~ PasoPeaton + UsoDirec 


# Regresiones
FA_1 ~ EDUBASICA + EDUSUP +  HPICO  + CSECO + SININFOTRF + FA_2

FA_2 ~ ADULTO40 + EXP_3 +  EXP_4 + EXP_5 + HPICO + USODISPMOB 

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit_4 <- cfa(model_4, data = MCond, orthogonal = TRUE)
summary(fit_4, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit_4, sort = TRUE, maximum.number = 10) 
#sink("Modelo_Muestra_Dos_Factores_alpha_005.txt")




