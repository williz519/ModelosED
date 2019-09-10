
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

DBVL <- read.csv("/Users/williz/Desktop/ModelosED/Database/DBVL.csv", sep="\t", dec=".")

# ESPECIFICACION DEL MODELO DE VARIABLES LATENTES.
# Nota: Se restringe todas las covarianzasde las variables latentes en el modelo
# CFA para que sean ortogonales con la opcion orthogonal = TRUE

# Si se desea corregir las variaciones de todas las variables latentes en un modelo
# CFA a la unidad, se utiliza el argumento std.lv = TRUE, pero las cargas factoriales
# del primer indicador de cada variable latente ya no se fijan en 1.


sink("ModeloIntersepto")

# MODELO 1


model1 <- " #Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
AMBLABOR =~ PrPer + AmbTr
HABPROSOC =~ ComVrb + ComAfec + ConCl +Ans
STRESS =~ Ans + StrC + UsoDirec

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF+HABPROSOC
STRESS ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF+HABPROSOC
AMBLABOR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF +HABPROSOC
HABPROSOC ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF 

#Varianzas Covarianzas
FRbr ~~ AFrSem
CulFr ~~ ComVrb

# Interceptos
 "

fit1 <- cfa(model1, data = DBVL, orthogonal = TRUE)
summary(fit1, fit.measures=TRUE, standardized = TRUE)
#parameterEstimates(fit1, standardized = TRUE, ci = FALSE)
#semPaths(fit1, what = "par", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit1)
modindices(fit1, sort = TRUE, maximum.number = 10) 

sink()
# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit11 <- cfa(model1, data = DBVL, orthogonal = TRUE)
summary(fit11, fit.measures= TRUE, standardized = TRUE)
semPaths(fit11, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit11, sort = TRUE, maximum.number = 10) 

sink()