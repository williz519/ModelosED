
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



#Creacion de la base de datos para el modelo logit multivariado
#write.table(DBVL, 
#        file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL1.csv", sep="\t", dec=".")

#write.csv2(DBVL, 
#           file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL1.csv", sep="\t", dec=".")

names(DBVL)



# ESPECIFICACION DEL MODELO DE VARIABLES LATENTES.


# Nota: Se restringe todas las covarianzasde las variables latentes en el modelo
# CFA para que sean ortogonales con la opcion orthogonal = TRUE

# Si se desea corregir las variaciones de todas las variables latentes en un modelo
# CFA a la unidad, se utiliza el argumento std.lv = TRUE, pero las cargas factoriales
# del primer indicador de cada variable latente ya no se fijan en 1.

sink("ResultadosMODELO1.txt")

# MODELO 1

model1 <- " #Variables Latentes
      ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
      STRESS =~ Ans + ComAfec + StrC
      AMBLABOR =~ PrPers + AmbT + ConsCl
      CONDSEG =~  CinSeg + UsDirec
      HABPROSOC =~ ComVrb
      
      # Regresiones
      ACTAGR ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF
      STRESS ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF
      AMBLABOR ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF 
      CONDSEG ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF 
      HABPROSOC ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF  

      # Interceptos"
      

fit1 <- cfa(model1, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit1, fit.measures=TRUE, standardized = TRUE)
#parameterEstimates(fit1, standardized = TRUE, ci = FALSE)
semPaths(fit1, what = "par", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit1)
modindices(fit1, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit11 <- cfa(model1, data = DBVL, orthogonal = TRUE)
summary(fit11, fit.measures= TRUE, standardized = TRUE)
semPaths(fit11, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit11, sort = TRUE, maximum.number = 10) 

sink()


# Indices de ajuste más comunes: CFI (>= .95), TLI (>= .95), RMSEA (<= .05) y SRMR (<= .06).

# Los indices de modificación son valores que nos brindarán una orientación acerca de la 
# re-especificación de la estructura factorial evaluada inicialmente.

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit1, sort = TRUE, maximum.number = 10)  

# Los Indices de modificacion representan la disminucion en el valor de chi-cuadrado 
# que se produciria en caso se realizar? lo sugerido.


sink("ResultadosMODELO2.txt")

# MODELO 2

model2 <- " #Variables Latentes
      ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF
STRESS ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF
AMBLABOR ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF 
CONDSEG ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF 
HABPROSOC ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF  

# Varianzas - Covarianzas
ACTAGR ~~ STRESS

# Interceptos"

fit2 <- cfa(model2, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit2, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit21 <- cfa(model2, data = DBVL, orthogonal = TRUE)
summary(fit21, fit.measures= TRUE, standardized = TRUE)
semPaths(fit21, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit21, sort = TRUE, maximum.number = 10) 

sink()


sink("ResultadosMODELO3.txt")

#MODELO 3

model3 <- " #Variables Latentes
      ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF
STRESS ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF
AMBLABOR ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + ACTAGR + STRESS
CONDSEG ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF 
HABPROSOC ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF  

# Varianzas - Covarianzas
ACTAGR ~~ STRESS

# Interceptos"

fit3 <- cfa(model3, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit3, fit.measures=TRUE, standardized = TRUE)
semPaths(fit3,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit31 <- cfa(model3, data = DBVL, orthogonal = TRUE)
summary(fit31, fit.measures= TRUE, standardized = TRUE)
semPaths(fit31, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit31, sort = TRUE, maximum.number = 10) 

sink()


#MODELO 4

model4 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb
STRESS =~  ConsCl

# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR

#Varianzas - Covarianzas
ACTAGR ~~ STRESS

# Interceptos"

fit4 <- lavaan::cfa(model4, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit4, fit.measures=TRUE, standardized = TRUE)
semPaths(fit4,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4, sort = TRUE, maximum.number = 10) 


# MODELO 5

model5 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR

#Varianzas - Covarianzas
ACTAGR ~~ STRESS

# Interceptos"

fit5 <- lavaan::cfa(model5, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit5, fit.measures=TRUE, standardized = TRUE)
semPaths(fit5,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit5, sort = TRUE, maximum.number = 10) 



# MODELO 6

model6 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS

# Interceptos"

fit6 <- lavaan::cfa(model6, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit6, fit.measures=TRUE, standardized = TRUE)
semPaths(fit6,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit6, sort = TRUE, maximum.number = 10) 


# MODELO 7

model7 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb

# Interceptos"

fit7 <- lavaan::cfa(model7, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit7, fit.measures=TRUE, standardized = TRUE)
semPaths(fit7,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit7, sort = TRUE, maximum.number = 10) 


# MODELO 8
model8 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans

# Interceptos"

fit8 <- lavaan::cfa(model8, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit8, fit.measures=TRUE, standardized = TRUE)
semPaths(fit8,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit8, sort = TRUE, maximum.number = 10) 


# MODELO 9

model9 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem

# Interceptos"

fit9 <- lavaan::cfa(model9, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit9, fit.measures=TRUE, standardized = TRUE)
semPaths(fit9,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit9, sort = TRUE, maximum.number = 10) 



# MODELO 10

model10 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem

# Interceptos"

fit10 <- lavaan::cfa(model10, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit10, fit.measures=TRUE, standardized = TRUE)
semPaths(fit10,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit10, sort = TRUE, maximum.number = 10) 


# MODELO 11

model11 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit11 <- lavaan::cfa(model11, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit11, fit.measures=TRUE, standardized = TRUE)
semPaths(fit11,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit11, sort = TRUE, maximum.number = 10) 


# MODELO 12
model12 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + StrC
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR + STRESS + AMBLABOR + CONDSEG + HABPROSOC ~ EDUBASICA + 
JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit12 <- lavaan::cfa(model12, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit12, fit.measures=TRUE, standardized = TRUE)
semPaths(fit12,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit12, sort = TRUE, maximum.number = 10) 



# MODELO 13
model13 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + StrC
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR ~ EDUBASICA  + TIEMPO_PROFESION + HORASTRABAJO
STRESS ~ JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO
AMBLABOR ~ EDUBASICA + ADULTO + HORASTRABAJO
CONDSEG ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION 
HABPROSOC ~ EDUBASICA + JOVEN + ADULTO + HORASTRABAJO 

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit13 <- lavaan::cfa(model13, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit13, fit.measures=TRUE, standardized = TRUE)
semPaths(fit13,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit13, sort = TRUE, maximum.number = 10) 


# MODELO 14

model14 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + StrC
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR ~ EDUBASICA + HORASTRABAJO
STRESS ~ JOVEN + ADULTO + HORASTRABAJO
AMBLABOR ~ EDUBASICA + ADULTO 
CONDSEG ~ EDUBASICA + JOVEN  + TIEMPO_PROFESION 
HABPROSOC ~  JOVEN + ADULTO + HORASTRABAJO 

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit14 <- lavaan::cfa(model14, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit14, fit.measures=TRUE, standardized = TRUE)
semPaths(fit14,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit14, sort = TRUE, maximum.number = 10) 


#MODELO 15

model15 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + StrC
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR ~  HORASTRABAJO
STRESS ~ JOVEN + HORASTRABAJO
AMBLABOR ~  ADULTO 
CONDSEG ~ EDUBASICA  + TIEMPO_PROFESION 
HABPROSOC ~  ADULTO + HORASTRABAJO 

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit15 <- lavaan::cfa(model15, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit15, fit.measures=TRUE, standardized = TRUE)
semPaths(fit15,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit15, sort = TRUE, maximum.number = 10)


# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index <- broom::glance(fit1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index04 <- broom::glance(fit4) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index05 <- broom::glance(fit5) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index06 <- broom::glance(fit6) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index07 <- broom::glance(fit7) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index08 <- broom::glance(fit8) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index09 <- broom::glance(fit9) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index10 <- broom::glance(fit10) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index11 <- broom::glance(fit11) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index12 <- broom::glance(fit12) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index13 <- broom::glance(fit13) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)


# Uniendo 
bind_rows(fit_index, fit_index02, fit_index03, fit_index04, fit_index05, fit_index06, 
          fit_index07, fit_index08, fit_index09, fit_index10, fit_index11,
          fit_index12, fit_index13, fit_index14, fit_index15, .id = "Modelo")



#CREACION DE LAS VARIABLES LATENTES EN LA DB

ACTAGR <- 0.372*DBVL$FRbr+0.260*DBVL$EnfCond+0.422*DBVL$AFrSem+0.278*DBVL$CulFr+0.254*DBVL$OmLmVel
          +0.233*DBVL$IgPare-0.816*DBVL$ComVrb

STRESS <- 1.283*DBVL$Ans+1.238*DBVL$ComAfec+1.012*DBVL$StrC+0.596*DBVL$ConsCl

AMBLABOR  <- DBVL$PrPers + DBVL$AmbT + DBVL$ConsCl
CONDSEG   <- DBVL$CinSeg + DBVL$UsDirec + DBVL$StrC
HABPROSOC <- DBVL$ComVrb + DBVL$ComAfec


DBVL

ACTAGR

DB<- cbind(DBVL,ACTAGR,STRESS)

DB

getwd() 

#sink("Resultados.txt")