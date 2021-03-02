
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

# Cargar Datos desde MAC
DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv", header = TRUE, sep = "\t")


MCond <- DBModLog
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

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/2. LV_ Escenarios/Escenario_2_alpha_0.10/"
setwd(workingDirectory)

sink("Modelo_Muestra_Dos_Factores_alpha_01.txt")

# MODELO 1 Dos Factores alpha 0.1 t value= 1.6

model1_0 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito
      FA_2 =~ PasoPeaton + UsoDirec + UsoCel 

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
EXP_4  +  HTRB_2 + HTRB_3 + HTRB_4 + HPICO  + CSECO + 
SININFOTRF + USOCINTURON + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
EXP_4  +  HTRB_2 + HTRB_3 + HTRB_4 + HPICO  + CSECO + 
SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_0 <- cfa(model1_0, data = MCond, orthogonal = TRUE)
summary(fit1_0, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_0, sort = TRUE, maximum.number = 10) 



model1_1 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito
      FA_2 =~ PasoPeaton + UsoDirec + UsoCel   

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB + JOVEN30
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_1 <- cfa(model1_1, data = MCond, orthogonal = TRUE)
summary(fit1_1, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_1, sort = TRUE, maximum.number = 10) 




model1_2 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito
      FA_2 =~ PasoPeaton + UsoDirec + UsoCel   

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB 
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB

#Covarianzas
UsoCel ~~ PasoPeaton

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_2 <- cfa(model1_2, data = MCond, orthogonal = TRUE)
summary(fit1_2, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_2, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_2, sort = TRUE, maximum.number = 10) 



model1_3 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito + UsoCel
      FA_2 =~ PasoPeaton + UsoDirec + UsoCel   

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB 
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB

#Covarianzas
UsoCel ~~ PasoPeaton

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_3 <- cfa(model1_3, data = MCond, orthogonal = TRUE)
summary(fit1_3, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_3, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_3, sort = TRUE, maximum.number = 10) 



model1_4 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito + UsoCel
      FA_2 =~ PasoPeaton + UsoDirec + UsoCel   

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB 
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel~~ IgPare

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_4 <- cfa(model1_4, data = MCond, orthogonal = TRUE)
summary(fit1_4, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_4, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_4, sort = TRUE, maximum.number = 10) 


model1_5 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito + UsoCel
      FA_2 =~ PasoPeaton + UsoDirec + UsoCel   

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB 
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel~~ IgPare
AFrSem ~~ UsoPito

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_5 <- cfa(model1_5, data = MCond, orthogonal = TRUE)
summary(fit1_5, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_4, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_5, sort = TRUE, maximum.number = 10) 


model1_6 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito + UsoCel
FA_2 =~ PasoPeaton + UsoDirec + UsoCel   

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB 
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB + FA_1

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel~~ IgPare
AFrSem ~~ UsoPito

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_6 <- cfa(model1_6, data = MCond, orthogonal = TRUE)
summary(fit1_6, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_4, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_6, sort = TRUE, maximum.number = 10) 


model1_7 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito + UsoCel
FA_2 =~ PasoPeaton + UsoDirec + UsoCel + CulFr 

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB 
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB + FA_1

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel~~ IgPare
AFrSem ~~ UsoPito

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_7 <- cfa(model1_7, data = MCond, orthogonal = TRUE)
summary(fit1_7, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_7, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_7, sort = TRUE, maximum.number = 10) 


model1_8 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoPito + UsoCel
FA_2 =~ PasoPeaton + UsoDirec + UsoCel + CulFr 

# Regresiones
FA_1 ~  USOCINTURON + USODISPMOB 
FA_2 ~ EDUBASICA + JOVEN30 + + ADULTO40 + EXP_2 + + HTRB_2 + HTRB_3 +
HTRB_4 + USOCINTURON + USODISPMOB + FA_1

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel~~ IgPare
AFrSem ~~ UsoPito
EnfCond ~~ UsoPito

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_8 <- cfa(model1_8, data = MCond, orthogonal = TRUE)
summary(fit1_8, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_8, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_8, sort = TRUE, maximum.number = 10) 






# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index00 <- broom::glance(fit1_0) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index01 <- broom::glance(fit1_1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit1_2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit1_3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index04 <- broom::glance(fit1_4) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index00, fit_index01, fit_index02, fit_index03, fit_index04, .id = "Modelo")

sink()


#######################################################################################################

#######################################################################################################


sink("Modelo_Muestra_Tres_Factores_alpha_01.txt")

# MODELO 2: Tres Factores alpha 0.1 t value= 1.6


model2_0 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
      FA_2 =~ OmLmVel + IgPare  
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
EXP_4  +  HTRB_2 + HTRB_3 + HTRB_4 + HPICO  + CSECO + 
SININFOTRF + USOCINTURON + USODISPMOB 

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
EXP_4  +  HTRB_2 + HTRB_3 + HTRB_4 + HPICO  + CSECO + 
SININFOTRF + USOCINTURON + USODISPMOB 

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
EXP_4  +  HTRB_2 + HTRB_3 + HTRB_4 + HPICO  + CSECO + 
SININFOTRF + USOCINTURON + USODISPMOB 

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_0 <- cfa(model2_0, data = MCond, orthogonal = TRUE)
summary(fit2_0, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_0,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_0, sort = TRUE, maximum.number = 10) 


model2_1 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
      FA_2 =~ OmLmVel + IgPare  
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ USOCINTURON + USODISPMOB + JOVEN30
FA_2 ~ JOVEN30+ ADULTO40 + EXP_1 + HPICO + USOCINTURON + USODISPMOB
FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + HTRB_2 + HTRB_3 + HTRB_4 + 
USOCINTURON + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_1 <- cfa(model2_1, data = MCond, orthogonal = TRUE)
summary(fit2_1, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_1, sort = TRUE, maximum.number = 10) 


model2_2 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
      FA_2 =~ OmLmVel + IgPare  
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ USOCINTURON + USODISPMOB 
FA_2 ~ ADULTO40 + EXP_1 + HPICO + USOCINTURON + USODISPMOB + FA_1
FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + HTRB_2 + HTRB_3 + HTRB_4 + 
USOCINTURON + USODISPMOB 

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_2 <- cfa(model2_2, data = MCond, orthogonal = TRUE)
summary(fit2_2, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_2,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_2, sort = TRUE, maximum.number = 10) 


model2_3 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
      FA_2 =~ OmLmVel + IgPare  
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ USOCINTURON + USODISPMOB 
FA_2 ~ ADULTO40 + EXP_1 + HPICO + USOCINTURON + USODISPMOB + FA_1
FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + HTRB_2 + HTRB_3 + HTRB_4 + 
USOCINTURON + USODISPMOB + FA_2

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_3 <- cfa(model2_3, data = MCond, orthogonal = TRUE)
summary(fit2_3, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_3,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_3, sort = TRUE, maximum.number = 10) 


model2_4 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
      FA_2 =~ OmLmVel + IgPare + UsoCel 
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel


# Regresiones
FA_1 ~ USOCINTURON + USODISPMOB 
FA_2 ~ ADULTO40 + EXP_1 + HPICO + USOCINTURON + USODISPMOB + FA_1
FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + HTRB_2 + HTRB_3 + HTRB_4 + 
USOCINTURON + USODISPMOB + FA_2


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_4 <- cfa(model2_4, data = MCond, orthogonal = TRUE)
summary(fit2_4, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_4,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_4, sort = TRUE, maximum.number = 10) 


model2_5 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
      FA_2 =~ OmLmVel + IgPare + UsoCel 
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel


# Regresiones
FA_1 ~ USOCINTURON + USODISPMOB 
FA_2 ~ ADULTO40 + EXP_1 + HPICO + USOCINTURON  + FA_1
FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + HTRB_2 + HTRB_3 + HTRB_4 + 
USOCINTURON + USODISPMOB + FA_2

#Var Cov
EnfCond ~~UsoPito

# Interceptos"


# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_5 <- cfa(model2_5, data = MCond, orthogonal = TRUE)
summary(fit2_5, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_5,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_5, sort = TRUE, maximum.number = 10) 



# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index00 <- broom::glance(fit2_0) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index01 <- broom::glance(fit2_1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2_2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit2_3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index04 <- broom::glance(fit2_4) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)


# Uniendo 
bind_rows(fit_index00, fit_index01, fit_index02, fit_index03, fit_index04, 
           .id = "Modelo")

sink()




#CREACION DE LAS VARIABLES LATENTES EN LA DB


ACTAGR <- 0.372*DBVL$FRbr+0.260*DBVL$EnfCond+0.422*DBVL$AFrSem+0.278*DBVL$CulFr+0.254*DBVL$OmLmVel
+0.233*DBVL$IgPare-0.816*DBVL$ComVrb

STRESS <- 1.283*DBVL$Ans+1.238*DBVL$ComAfec+1.012*DBVL$StrC+0.596*DBVL$ConsCl

#sink("Resultados.txt")