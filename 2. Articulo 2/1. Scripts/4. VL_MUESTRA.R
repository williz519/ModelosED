
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


############### ##########  ########### ########### ############# ############# ############
############### ##########  ########### ########### ############# ############# ############

#sink("Modelo_Muestra_Dos_Factores_alpha_005.txt")

# MODELO 1 Cuatro Factores Factores alpha 0.05 t value= 1.96

model1_0 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr
FA_2 =~ OmLmVel + IgPare 
FA_3 =~ PasoPeaton + UsoDirec + UsoCel  
FA_4 =~ UsoPito

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_0 <- cfa(model1_0, data = MCond, orthogonal = TRUE)
summary(fit1_0, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_0, sort = TRUE, maximum.number = 10) 



model1_1 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr
FA_2 =~ OmLmVel + IgPare 
FA_3 =~ PasoPeaton + UsoDirec + UsoCel  
FA_4 =~ UsoPito

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_1 <- cfa(model1_1, data = MCond, orthogonal = TRUE)
summary(fit1_1, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_1, sort = TRUE, maximum.number = 10) 




model1_2 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr
FA_2 =~ OmLmVel + IgPare 
FA_3 =~ PasoPeaton + UsoDirec + UsoCel  
FA_4 =~ UsoPito

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_2 <- cfa(model1_2, data = MCond, orthogonal = TRUE)
summary(fit1_2, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_2, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_2, sort = TRUE, maximum.number = 10) 



model1_3 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr
FA_2 =~ OmLmVel + IgPare + UsoCel
FA_3 =~ PasoPeaton + UsoDirec + UsoCel  
FA_4 =~ UsoPito

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_3 <- cfa(model1_3, data = MCond, orthogonal = TRUE)
summary(fit1_3, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_3, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_3, sort = TRUE, maximum.number = 10) 



model1_4 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr
FA_2 =~ OmLmVel + IgPare + UsoCel
FA_3 =~ PasoPeaton + UsoDirec + UsoCel  
FA_4 =~ UsoPito + EnfCond

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_4 <- cfa(model1_4, data = MCond, orthogonal = TRUE)
summary(fit1_4, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_4, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_4, sort = TRUE, maximum.number = 10) 


model1_5 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem + CulFr
FA_2 =~ OmLmVel + IgPare + UsoCel + UsoPito
FA_3 =~ PasoPeaton + UsoDirec + UsoCel  
FA_4 =~ UsoPito + EnfCond

# Regresiones
FA_1 ~ EDUBASICA +  EXP_3 + HTRB_2 + CSECO + SININFOTRF  + USODISPMOB

FA_2 ~ ADULTO40 +  EXP_2 + EXP_5 + HPICO  + CSECO + SININFOTRF  + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 +  EXP_2 + HTRB_2 + HTRB_3 + USODISPMOB

FA_4 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_5 + CSECO + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_5 <- cfa(model1_5, data = MCond, orthogonal = TRUE)
summary(fit1_5, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_4, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_5, sort = TRUE, maximum.number = 10) 


model1_6 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem + CulFr
FA_2 =~ OmLmVel + IgPare + UsoCel + UsoPito
FA_3 =~ PasoPeaton + UsoDirec + UsoCel  
FA_4 =~ UsoPito + EnfCond + UsoCel

# Regresiones
FA_1 ~ EXP_3 + HTRB_2 + CSECO + SININFOTRF  + USODISPMOB

FA_2 ~ ADULTO40 +  EXP_2 + EXP_5 + HPICO  + CSECO + SININFOTRF  + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 +  EXP_2 + HTRB_2 + HTRB_3 + USODISPMOB

FA_4 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_5 + CSECO + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_6 <- cfa(model1_6, data = MCond, orthogonal = TRUE)
summary(fit1_6, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_4, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_6, sort = TRUE, maximum.number = 10) 



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


#sink("Modelo_Muestra_Tres_Factores_alpha_005.txt")

# MODELO 2: tres Factores alpha 0.1 t value= 1.645


model2_0 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
FA_2 =~ OmLmVel + IgPare  
FA_3 =~ PasoPeaton + UsoDirec + UsoCel


# Regresiones
FA_1 ~ 

FA_2 ~  

FA_3 ~ 

# Interceptos "

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
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_1 <- sem(model2_1, data = MCond, orthogonal = TRUE)
summary(fit2_1, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_1, sort = TRUE, maximum.number = 10) 


model2_2 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito + UsoCel
FA_2 =~ OmLmVel + IgPare  
FA_3 =~ PasoPeaton + UsoDirec + UsoCel


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_2 <- sem(model2_2, data = MCond, orthogonal = TRUE)
summary(fit2_2, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_2,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_2, sort = TRUE, maximum.number = 10) 


model2_3 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito + UsoCel
FA_2 =~ OmLmVel + IgPare  
FA_3 =~ PasoPeaton + UsoDirec + UsoCel


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1 + FA_3

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_3 <- sem(model2_3, data = MCond, orthogonal = TRUE)
summary(fit2_3, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_3,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_3, sort = TRUE, maximum.number = 10) 


model2_4 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito + UsoCel + UsoDirec
FA_2 =~ OmLmVel + IgPare  
FA_3 =~ PasoPeaton + UsoDirec + UsoCel


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1 + FA_3

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

# Interceptos "
# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_4 <- sem(model2_4, data = MCond, orthogonal = TRUE)
summary(fit2_4, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_3,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_4, sort = TRUE, maximum.number = 10) 



# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index00 <- broom::glance(fit2_0) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index01 <- broom::glance(fit2_1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2_2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit2_3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)


# Uniendo 
bind_rows(fit_index00, fit_index01, fit_index02, fit_index03, fit_index04, fit_index05, 
           .id = "Modelo")

sink()


############### ##########  ########### ########### ############# ############# ############

#######################################################################################################

## PERSONALIDAD

#sink("Modelo_Muestra_Tres_Factores_alpha_005.txt")

# MODELO 2: Tres Factores alpha 0.05 t value= 1.96


model3_0 <- " #Variables Latentes Personalidad
FA_P1 =~ Ans + StrC 
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ ComVrb + ComAfec + ConCl


# Regresiones
FA_P1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit3_0 <- sem(model3_0, data = MCond, orthogonal = TRUE)
summary(fit3_0, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit3_0,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3_0, sort = TRUE, maximum.number = 10) 


model3_1 <- " #Variables Latentes Personalidad
FA_P1 =~ Ans + StrC 
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ ComVrb + ComAfec + ConCl


# Regresiones
FA_P1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_P1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit3_1 <- sem(model3_1, data = MCond, orthogonal = TRUE)
summary(fit3_1, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit3_0,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3_1, sort = TRUE, maximum.number = 10) 


model3_2 <- " #Variables Latentes Personalidad
FA_P1 =~ Ans + StrC 
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ ComVrb + ComAfec + ConCl


# Regresiones
FA_P1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_P3

FA_P3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_P1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit3_2 <- sem(model3_2, data = MCond, orthogonal = TRUE)
summary(fit3_2, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit3_0,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3_2, sort = TRUE, maximum.number = 10) 


model3_3 <- " #Variables Latentes Personalidad
FA_P1 =~ Ans + StrC + ComVrb
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ ComVrb + ComAfec + ConCl


# Regresiones
FA_P1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_P3

FA_P3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_P1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit3_3 <- sem(model3_3, data = MCond, orthogonal = TRUE)
summary(fit3_3, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit3_0,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3_3, sort = TRUE, maximum.number = 10) 


model3_4 <- " #Variables Latentes Personalidad
FA_P1 =~ Ans + StrC + ComVrb
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ ComVrb + ComAfec + ConCl


# Regresiones
FA_P1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 + HPICO + SININFOTRF  + USODISPMOB

FA_P2 ~ EDUBASICA + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5  + CSECO + FA_P3

FA_P3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_3  + HTRB_2 + HPICO + 
SININFOTRF  + USODISPMOB + FA_P1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit3_4 <- sem(model3_4, data = MCond, orthogonal = TRUE)
summary(fit3_4, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit3_0,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3_4, sort = TRUE, maximum.number = 10) 



#####   #########     ############      #############     ##############      #############     #######
#####   #########     ############      #############     ##############      #############     #######
#####   #########     ############      #############     ##############      #############     #######

# MODELO CONJUNTO

#Nueve Factores

model4_0 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr + OmLmVel 
FA_2 =~ Ans + StrC + ComAfec + ConCl
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton + UsoCel
FA_5 =~ ComVrb
FA_6 =~ IgPare
FA_7 =~ UsoPito


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_0 <- sem(model4_0, data = MCond, orthogonal = TRUE)
summary(fit4_0, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_0, sort = TRUE, maximum.number = 10) 


model4_1 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr + OmLmVel + IgPare + UsoPito + + UsoCel
FA_2 =~ Ans + StrC + ComAfec + ConCl + ComVrb + + CulFr
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton + UsoCel



# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_1 <- sem(model4_1, data = MCond, orthogonal = TRUE)
summary(fit4_1, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_1, sort = TRUE, maximum.number = 10) 


model4_2 <-  " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr + OmLmVel + IgPare + UsoPito + + UsoCel
FA_2 =~ Ans + StrC + ComAfec + ConCl + ComVrb + + CulFr
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton + UsoCel



# Regresiones
FA_1 ~  FA_2

FA_2 ~  EXP_2 + EXP_3 + USODISPMOB

FA_3 ~ EDUBASICA + EXP_2 + EXP_3 +  EXP_4 + EXP_5  + FA_2

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + HTRB_2 + HTRB_3  + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_2 <- sem(model4_2, data = MCond, orthogonal = TRUE)
summary(fit4_2, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_2, sort = TRUE, maximum.number = 10) 


################################################################################################

################################################################################################

################################################################################################

# 8 Factores

model4_3 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem + CulFr 
FA_2 =~ PrPer + AmbTr
FA_3 =~ ComAfec + ConCl + ComVrb
FA_4 =~ Ans + StrC 
FA_5 =~ UsoDirec + PasoPeaton
FA_6 =~ IgPare + OmLmVel + UsoCel
FA_7 =~ UsoPito + EnfCond



# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_3

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_4 + FA_5 + FA_6

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_3 <- sem(model4_3, data = MCond, orthogonal = TRUE)
summary(fit4_3, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_3, sort = TRUE, maximum.number = 10) 









model4_4 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem + CulFr 
FA_2 =~ PrPer + AmbTr
FA_3 =~ ComAfec + ConCl + ComVrb
FA_4 =~ Ans + StrC 
FA_5 =~ UsoDirec + PasoPeaton
FA_6 =~ IgPare + OmLmVel + UsoCel
FA_7 =~ UsoPito + EnfCond

# Regresiones
FA_1 ~ EDUBASICA + EXP_3 +  EXP_4 + HTRB_2 + CSECO + SININFOTRF  + USODISPMOB

FA_2 ~ EDUBASICA + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + CSECO + FA_3

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_3 + HPICO + SININFOTRF + FA_4 + FA_5 + FA_6

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 + SININFOTRF + USODISPMOB + FA_1

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40  + EXP_2 + HTRB_2 + USODISPMOB

FA_6 ~  ADULTO40 + EXP_2  + EXP_5 + HPICO  + CSECO + SININFOTRF  + USODISPMOB + FA_1

FA_7 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_5 + CSECO  + FA_1


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_4 <- sem(model4_4, data = MCond, orthogonal = TRUE)
summary(fit4_4, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_4, sort = TRUE, maximum.number = 10) 


model4_5 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr 
FA_2 =~ Ans + StrC + ComAfec + ConCl
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton + UsoCel
FA_5 =~ ComVrb
FA_6 =~ IgPare + OmLmVel
FA_7 =~ UsoPito


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_5 <- sem(model4_5, data = MCond, orthogonal = TRUE)
summary(fit4_5, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_5, sort = TRUE, maximum.number = 10)


model4_6 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem  + CulFr 
FA_2 =~ Ans + StrC + ComAfec + ConCl
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton + UsoCel
FA_5 =~ ComVrb
FA_6 =~ IgPare + OmLmVel + UsoCel
FA_7 =~ UsoPito + EnfCond


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_6 <- sem(model4_6, data = MCond, orthogonal = TRUE)
summary(fit4_6, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_6, sort = TRUE, maximum.number = 10)


model4_7 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem  + CulFr 
FA_2 =~ Ans + StrC + ComAfec + ConCl
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton + UsoCel
FA_5 =~ ComVrb
FA_6 =~ IgPare + OmLmVel + UsoCel
FA_7 =~ UsoPito + EnfCond


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_7 <- sem(model4_7, data = MCond, orthogonal = TRUE)
summary(fit4_7, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_7, sort = TRUE, maximum.number = 10)


model4_8 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem + CulFr 
FA_2 =~ Ans + StrC + ComAfec + ConCl + CulFr 
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton 
FA_5 =~ ComVrb + CulFr + PasoPeaton + UsoCel
FA_6 =~ IgPare + OmLmVel + UsoCel
FA_7 =~ UsoPito + EnfCond


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2 + FA_4

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_7

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_8 <- sem(model4_8, data = MCond, orthogonal = TRUE)
summary(fit4_8, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_8, sort = TRUE, maximum.number = 10)


model4_9 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem + CulFr 
FA_2 =~ Ans + StrC + ComAfec + ConCl + CulFr 
FA_3 =~ PrPer + AmbTr
FA_4 =~ UsoDirec + PasoPeaton 
FA_5 =~ ComVrb + CulFr + PasoPeaton + UsoCel
FA_6 =~ IgPare + OmLmVel + UsoCel
FA_7 =~ UsoPito + EnfCond


# Regresiones
FA_1 ~ EDUBASICA + CSECO + FA_2 + FA_4

FA_2 ~ EDUBASICA + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 + HTRB_2 + SININFOTRF + USODISPMOB + FA_7

FA_3 ~ EDUBASICA + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HPICO  + FA_2

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2 + USODISPMOB 

FA_5 ~ EDUBASICA  + EXP_2 + EXP_3 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + FA_2

FA_6 ~ EDUBASICA  + ADULTO40  + EXP_5  + HPICO  + CSECO + SININFOTRF  + USODISPMOB + FA_1

FA_7 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_5 + CSECO  + FA_1


# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_9 <- sem(model4_9, data = MCond, orthogonal = TRUE)
summary(fit4_9, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_9, sort = TRUE, maximum.number = 10)


model4_10 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr + OmLmVel
FA_2 =~ Ans + StrC 
FA_3 =~ PrPer + AmbTr
FA_4 =~ ComAfec + ConCl + ComVrb
FA_5 =~ UsoDirec + PasoPeaton + UsoCel
FA_6 =~ UsoCel + ComVrb 
FA_7 =~ UsoPito + EnfCond 
FA_8 =~ IgPare + OmLmVel 


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1 + FA_5

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

FA_8 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_10 <- sem(model4_10, data = MCond, orthogonal = TRUE)
summary(fit4_10, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_10, sort = TRUE, maximum.number = 10)


model4_11 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr + OmLmVel
FA_2 =~ Ans + StrC 
FA_3 =~ PrPer + AmbTr
FA_4 =~ ComAfec + ConCl + ComVrb
FA_5 =~ UsoDirec + PasoPeaton + UsoCel
FA_6 =~ UsoCel + ComVrb 
FA_7 =~ UsoPito + EnfCond 
FA_8 =~ IgPare + OmLmVel 


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1 + FA_5

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_2

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

FA_8 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_11 <- sem(model4_11, data = MCond, orthogonal = TRUE)
summary(fit4_11, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_10, sort = TRUE, maximum.number = 10)



model4_12 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr + OmLmVel
FA_2 =~ Ans + StrC 
FA_3 =~ PrPer + AmbTr
FA_4 =~ ComAfec + ConCl + ComVrb
FA_5 =~ UsoDirec + PasoPeaton + UsoCel
FA_6 =~ UsoCel + ComVrb 
FA_7 =~ UsoPito + EnfCond 
FA_8 =~ IgPare + OmLmVel 


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1 + FA_5

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_2 + FA_4

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

FA_8 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_12 <- sem(model4_12, data = MCond, orthogonal = TRUE)
summary(fit4_12, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_12, sort = TRUE, maximum.number = 10)



model4_13 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem  + CulFr + OmLmVel
FA_2 =~ Ans + StrC 
FA_3 =~ PrPer + AmbTr
FA_4 =~ ComAfec + ConCl + ComVrb
FA_5 =~ UsoDirec + PasoPeaton + UsoCel + CulFr
FA_6 =~ UsoCel + ComVrb 
FA_7 =~ UsoPito + EnfCond 
FA_8 =~ IgPare + OmLmVel 


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1 + FA_5

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_2 + FA_4

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

FA_8 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_13 <- sem(model4_13, data = MCond, orthogonal = TRUE)
summary(fit4_13, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_13, sort = TRUE, maximum.number = 10)


model4_14 <- " #Variables Latentes
FA_1 =~ FRbr  + AFrSem  + CulFr 
FA_2 =~ Ans + StrC 
FA_3 =~ PrPer + AmbTr
FA_4 =~ ComAfec + ConCl + ComVrb
FA_5 =~ UsoDirec + PasoPeaton + UsoCel + CulFr
FA_6 =~ UsoCel + ComVrb 
FA_7 =~ UsoPito + EnfCond 
FA_8 =~ IgPare + OmLmVel 


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_4

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1 + FA_5

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB  + FA_4

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

FA_8 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + 
HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF + USODISPMOB + FA_1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit4_14 <- sem(model4_14, data = MCond, orthogonal = TRUE)
summary(fit4_14, fit.measures=TRUE, standardized = TRUE)
#semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4_14, sort = TRUE, maximum.number = 10)
