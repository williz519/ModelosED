
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

#MODELO MODO DE CONDUCCION 2 FACTORES

model2F_0 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + IgPare + OmLmVel + UsoPito + CulFr + UsoCel + + UsoDirec
FA_2 =~ PasoPeaton + UsoDirec  

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2F_0 <- cfa(model2F_0, data = MCond, orthogonal = TRUE)
summary(fit2F_0, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit2F_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit2F_0, sort = TRUE, maximum.number = 10) 


model2F_1 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + IgPare + OmLmVel + UsoPito + CulFr + UsoCel + + UsoDirec
FA_2 =~ PasoPeaton + UsoDirec  

# Regresiones
FA_1 ~ EXP_3  + HTRB_2  + USODISPMOB 

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40  + EXP_2 + USODISPMOB 
# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2F_1 <- cfa(model2F_1, data = MCond, orthogonal = TRUE)
summary(fit2F_1, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit2F_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit2F_1)
modindices(fit2F_1, sort = TRUE, maximum.number = 10) 






############### ##########  ########### ########### ############# ############# ############
############### ##########  ########### ########### ############# ############# ############

#sink("Modelo_Tres_Factores.txt")

#MODELO MODO DE CONDUCCION 3 FACTORES

model_3F_0 <- " #Variables Latentes
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

fit_0 <- cfa(model_3F_0, data = MCond, orthogonal = TRUE)
summary(fit_0, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit_0, sort = TRUE, maximum.number = 10) 


#MODELO MODO DE CONDUCCION 3 FACTORES

model_1 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + EnfCond + UsoPito + CulFr 
FA_2 =~ IgPare + OmLmVel + UsoCel
FA_3 =~ PasoPeaton + UsoDirec 


# Regresiones
FA_1 ~  HPICO  + CSECO + SININFOTRF + FA_2

FA_2 ~ ADULTO40  + HPICO + USODISPMOB 

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + EXP_2  + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit_1 <- cfa(model_1, data = MCond, orthogonal = TRUE)
summary(fit_1, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit_1, sort = TRUE, maximum.number = 10) 

sink()



############### ##########  ########### ########### ############# ############# ############
############### ##########  ########### ########### ############# ############# ############

#sink("Modelo_Muestra_Dos_Factores_alpha_005.txt")

# MODELO 1 Cuatro Factores Factores alpha 0.05 t value= 1.96

model1_0 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + CulFr 
FA_2 =~ Ans + StrC  
FA_3 =~ PrPer + AmbTr   
FA_4 =~ EnfCond + UsoCel + UsoPito 
FA_5 =~  ConCl + ComAfec
FA_6 =~ OmLmVel + IgPare 


# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_5

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_2

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_0 <- cfa(model1_0, data = MCond, orthogonal = TRUE)
summary(fit1_0, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_0, sort = TRUE, maximum.number = 10) 


model1_1 <- " #Variables Latentes
FA_1 =~ FRbr +  AFrSem + CulFr 
FA_2 =~ Ans + StrC  
FA_3 =~ PrPer + AmbTr   
FA_4 =~ EnfCond + UsoCel + UsoPito 
FA_5 =~  ConCl + ComAfec
FA_6 =~ OmLmVel + IgPare 


# Regresiones
FA_1 ~  EXP_3 +  HTRB_2 + USODISPMOB 

FA_2 ~ ADULTO40 + ADULTO60 + EXP_2 + EXP_3 + SININFOTRF  + USODISPMOB 

FA_3 ~ EDUBASICA  + EXP_2 + EXP_3 + EXP_4 + EXP_5  + FA_5

FA_4 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_5  + CSECO  + FA_1

FA_5 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_3  + USODISPMOB + FA_2

FA_6 ~ ADULTO40  + EXP_5 + HPICO  + CSECO + SININFOTRF  + USODISPMOB + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_1 <- cfa(model1_1, data = MCond, orthogonal = TRUE)
summary(fit1_1, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_1, sort = TRUE, maximum.number = 10) 

##################################################################################################

model2_0 <- " #Variables Latentes
FA_1 =~ FRbr  + AFrSem + CulFr      
FA_2 =~ Ans + StrC + ComAfec + ConCl     
FA_3 =~ PrPer + AmbTr    
FA_4 =~ EnfCond + UsoCel + UsoPito
FA_5 =~ OmLmVel + IgPare



# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_4

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_0 <- cfa(model2_0, data = MCond, orthogonal = TRUE)
summary(fit2_0, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit2_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit2_1)
modindices(fit2_0, sort = TRUE, maximum.number = 10) 


model2_1 <- " #Variables Latentes
FA_1 =~ FRbr  + AFrSem + CulFr      
FA_2 =~ Ans + StrC + ComAfec + ConCl     
FA_3 =~ PrPer + AmbTr    
FA_4 =~ EnfCond + UsoCel + UsoPito
FA_5 =~ OmLmVel + IgPare

# Regresiones
FA_1 ~ EXP_3 + HTRB_2 +  USODISPMOB 

FA_2 ~  EXP_2 + EXP_3  + USODISPMOB + FA_4

FA_3 ~ EDUBASICA  + EXP_2 + EXP_4 + EXP_5  + USODISPMOB

FA_4 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_2  + EXP_5  + CSECO  + FA_1

FA_5 ~ ADULTO40 + EXP_5  + HPICO  + CSECO + SININFOTRF  + USODISPMOB + + FA_1

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_1 <- cfa(model2_1, data = MCond, orthogonal = TRUE)
summary(fit2_1, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit2_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit2_1)
modindices(fit2_1, sort = TRUE, maximum.number = 10) 





##################################################################################################
model1_1_2 <- " #Variables Latentes
FA_MC1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito
FA_MC2 =~ OmLmVel + IgPare  + UsoCel
FA_MC3 =~ PasoPeaton + UsoDirec  
FA_P1 =~ ComAfec + ConCl
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ Ans + StrC 


# Regresiones
FA_MC1 ~ JOVEN30 + ADULTO40 + ADULTO60 + FA_P1

FA_MC2 ~ HPICO  + CSECO  + FA_MC1

FA_MC3 ~ EDUBASICA + JOVEN30  + EXP_2 + USODISPMOB

FA_P1 ~ JOVEN30 + ADULTO40 + ADULTO60  + USODISPMOB + FA_P3 

FA_P2 ~ EDUBASICA + EXP_2 ++  EXP_4 + EXP_5 + USODISPMOB

FA_P3 ~ EXP_3 + SININFOTRF  

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_1_2 <- cfa(model1_1_2, data = MCond, orthogonal = TRUE)
summary(fit1_1_2, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_1_2, sort = TRUE, maximum.number = 10) 



####################################################################################################

model1_2 <- " #Variables Latentes
FA_MC1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito + IgPare + OmLmVel + UsoCel
FA_MC3 =~ PasoPeaton + UsoDirec   
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ Ans + StrC + ComAfec + CulFr + ConCl


# Regresiones
FA_MC1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_MC3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB

FA_P3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_2 <- cfa(model1_2, data = MCond, orthogonal = TRUE)
summary(fit1_2, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_2, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_2, sort = TRUE, maximum.number = 10) 

model1_2_2 <- " #Variables Latentes
FA_MC1 =~ FRbr + EnfCond + AFrSem + CulFr + UsoPito + IgPare + OmLmVel + UsoCel
FA_MC3 =~ PasoPeaton + UsoDirec   
FA_P2 =~ PrPer + AmbTr 
FA_P3 =~ Ans + StrC + ComAfec + CulFr + ConCl


# Regresiones
FA_MC1 ~  EXP_3 + USODISPMOB

FA_MC3 ~ EDUBASICA + JOVEN30 + EXP_2 + USODISPMOB

FA_P2 ~ EDUBASICA + EXP_2 +  EXP_4 + EXP_5 + USODISPMOB

FA_P3 ~ EXP_2 + EXP_3 + SININFOTRF  + USODISPMOB 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_2_2 <- cfa(model1_2_2, data = MCond, orthogonal = TRUE)
summary(fit1_2_2, fit.measures= TRUE, standardized = TRUE)
#semPaths(fit1_2, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_2_2, sort = TRUE, maximum.number = 10) 

################################################################################################
################################################################################################
################################################################################################

###########################             #######################           #####################


model3_0 <- " #Variables Latentes
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
SININFOTRF  + USODISPMOB + FA_1 + FA_5 

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_3

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB 

FA_6 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1 

FA_7 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + HTRB_2 + HTRB_3 + HPICO  + CSECO + 
SININFOTRF  + USODISPMOB + FA_1 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit3_0 <- cfa(model3_0, data = MCond, orthogonal = TRUE)
summary(fit3_0, fit.measures= TRUE, standardized = TRUE)
semPaths(fit3_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit3_0)
modindices(fit3_0, sort = TRUE, maximum.number = 10) 


model3_1 <- " #Variables Latentes
FA_1 =~ FRbr + AFrSem + CulFr
FA_2 =~ PrPer + AmbTr 
FA_3 =~ ComAfec + ConCl + ComVrb
FA_4 =~ Ans + StrC
FA_5 =~ UsoDirec + PasoPeaton
FA_6 =~ IgPare + OmLmVel + UsoCel
FA_7 =~ UsoPito + EnfCond


# Regresiones
FA_1 ~  EXP_3 +  HTRB_2 + USODISPMOB

FA_2 ~ EDUBASICA  + ADULTO60 + EXP_2 + EXP_3 +  EXP_4 + EXP_5 + FA_3

FA_3 ~ EDUBASICA  + ADULTO60  + EXP_3 + FA_1 + FA_5 

FA_4 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60  + SININFOTRF + FA_3

FA_5 ~ EDUBASICA + JOVEN30 + ADULTO40  + EXP_2 + USODISPMOB 

FA_6 ~ ADULTO40 + EXP_5  + HPICO  + CSECO + SININFOTRF + FA_1 

FA_7 ~ JOVEN30 + ADULTO40 + ADULTO60 + EXP_2 + + EXP_5  + FA_1 

# Interceptos "

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit3_1 <- cfa(model3_1, data = MCond, orthogonal = TRUE)
summary(fit3_1, fit.measures= TRUE, standardized = TRUE)
semPaths(fit3_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit3_0)
modindices(fit3_1, sort = TRUE, maximum.number = 10) 
