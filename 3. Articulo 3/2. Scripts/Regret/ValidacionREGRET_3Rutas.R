library(readxl)

# Modelo MNL

#Cargas las Bases de Datos
rm(list = ls())

db = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_3Rutas.csv",sep="\t", dec=".",header=TRUE)

db_test = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBValidacion_3Rutas.csv",sep="\t", dec=".",header=TRUE)

resultados <-  read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/Regret/3_Rutas/Modelo_Regret_3Rutas_estimates.csv",sep=",", dec=".",header=TRUE )
#/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/Regret/3_Rutas/Modelo_Regret_3Rutas_OLD2_estimates.csv

#####  REGRET ###

db$RSem_ruta1 = db$SEM_A1_km
db$RSem_ruta2 = db$SEM_A2_km
db$RSem_ruta4 = db$SEM_EC_km

db$RACC0_2_1 = db$ACC_A2_0 - db$ACC_A1_0 
db$RACC0_4_1 = db$ACC_EC_0 - db$ACC_A1_0
db$RACC0_1_2 = db$ACC_A1_0 - db$ACC_A2_0
db$RACC0_4_2 = db$ACC_EC_0 - db$ACC_A2_0
db$RACC0_1_4 = db$ACC_A1_0 - db$ACC_EC_0
db$RACC0_2_4 = db$ACC_A2_0 - db$ACC_EC_0

db$RACC1_2_1 = db$ACC_A2_1 - db$ACC_A1_1
db$RACC1_4_1 = db$ACC_EC_1 - db$ACC_A1_1
db$RACC1_1_2 = db$ACC_A1_1 - db$ACC_A2_1
db$RACC1_4_2 = db$ACC_EC_1 - db$ACC_A2_1
db$RACC1_1_4 = db$ACC_A1_1 - db$ACC_EC_1
db$RACC1_2_4 = db$ACC_A2_1 - db$ACC_EC_1

db$RACC2_2_1 = db$ACC_A2_2 - db$ACC_A1_2
db$RACC2_4_1 = db$ACC_EC_2 - db$ACC_A1_2
db$RACC2_1_2 = db$ACC_A1_2 - db$ACC_A2_2
db$RACC2_4_2 = db$ACC_EC_2 - db$ACC_A2_2
db$RACC2_1_4 = db$ACC_A1_2 - db$ACC_EC_2
db$RACC2_2_4 = db$ACC_A2_2 - db$ACC_EC_2

db$RCamFDNO_2_1 = db$NO_CAMFD_A2 - db$NO_CAMFD_A1
db$RCamFDNO_4_1 = db$NO_CAMFD_EC - db$NO_CAMFD_A1
db$RCamFDNO_1_2 = db$NO_CAMFD_A1 - db$NO_CAMFD_A2
db$RCamFDNO_4_2 = db$NO_CAMFD_EC - db$NO_CAMFD_A2
db$RCamFDNO_1_4 = db$NO_CAMFD_A1 - db$NO_CAMFD_EC
db$RCamFDNO_2_4 = db$NO_CAMFD_A1 - db$NO_CAMFD_EC

db$RCamFDSI_2_1 = db$SI_CAMFD_A2 - db$SI_CAMFD_A1
db$RCamFDSI_4_1 = db$SI_CAMFD_EC - db$SI_CAMFD_A1
db$RCamFDSI_1_2 = db$SI_CAMFD_A1 - db$SI_CAMFD_A2
db$RCamFDSI_4_2 = db$SI_CAMFD_EC - db$SI_CAMFD_A2
db$RCamFDSI_1_4 = db$SI_CAMFD_A1 - db$SI_CAMFD_EC
db$RCamFDSI_2_4 = db$SI_CAMFD_A1 - db$SI_CAMFD_EC

db$RPanelNO_2_1 = db$NO_PANEL_A2 - db$NO_PANEL_A1
db$RPanelNO_4_1 = db$NO_PANEL_EC - db$NO_PANEL_A1
db$RPanelNO_1_2 = db$NO_PANEL_A1 - db$NO_PANEL_A2
db$RPanelNO_4_2 = db$NO_PANEL_EC - db$NO_PANEL_A2
db$RPanelNO_1_4 = db$NO_PANEL_A1 - db$NO_PANEL_EC
db$RPanelNO_2_4 = db$NO_PANEL_A2 - db$NO_PANEL_EC

db$RPanelSI_2_1 = db$SI_PANEL_A2 - db$SI_PANEL_A1
db$RPanelSI_4_1 = db$SI_PANEL_EC - db$SI_PANEL_A1
db$RPanelSI_1_2 = db$SI_PANEL_A1 - db$SI_PANEL_A2
db$RPanelSI_4_2 = db$SI_PANEL_EC - db$SI_PANEL_A2
db$RPanelSI_1_4 = db$SI_PANEL_A1 - db$SI_PANEL_EC
db$RPanelSI_2_4 = db$SI_PANEL_A2 - db$SI_PANEL_EC

db$RZerNO_2_1 = db$NO_ZER_A2 - db$NO_ZER_A1
db$RZerNO_4_1 = db$NO_ZER_EC - db$NO_ZER_A1
db$RZerNO_1_2 = db$NO_ZER_A1 - db$NO_ZER_A2
db$RZerNO_4_2 = db$NO_ZER_EC - db$NO_ZER_A2
db$RZerNO_1_4 = db$NO_ZER_A1 - db$NO_ZER_EC
db$RZerNO_2_4 = db$NO_ZER_A2 - db$NO_ZER_EC

db$RZerSI_2_1 = db$SI_ZER_A2 - db$SI_ZER_A1
db$RZerSI_4_1 = db$SI_ZER_EC - db$SI_ZER_A1
db$RZerSI_1_2 = db$SI_ZER_A1 - db$SI_ZER_A2
db$RZerSI_4_2 = db$SI_ZER_EC - db$SI_ZER_A2
db$RZerSI_1_4 = db$SI_ZER_A1 - db$SI_ZER_EC
db$RZerSI_2_4 = db$SI_ZER_A2 - db$SI_ZER_EC

db$RMtrNO_2_1 = db$NO_MTRP_A2 - db$NO_MTRP_A1
db$RMtrNO_4_1 = db$NO_MTRP_EC - db$NO_MTRP_A1
db$RMtrNO_1_2 = db$NO_MTRP_A1 - db$NO_MTRP_A2
db$RMtrNO_4_2 = db$NO_MTRP_EC - db$NO_MTRP_A2
db$RMtrNO_1_4 = db$NO_MTRP_A1 - db$NO_MTRP_EC
db$RMtrNO_2_4 = db$NO_MTRP_A2 - db$NO_MTRP_EC

db$RMtrSI_2_1 = db$SI_MTRP_A2 - db$SI_MTRP_A1
db$RMtrSI_4_1 = db$SI_MTRP_EC - db$SI_MTRP_A1
db$RMtrSI_1_2 = db$SI_MTRP_A1 - db$SI_MTRP_A2
db$RMtrSI_4_2 = db$SI_MTRP_EC - db$SI_MTRP_A2
db$RMtrSI_1_4 = db$SI_MTRP_A1 - db$SI_MTRP_EC
db$RMtrSI_2_4 = db$SI_MTRP_A2 - db$SI_MTRP_EC


#### VALIDACION DEL MODELO ##########

## Aciertos sobre la base del modelo

c <- resultados$Estimate        # coefficients b1, b2

V_1 = array()
V_2 = array()
V_4 = array()


V_1 = c[1]  - 
  log(1+exp(c[4]*(db$T_Alt_2 - db$T_Alt_1)*(1 + db$CG_Alt_1))) - 
  log(1+exp(c[4]*(db$T_Alt_4 - db$T_Alt_1)*(1 + db$CG_Alt_1))) - 
  log(1+exp(c[5]*(db$D_Alt_2 - db$D_Alt_1))) - 
  log(1+exp(c[5]*(db$D_Alt_4 - db$D_Alt_1))) -
  log(1+exp(c[6]*(db$RSem_ruta2 - db$RSem_ruta1)))  - 
  log(1+exp(c[6]*(db$RSem_ruta4 - db$RSem_ruta1)))  -
  log(1+exp(c[7]* db$RACC0_2_1))  + log(1+exp(c[7] * db$RACC0_4_1)) -
  log(1+exp(c[8] * db$RACC1_2_1))  + log(1+exp(c[8] * db$RACC1_4_1)) -
  log(1+exp(c[9] * db$RACC2_2_1))  + log(1+exp(c[9] * db$RACC2_4_1)) -
  log(1+exp(c[10] * db$RCamFDNO_2_1)) + log(1+exp(c[10] * db$RCamFDNO_4_1)) - 
  log(1+exp(c[11] * db$RCamFDSI_2_1)) + log(1+exp(c[11] * db$RCamFDSI_4_1)) -
  log(1+exp(c[12] * db$RPanelNO_2_1)) + log(1+exp(c[12] * db$RPanelNO_4_1)) -
  log(1+exp(c[13] * db$RPanelSI_2_1)) + log(1+exp(c[13] * db$RPanelSI_4_1)) -
  log(1+exp(c[14] * db$RZerNO_2_1)) + log(1+exp(c[14] * db$RZerNO_4_1)) -
  log(1+exp(c[15] * db$RZerSI_2_1)) + log(1+exp(c[15] * db$RZerSI_4_1)) -
  log(1+exp(c[16] * db$RMtrNO_2_1)) + log(1+exp(c[16] * db$RMtrNO_4_1)) -
  log(1+exp(c[17] * db$RMtrSI_2_1)) + log(1+exp(c[17] * db$RMtrSI_4_1))


V_2 = c[2]  - 
  log(1+exp(c[4]*(db$T_Alt_1 - db$T_Alt_2)*(1 + db$CG_Alt_2))) - 
  log(1+exp(c[4]*(db$T_Alt_4 - db$T_Alt_2)*(1 + db$CG_Alt_2))) - 
  log(1+exp(c[5]*(db$D_Alt_1 - db$D_Alt_2))) - 
  log(1+exp(c[5]*(db$D_Alt_4 - db$D_Alt_2))) -
  log(1+exp(c[6]*(db$RSem_ruta1  - db$RSem_ruta2)))  - 
  log(1+exp(c[6]*(db$RSem_ruta4  - db$RSem_ruta2)))  -
  log(1+exp(c[7] * db$RACC0_1_2)) + log(1+exp(c[7] * db$RACC0_4_2)) -
  log(1+exp(c[8] * db$RACC1_1_2)) + log(1+exp(c[8] * db$RACC1_4_2)) -
  log(1+exp(c[9] * db$RACC2_1_2)) + log(1+exp(c[9] * db$RACC2_4_2)) -
  log(1+exp(c[10] * db$RCamFDNO_1_2)) + log(1+exp(c[10] * db$RCamFDNO_4_2)) - 
  log(1+exp(c[11] * db$RCamFDSI_1_2)) + log(1+exp(c[11] * db$RCamFDSI_4_2)) -
  log(1+exp(c[12] * db$RPanelNO_1_2)) + log(1+exp(c[12] * db$RPanelNO_4_2)) -
  log(1+exp(c[13] * db$RPanelSI_1_2)) + log(1+exp(c[13] * db$RPanelSI_4_2)) -
  log(1+exp(c[14] * db$RZerNO_1_2)) + log(1+exp(c[14] * db$RZerNO_4_2)) -
  log(1+exp(c[15] * db$RZerSI_1_2)) + log(1+exp(c[15] * db$RZerSI_4_2)) -
  log(1+exp(c[16] * db$RMtrNO_1_2)) + log(1+exp(c[16] * db$RMtrNO_4_2)) -
  log(1+exp(c[17] * db$RMtrSI_1_2)) + log(1+exp(c[17] * db$RMtrSI_4_2))

V_4 = c[3] - 
  log(1+exp(c[4]*(db$T_Alt_1 - db$T_Alt_4)*(1 + db$CG_Alt_4))) - 
  log(1+exp(c[4]*(db$T_Alt_2 - db$T_Alt_4)*(1 + db$CG_Alt_4))) - 
  log(1+exp(c[5]*(db$D_Alt_1 - db$D_Alt_4))) - 
  log(1+exp(c[5]*(db$D_Alt_2 - db$D_Alt_4))) -
  log(1+exp(c[6]*(db$RSem_ruta1  - db$RSem_ruta4)))  - 
  log(1+exp(c[6]*(db$RSem_ruta2  - db$RSem_ruta4)))  - 
  log(1+exp(c[7]*db$RACC0_1_4))  + log(1+exp(c[7]*db$RACC0_2_4)) -
  log(1+exp(c[8]*db$RACC1_1_4))  + log(1+exp(c[8]*db$RACC1_2_4)) -
  log(1+exp(c[9]*db$RACC2_1_4))  + log(1+exp(c[9]*db$RACC2_2_4)) -
  log(1+exp(c[10] * db$RCamFDNO_1_4)) + log(1+exp(c[10] * db$RCamFDNO_2_4)) - 
  log(1+exp(c[11] * db$RCamFDSI_1_4)) + log(1+exp(c[11] * db$RCamFDSI_2_4)) -
  log(1+exp(c[12] * db$RPanelNO_1_4)) + log(1+exp(c[12] * db$RPanelNO_2_4)) -
  log(1+exp(c[13] * db$RPanelSI_1_4)) + log(1+exp(c[13] * db$RPanelSI_2_4)) -
  log(1+exp(c[14] * db$RZerNO_1_4)) + log(1+exp(c[14] * db$RZerNO_2_4)) -
  log(1+exp(c[15] * db$RZerSI_1_4)) + log(1+exp(c[15] * db$RZerSI_2_4)) -
  log(1+exp(c[16] * db$RMtrNO_1_4)) + log(1+exp(c[16] * db$RMtrNO_2_4)) -
  log(1+exp(c[17] * db$RMtrSI_1_4)) + log(1+exp(c[17] * db$RMtrSI_2_4)) -
  log(1+exp(c[18] * db$SININFOTRF)) + log(1+exp(c[19] * db$CONINFOTRF)) -
  log(1+exp(c[20] * db$UsoCel_Poco)) +  log(1+exp(c[21] * db$UsoCel_Frec)) 
  

P_r1 = array()
P_r2 = array()
P_r4 = array()

for (i in 1:nrow(db)){
  P_r1[i] = exp(-V_1[i])/(exp(-V_1[i])+exp(-V_2[i])+exp(-V_4[i]))
  P_r2[i] = exp(-V_2[i])/(exp(-V_1[i])+exp(-V_2[i])+exp(-V_4[i]))
  P_r4[i] = exp(-V_4[i])/(exp(-V_1[i])+exp(-V_2[i])+exp(-V_4[i]))}

db$P_r1 <- P_r1
db$P_r2 <- P_r2
db$P_r4 <- P_r4


Eleccion = array()

for (i in 1:nrow(db)){
  if (max(db$P_r1[i],db$P_r2[i],db$P_r4[i]) == db$P_r1[i]) {Eleccion[i] = 1}
  else
  {if (max(db$P_r1[i],db$P_r2[i],db$P_r4[i]) == db$P_r2[i]) {Eleccion[i] = 2}
    else
      {Eleccion[i] = 4
      }}}

Prob <- cbind(P_r1, P_r2, P_r4)
Prob
table(Real = db$CHOICE, predicted = Eleccion)
mean(factor(Eleccion, ordered = TRUE) == db$CHOICE)


Umbral = array()
err = 0.1

for (i in 1:nrow(db)){
  if (db$CHOICE[i] != Eleccion[i]){
    if (Eleccion[i] == 1){
      if (abs(db$P_r1[i]-db$P_r2[i]) < err){Umbral[i] = 2}
      else
      {if (abs(db$P_r1[i]-db$P_r4[i]) < err){Umbral[i] = 4}
        else
          {Umbral[i] = 1}}}
    else {if (Eleccion[i] == 2){
      if (abs(db$P_r2[i]-db$P_r1[i]) < err){Umbral[i] = 1}
      else
      {if (abs(db$P_r2[i]-db$P_r4[i]) < err){Umbral[i] = 4}
        else
          {Umbral[i] = 2}}}
      else
      { if (Eleccion[i] == 4){
        if (abs(db$P_r4[i]-db$P_r1[i]) < err){Umbral[i] = 1}
        else
        {if (abs(db$P_r4[i]-db$P_r2[i]) < err){Umbral[i] = 2}
          else
            {Umbral[i] = 4}}}}}}
  else
  {Umbral[i] = db$CHOICE[i]}}

#Umbral
#Eleccion
#NewProb <- cbind(P_r1, P_r2, P_r4, db$CHOICE, Eleccion, Umbral)
#NewProb

table(Real = db$CHOICE, predicted = Umbral)
mean(factor(Umbral, ordered = TRUE) == db$CHOICE)



#### VALIDACION DEL MODELO EN TESTING  ##########

## Aciertos sobre la base de testeo

db <- db_test

#####  REGRET ###

db$RSem_ruta1 = db$SEM_A1_km
db$RSem_ruta2 = db$SEM_A2_km
db$RSem_ruta4 = db$SEM_EC_km

db$RACC0_2_1 = db$ACC_A2_0 - db$ACC_A1_0 
db$RACC0_4_1 = db$ACC_EC_0 - db$ACC_A1_0
db$RACC0_1_2 = db$ACC_A1_0 - db$ACC_A2_0
db$RACC0_4_2 = db$ACC_EC_0 - db$ACC_A2_0
db$RACC0_1_4 = db$ACC_A1_0 - db$ACC_EC_0
db$RACC0_2_4 = db$ACC_A2_0 - db$ACC_EC_0

db$RACC1_2_1 = db$ACC_A2_1 - db$ACC_A1_1
db$RACC1_4_1 = db$ACC_EC_1 - db$ACC_A1_1
db$RACC1_1_2 = db$ACC_A1_1 - db$ACC_A2_1
db$RACC1_4_2 = db$ACC_EC_1 - db$ACC_A2_1
db$RACC1_1_4 = db$ACC_A1_1 - db$ACC_EC_1
db$RACC1_2_4 = db$ACC_A2_1 - db$ACC_EC_1

db$RACC2_2_1 = db$ACC_A2_2 - db$ACC_A1_2
db$RACC2_4_1 = db$ACC_EC_2 - db$ACC_A1_2
db$RACC2_1_2 = db$ACC_A1_2 - db$ACC_A2_2
db$RACC2_4_2 = db$ACC_EC_2 - db$ACC_A2_2
db$RACC2_1_4 = db$ACC_A1_2 - db$ACC_EC_2
db$RACC2_2_4 = db$ACC_A2_2 - db$ACC_EC_2

db$RCamFDNO_2_1 = db$NO_CAMFD_A2 - db$NO_CAMFD_A1
db$RCamFDNO_4_1 = db$NO_CAMFD_EC - db$NO_CAMFD_A1
db$RCamFDNO_1_2 = db$NO_CAMFD_A1 - db$NO_CAMFD_A2
db$RCamFDNO_4_2 = db$NO_CAMFD_EC - db$NO_CAMFD_A2
db$RCamFDNO_1_4 = db$NO_CAMFD_A1 - db$NO_CAMFD_EC
db$RCamFDNO_2_4 = db$NO_CAMFD_A1 - db$NO_CAMFD_EC

db$RCamFDSI_2_1 = db$SI_CAMFD_A2 - db$SI_CAMFD_A1
db$RCamFDSI_4_1 = db$SI_CAMFD_EC - db$SI_CAMFD_A1
db$RCamFDSI_1_2 = db$SI_CAMFD_A1 - db$SI_CAMFD_A2
db$RCamFDSI_4_2 = db$SI_CAMFD_EC - db$SI_CAMFD_A2
db$RCamFDSI_1_4 = db$SI_CAMFD_A1 - db$SI_CAMFD_EC
db$RCamFDSI_2_4 = db$SI_CAMFD_A1 - db$SI_CAMFD_EC

db$RPanelNO_2_1 = db$NO_PANEL_A2 - db$NO_PANEL_A1
db$RPanelNO_4_1 = db$NO_PANEL_EC - db$NO_PANEL_A1
db$RPanelNO_1_2 = db$NO_PANEL_A1 - db$NO_PANEL_A2
db$RPanelNO_4_2 = db$NO_PANEL_EC - db$NO_PANEL_A2
db$RPanelNO_1_4 = db$NO_PANEL_A1 - db$NO_PANEL_EC
db$RPanelNO_2_4 = db$NO_PANEL_A2 - db$NO_PANEL_EC

db$RPanelSI_2_1 = db$SI_PANEL_A2 - db$SI_PANEL_A1
db$RPanelSI_4_1 = db$SI_PANEL_EC - db$SI_PANEL_A1
db$RPanelSI_1_2 = db$SI_PANEL_A1 - db$SI_PANEL_A2
db$RPanelSI_4_2 = db$SI_PANEL_EC - db$SI_PANEL_A2
db$RPanelSI_1_4 = db$SI_PANEL_A1 - db$SI_PANEL_EC
db$RPanelSI_2_4 = db$SI_PANEL_A2 - db$SI_PANEL_EC

db$RZerNO_2_1 = db$NO_ZER_A2 - db$NO_ZER_A1
db$RZerNO_4_1 = db$NO_ZER_EC - db$NO_ZER_A1
db$RZerNO_1_2 = db$NO_ZER_A1 - db$NO_ZER_A2
db$RZerNO_4_2 = db$NO_ZER_EC - db$NO_ZER_A2
db$RZerNO_1_4 = db$NO_ZER_A1 - db$NO_ZER_EC
db$RZerNO_2_4 = db$NO_ZER_A2 - db$NO_ZER_EC

db$RZerSI_2_1 = db$SI_ZER_A2 - db$SI_ZER_A1
db$RZerSI_4_1 = db$SI_ZER_EC - db$SI_ZER_A1
db$RZerSI_1_2 = db$SI_ZER_A1 - db$SI_ZER_A2
db$RZerSI_4_2 = db$SI_ZER_EC - db$SI_ZER_A2
db$RZerSI_1_4 = db$SI_ZER_A1 - db$SI_ZER_EC
db$RZerSI_2_4 = db$SI_ZER_A2 - db$SI_ZER_EC

db$RMtrNO_2_1 = db$NO_MTRP_A2 - db$NO_MTRP_A1
db$RMtrNO_4_1 = db$NO_MTRP_EC - db$NO_MTRP_A1
db$RMtrNO_1_2 = db$NO_MTRP_A1 - db$NO_MTRP_A2
db$RMtrNO_4_2 = db$NO_MTRP_EC - db$NO_MTRP_A2
db$RMtrNO_1_4 = db$NO_MTRP_A1 - db$NO_MTRP_EC
db$RMtrNO_2_4 = db$NO_MTRP_A2 - db$NO_MTRP_EC

db$RMtrSI_2_1 = db$SI_MTRP_A2 - db$SI_MTRP_A1
db$RMtrSI_4_1 = db$SI_MTRP_EC - db$SI_MTRP_A1
db$RMtrSI_1_2 = db$SI_MTRP_A1 - db$SI_MTRP_A2
db$RMtrSI_4_2 = db$SI_MTRP_EC - db$SI_MTRP_A2
db$RMtrSI_1_4 = db$SI_MTRP_A1 - db$SI_MTRP_EC
db$RMtrSI_2_4 = db$SI_MTRP_A2 - db$SI_MTRP_EC

########################################################

VT_1 = array()
VT_2 = array()
VT_4 = array()

VT_1 = c[1]  - 
  log(1+exp(c[4]*(db$T_Alt_2 - db$T_Alt_1)*(1 + db$CG_Alt_1))) - 
  log(1+exp(c[4]*(db$T_Alt_4 - db$T_Alt_1)*(1 + db$CG_Alt_1))) - 
  log(1+exp(c[5]*(db$D_Alt_2 - db$D_Alt_1))) - 
  log(1+exp(c[5]*(db$D_Alt_4 - db$D_Alt_1))) -
  log(1+exp(c[6]*(db$RSem_ruta2 - db$RSem_ruta1)))  - 
  log(1+exp(c[6]*(db$RSem_ruta4 - db$RSem_ruta1)))  -
  log(1+exp(c[7]* db$RACC0_2_1))  + log(1+exp(c[7] * db$RACC0_4_1)) -
  log(1+exp(c[8] * db$RACC1_2_1))  + log(1+exp(c[8] * db$RACC1_4_1)) -
  log(1+exp(c[9] * db$RACC2_2_1))  + log(1+exp(c[9] * db$RACC2_4_1)) -
  log(1+exp(c[10] * db$RCamFDNO_2_1)) + log(1+exp(c[10] * db$RCamFDNO_4_1)) - 
  log(1+exp(c[11] * db$RCamFDSI_2_1)) + log(1+exp(c[11] * db$RCamFDSI_4_1)) -
  log(1+exp(c[12] * db$RPanelNO_2_1)) + log(1+exp(c[12] * db$RPanelNO_4_1)) -
  log(1+exp(c[13] * db$RPanelSI_2_1)) + log(1+exp(c[13] * db$RPanelSI_4_1)) -
  log(1+exp(c[14] * db$RZerNO_2_1)) + log(1+exp(c[14] * db$RZerNO_4_1)) -
  log(1+exp(c[15] * db$RZerSI_2_1)) + log(1+exp(c[15] * db$RZerSI_4_1)) -
  log(1+exp(c[16] * db$RMtrNO_2_1)) + log(1+exp(c[16] * db$RMtrNO_4_1)) -
  log(1+exp(c[17] * db$RMtrSI_2_1)) + log(1+exp(c[17] * db$RMtrSI_4_1))


VT_2 = c[2]  - 
  log(1+exp(c[4]*(db$T_Alt_1 - db$T_Alt_2)*(1 + db$CG_Alt_2))) - 
  log(1+exp(c[4]*(db$T_Alt_4 - db$T_Alt_2)*(1 + db$CG_Alt_2))) - 
  log(1+exp(c[5]*(db$D_Alt_1 - db$D_Alt_2))) - 
  log(1+exp(c[5]*(db$D_Alt_4 - db$D_Alt_2))) -
  log(1+exp(c[6]*(db$RSem_ruta1  - db$RSem_ruta2)))  - 
  log(1+exp(c[6]*(db$RSem_ruta4  - db$RSem_ruta2)))  -
  log(1+exp(c[7] * db$RACC0_1_2)) + log(1+exp(c[7] * db$RACC0_4_2)) -
  log(1+exp(c[8] * db$RACC1_1_2)) + log(1+exp(c[8] * db$RACC1_4_2)) -
  log(1+exp(c[9] * db$RACC2_1_2)) + log(1+exp(c[9] * db$RACC2_4_2)) -
  log(1+exp(c[10] * db$RCamFDNO_1_2)) + log(1+exp(c[10] * db$RCamFDNO_4_2)) - 
  log(1+exp(c[11] * db$RCamFDSI_1_2)) + log(1+exp(c[11] * db$RCamFDSI_4_2)) -
  log(1+exp(c[12] * db$RPanelNO_1_2)) + log(1+exp(c[12] * db$RPanelNO_4_2)) -
  log(1+exp(c[13] * db$RPanelSI_1_2)) + log(1+exp(c[13] * db$RPanelSI_4_2)) -
  log(1+exp(c[14] * db$RZerNO_1_2)) + log(1+exp(c[14] * db$RZerNO_4_2)) -
  log(1+exp(c[15] * db$RZerSI_1_2)) + log(1+exp(c[15] * db$RZerSI_4_2)) -
  log(1+exp(c[16] * db$RMtrNO_1_2)) + log(1+exp(c[16] * db$RMtrNO_4_2)) -
  log(1+exp(c[17] * db$RMtrSI_1_2)) + log(1+exp(c[17] * db$RMtrSI_4_2))

VT_4 = c[3] - 
  log(1+exp(c[4]*(db$T_Alt_1 - db$T_Alt_4)*(1 + db$CG_Alt_4))) - 
  log(1+exp(c[4]*(db$T_Alt_2 - db$T_Alt_4)*(1 + db$CG_Alt_4))) - 
  log(1+exp(c[5]*(db$D_Alt_1 - db$D_Alt_4))) - 
  log(1+exp(c[5]*(db$D_Alt_2 - db$D_Alt_4))) -
  log(1+exp(c[6]*(db$RSem_ruta1  - db$RSem_ruta4)))  - 
  log(1+exp(c[6]*(db$RSem_ruta2  - db$RSem_ruta4)))  - 
  log(1+exp(c[7]*db$RACC0_1_4))  + log(1+exp(c[7]*db$RACC0_2_4)) -
  log(1+exp(c[8]*db$RACC1_1_4))  + log(1+exp(c[8]*db$RACC1_2_4)) -
  log(1+exp(c[9]*db$RACC2_1_4))  + log(1+exp(c[9]*db$RACC2_2_4)) -
  log(1+exp(c[10] * db$RCamFDNO_1_4)) + log(1+exp(c[10] * db$RCamFDNO_2_4)) - 
  log(1+exp(c[11] * db$RCamFDSI_1_4)) + log(1+exp(c[11] * db$RCamFDSI_2_4)) -
  log(1+exp(c[12] * db$RPanelNO_1_4)) + log(1+exp(c[12] * db$RPanelNO_2_4)) -
  log(1+exp(c[13] * db$RPanelSI_1_4)) + log(1+exp(c[13] * db$RPanelSI_2_4)) -
  log(1+exp(c[14] * db$RZerNO_1_4)) + log(1+exp(c[14] * db$RZerNO_2_4)) -
  log(1+exp(c[15] * db$RZerSI_1_4)) + log(1+exp(c[15] * db$RZerSI_2_4)) -
  log(1+exp(c[16] * db$RMtrNO_1_4)) + log(1+exp(c[16] * db$RMtrNO_2_4)) -
  log(1+exp(c[17] * db$RMtrSI_1_4)) + log(1+exp(c[17] * db$RMtrSI_2_4)) -
  log(1+exp(c[18] * db$SININFOTRF)) + log(1+exp(c[19] * db$CONINFOTRF)) -
  log(1+exp(c[20] * db$UsoCel_Poco)) +  log(1+exp(c[21] * db$UsoCel_Frec)) 



PT_r1 = array()
PT_r2 = array()
PT_r4 = array()

for (i in 1:nrow(db)){
  PT_r1[i] = exp(-VT_1[i])/(exp(-VT_1[i])+exp(-VT_2[i])+exp(-VT_4[i]))
  PT_r2[i] = exp(-VT_2[i])/(exp(-VT_1[i])+exp(-VT_2[i])+exp(-VT_4[i]))
  PT_r4[i] = exp(-VT_4[i])/(exp(-VT_1[i])+exp(-VT_2[i])+exp(-VT_4[i]))}

db$PT_r1 <- PT_r1
db$PT_r2 <- PT_r2
db$PT_r4 <- PT_r4


EleccionTest = array()

for (i in 1:nrow(db)){
  if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r4[i]) == db$PT_r1[i]) {EleccionTest[i] = 1}
  else
  {if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r4[i]) == db$PT_r2[i]) {EleccionTest[i] = 2}
    else
    {EleccionTest[i] = 4
    }}}

#ProbT <- cbind(PT_r1, PT_r2, PT_r4)
#ProbT
table(predicted = EleccionTest, Real = db$CHOICE)
mean(factor(EleccionTest, ordered = TRUE) == db$CHOICE)


UmbralTest = array()
errT = 0.1

for (i in 1:nrow(db)){
  if (db$CHOICE[i] != EleccionTest[i]){
    if (EleccionTest[i] == 1){
      if (abs(db$PT_r1[i]-db$PT_r2[i]) < errT){UmbralTest[i] = 2}
      else
      {if (abs(db$PT_r1[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
        else
          {UmbralTest[i] = 1}}}
    else {if (EleccionTest[i] == 2){
      if (abs(db$PT_r2[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
      else
      {if (abs(db$PT_r2[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
        else
          {UmbralTest[i] = 2}}}
      else
      { if (EleccionTest[i] == 4){
        if (abs(db$PT_r4[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
        else
        {if (abs(db$PT_r4[i]-db$PT_r2[i]) < err){UmbralTest[i] = 2}
          else
            {UmbralTest[i] = 4}}}}}}
  else
  {UmbralTest[i] = db$CHOICE[i]}}

#UmbralTest
#EleccionTest
NewProbT <- cbind(PT_r1, PT_r2, PT_r4, db$CHOICE, EleccionTest, UmbralTest)
#NewProbT

table(Real = db$CHOICE, predicted = UmbralTest)
mean(factor(UmbralTest, ordered = TRUE) == db$CHOICE)


