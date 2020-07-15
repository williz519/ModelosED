library(readxl)

# Modelo MNL

#Cargas las Bases de Datos
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos MNL/MNL_Modelo_2/"
setwd(workingDirectory)

db = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBValidacion.csv",sep="\t", dec=".",header=TRUE)
est= read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos MNL/MNL_Modelo_2/MNL_Modelo_2 Sin VNS_estimates.csv",sep=",", dec=".",header=TRUE
)
#ttt = readRDS("/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/ICLV3_ModoCond_model.rds")

names(db)

## Parametros estimados

asc_ruta1  =  est[1,2]  
asc_ruta2  =  est[2,2] 
asc_ruta3  =  est[3,2]
asc_rutaEC = est[4,2]
b_tt       = est[5,2]  
b_dt       = est[6,2]  
b_CongCD   = est[7,2] 
b_CongEF   = est[8,2]     
b_ACC_0    = est[9,2]   
b_ACC_1    = est[10,2]  
b_ACC_2    = est[11,2]       
b_NO_CAMFD = est[12,2] 
b_SI_CAMFD = est[13,2]  


V_1 = array()
V_2 = array()
V_3 = array()
V_4 = array()

for (i in 1:nrow(db)){
  V_1[i] = asc_ruta1  + b_tt * db$TIEMPOAlt1[i] + b_dt * db$DISTAlt1[i] + 
    b_CongCD*db$CONG_CD_A1[i] + b_CongEF*db$CONG_EF_A1[i] +
    b_ACC_0*db$ACC_A1_0[i] + b_ACC_1*db$ACC_A1_1[i] + b_ACC_2*db$ACC_A1_2[i] + 
    b_NO_CAMFD * db$NO_CAMFD_A1[i] + b_SI_CAMFD * db$SI_CAMFD_A1[i] 
  
  V_2[i] = asc_ruta2  + b_tt * db$TIEMPOAlt2[i] + b_dt * db$DISTAlt2[i]  + 
    b_CongCD*db$CONG_CD_A2[i] + b_CongEF*db$CONG_EF_A2[i] + 
    b_ACC_0*db$ACC_A2_0[i] + b_ACC_1*db$ACC_A2_1[i] + b_ACC_2*db$ACC_A2_2[i] + 
    b_NO_CAMFD * db$NO_CAMFD_A2[i] + b_SI_CAMFD * db$SI_CAMFD_A2[i] 
  
  V_3[i] = asc_ruta3  + b_tt * db$TIEMPOAlt3[i] + b_dt * db$DISTAlt3[i] + 
    b_CongCD*db$CONG_CD_A3[i] + b_CongEF*db$CONG_EF_A3[i] +  
    b_ACC_0*db$ACC_A3_0[i] + b_ACC_1*db$ACC_A3_1[i] + b_ACC_2*db$ACC_A3_2[i] +  
    b_NO_CAMFD * db$NO_CAMFD_A3[i] + b_SI_CAMFD * db$SI_CAMFD_A3[i] 
  
  V_4[i] = asc_rutaEC + b_tt * db$TIEMPOEC[i]   + b_dt * db$DISTEC[i]  + 
    b_CongCD*db$CONG_CD_EC[i] + b_CongEF*db$CONG_EF_EC[i] +
    b_ACC_0*db$ACC_EC_0[i] + b_ACC_1*db$ACC_EC_1[i] + b_ACC_2*db$ACC_EC_2[i] + 
    b_NO_CAMFD * db$NO_CAMFD_EC[i] + b_SI_CAMFD * db$SI_CAMFD_EC[i] 
}
P_r1 = array()
P_r2 = array()
P_r3 = array()
P_r4 = array()

for (i in 1:nrow(db)){
  P_r1[i] = exp(V_1[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r2[i] = exp(V_2[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r3[i] = exp(V_3[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r4[i] = exp(V_4[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))}

db$P_r1 <- P_r1
db$P_r2 <- P_r2
db$P_r3 <- P_r3
db$P_r4 <- P_r4

Eleccion = array()

for (i in 1:nrow(db)){
  if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r1[i]) {Eleccion[i] = 1}
  else
  {if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r2[i]) {Eleccion[i] = 2}
    else
    {if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r3[i]) {Eleccion[i] = 3}
      else
      {Eleccion[i] = 4
      }}}}

db$PREDICTED <- Eleccion

Conteo = array()
db$Conteo <- Conteo

table(db$CHOICE)
table(db$PREDICTED)

for (i in 1:nrow(db)){
  if (db$CHOICE[i] == db$PREDICTED[i]) {db$Conteo[i]=1}
  else
  {db$Conteo[i] = 0}
}

CP <- data.frame(db$CHOICE,db$PREDICTED, db$Conteo)
CP

table(db$Conteo)
