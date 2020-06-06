
library(readxl)

# Modelo ICVL 

#Cargas las Bases de Datos
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/"
setwd(workingDirectory)

db = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBValidacion.csv",sep="\t", dec=".",header=TRUE)
est= read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/ICLV3_ModoCond_estimates.csv",sep=",", dec=".",header=TRUE
                    )
#ttt = readRDS("/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/ICLV3_ModoCond_model.rds")

names(db)

## Parametros estimados

asc_ruta1  =  est[1,2]  
asc_ruta2  =  0.8646  
asc_ruta3  =  0.0000
asc_rutaEC = 12.8467
b_tt       = 0.1722  
b_dt       = -1.0132   
b_CongAB   = -0.5783   
b_CongCD   = -0.8062   
b_CongEF   = 0.0000       
b_Sem_1    = 0.1213   
b_Sem_2    = -0.0676   
b_Sem_3    = 0.0000     
b_ACC_0    = 6.8977   
b_ACC_1    = 5.9318   
b_ACC_2    = 0.0000       
b_NO_CAMFD = 0.0000 
b_SI_CAMFD = -1.3621   
b_NO_PANEL = 0.0000      
b_SI_PANEL = -0.2954  
b_NO_ZER   =  0.0000     
b_SI_ZER   = -1.1917  
b_No_MTRP  =  0.0000   
b_Si_MTRP  = 1.0733 
lambda1    = 21.0782 
lambda2    = -6.5223   
lambda3    = 3.1789 
gamma_USOCINTURON = -0.0620   
gamma_USODISPMOB  = -0.0689   
gamma_ADULTO40    = -0.5334   
gamma_EXP_1       = -0.0853   
gamma_HPICO       = 0.2286   
gamma_LV1         = 3.3122  
gamma_EDUBASICA   = -0.4295   
gamma_JOVEN30     = -1.2027   
gamma_EXP_2       = 0.8542   
gamma_HTRB_2      = -3.1563   
gamma_HTRB_3      = -3.0822   
gamma_HTRB_4      = -2.8874   
gamma_LV2         = 0.1216   

LV_1 = array()
LV_2 = array()
LV_3 = array()

for (i in 1:nrow(db)){
  LV_1[i] = gamma_USOCINTURON * db$USOCINTURON[i] + gamma_USODISPMOB * db$USODISPMOB[i]
  LV_2[i] = gamma_ADULTO40*db$ADULTO40[i] + gamma_EXP_1*db$EXP_1[i] + gamma_HPICO*db$HPICO[i] + 
    gamma_USOCINTURON*db$USOCINTURON[i]  + gamma_LV1*LV_1[i]
  LV_3[i] = gamma_EDUBASICA * db$EDUBASICA[i] + gamma_JOVEN30*db$JOVEN30[i] +  gamma_ADULTO40*db$ADULTO40[i]+
    gamma_EXP_2*db$EXP_2[i] + gamma_HTRB_2*db$HTRB_2[i] + gamma_HTRB_3*db$HTRB_3[i] + gamma_HTRB_4*db$HTRB_4[i] + 
    gamma_USOCINTURON * db$USOCINTURON[i] + gamma_USODISPMOB * db$USODISPMOB[i] + gamma_LV2*LV_2[i]
}

V_1 = array()
V_2 = array()
V_3 = array()
V_4 = array()

for (i in 1:nrow(db)){
  V_1[i] = asc_ruta1  + b_tt * db$TIEMPOAlt1[i] + b_dt * db$DISTAlt1[i] + b_CongAB*db$CONG_AB_A1[i] + 
    b_CongCD*db$CONG_CD_A1[i] + b_CongEF*db$CONG_EF_A1[i] +
    b_Sem_1*db$SEMF_A1_1[i] + b_Sem_2*db$SEMF_A1_2[i] + b_Sem_3*db$SEMF_A1_3[i] + 
    b_ACC_0*db$ACC_A1_0[i] + b_ACC_1*db$ACC_A1_1[i] + b_ACC_2*db$ACC_A1_2[i] + 
    b_NO_CAMFD * db$NO_CAMFD_A1[i] + b_SI_CAMFD * db$SI_CAMFD_A1[i] +
    b_NO_PANEL * db$NO_PANEL_A1[i] + b_SI_PANEL * db$SI_PANEL_A1[i] + 
    b_NO_ZER * db$NO_ZER_A1[i] + b_SI_ZER * db$SI_ZER_A1[i] + b_No_MTRP * db$NO_MTRP_A1[i] +
    b_Si_MTRP * db$SI_MTRP_A1[i]
  
  V_2[i] = asc_ruta2  + b_tt * db$TIEMPOAlt2[i] + b_dt * db$DISTAlt2[i] + b_CongAB* db$CONG_AB_A2[i] + 
    b_CongCD*db$CONG_CD_A2[i] + b_CongEF*db$CONG_EF_A2[i] + 
    b_Sem_1*db$SEMF_A2_1[i] + b_Sem_2*db$SEMF_A2_2[i] + b_Sem_3*db$SEMF_A2_3[i] + 
    b_ACC_0*db$ACC_A2_0[i] + b_ACC_1*db$ACC_A2_1[i] + b_ACC_2*db$ACC_A2_2[i] + 
    b_NO_CAMFD * db$NO_CAMFD_A2[i] + b_SI_CAMFD * db$SI_CAMFD_A2[i] +
    b_NO_PANEL * db$NO_PANEL_A2[i] + b_SI_PANEL * db$SI_PANEL_A2[i] + 
    b_NO_ZER * db$NO_ZER_A2[i] + b_SI_ZER * db$SI_ZER_A2[i] + b_No_MTRP * db$NO_MTRP_A2[i] +
    b_Si_MTRP * db$SI_MTRP_A2[i]
  
  V_3[i] = asc_ruta3  + b_tt * db$TIEMPOAlt3[i] + b_dt * db$DISTAlt3[i] + b_CongAB*db$CONG_AB_A3[i] + 
    b_CongCD*db$CONG_CD_A3[i] + b_CongEF*db$CONG_EF_A3[i] +  
    b_Sem_1*db$SEMF_A3_1[i] + b_Sem_2*db$SEMF_A3_2[i] + b_Sem_3*db$SEMF_A3_3[i] + 
    b_ACC_0*db$ACC_A3_0[i] + b_ACC_1*db$ACC_A3_1[i] + b_ACC_2*db$ACC_A3_2[i] +  
    b_NO_CAMFD * db$NO_CAMFD_A3[i] + b_SI_CAMFD * db$SI_CAMFD_A3[i] +
    b_NO_PANEL * db$NO_PANEL_A3[i] + b_SI_PANEL * db$SI_PANEL_A3[i] + 
    b_NO_ZER * db$NO_ZER_A3[i] + b_SI_ZER * db$SI_ZER_A3[i] + b_No_MTRP * db$NO_MTRP_A3[i] +
    b_Si_MTRP * db$SI_MTRP_A3[i]
  
  V_4[i] = asc_rutaEC + b_tt * db$TIEMPOEC[i]   + b_dt * db$DISTEC[i] + b_CongAB*db$CONG_AB_EC[i] + 
    b_CongCD*db$CONG_CD_EC[i] + b_CongEF*db$CONG_EF_EC[i] +
    b_Sem_1*db$SEMF_EC_1[i] + b_Sem_2*db$SEMF_EC_2[i] + b_Sem_3*db$SEMF_EC_3[i] + 
    b_ACC_0*db$ACC_EC_0[i] + b_ACC_1*db$ACC_EC_1[i] + b_ACC_2*db$ACC_EC_2[i] + 
    b_NO_CAMFD * db$NO_CAMFD_EC[i] + b_SI_CAMFD * db$SI_CAMFD_EC[i] +
    b_NO_PANEL * db$NO_PANEL_EC[i] + b_SI_PANEL * db$SI_PANEL_EC[i] + 
    b_NO_ZER * db$NO_ZER_EC[i] + b_SI_ZER * db$SI_ZER_EC[i] + b_No_MTRP * db$NO_MTRP_EC[i] +
    b_Si_MTRP * db$SI_MTRP_EC[i] +
    lambda1 * LV_1[i] + lambda2 * LV_2[i] + lambda3 * LV_3[i]
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

