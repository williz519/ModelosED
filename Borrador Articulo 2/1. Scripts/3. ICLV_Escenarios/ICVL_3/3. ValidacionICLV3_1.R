
library(readxl)

# Modelo ICVL Se elimina la ruta 3

#Cargas las Bases de Datos
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/"
setwd(workingDirectory)

db = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBValidacion.csv",sep="\t", dec=".",header=TRUE)
est= read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/ICLV3_ModoCond Sin ruta 3_estimates.csv",sep=",", dec=".",header=TRUE)
#ttt = readRDS("/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/ICLV3_ModoCond_model.rds")

names(db)

db <-  db %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(CHOICE %in% c("3")))


## Parametros estimados

asc_ruta1  =  est[1,2]  
asc_ruta2  =  est[2,2]  
asc_rutaEC =  est[3,2]
b_tt       =  est[4,2]  
b_dt       =  est[5,2]  
b_CongAB   =  est[6,2]   
b_CongCD   =  est[7,2]  
b_CongEF   =  est[8,2]      
b_Sem_1    =  est[9,2] 
b_Sem_2    =  est[10,2]   
b_Sem_3    =  est[11,2]    
b_ACC_0    =  est[12,2]  
b_ACC_1    =  est[13,2]  
b_ACC_2    =  est[14,2]      
b_NO_CAMFD =  est[15,2]
b_SI_CAMFD =  est[16,2]  
b_NO_PANEL =  est[17,2]    
b_SI_PANEL =  est[18,2]
b_NO_ZER   =  est[19,2]   
b_SI_ZER   =  est[20,2]  
b_No_MTRP  =  est[21,2]  
b_Si_MTRP  =  est[22,2] 
lambda1    =  est[23,2]
lambda2    =  est[24,2]  
lambda3    =  est[25,2] 
gamma_USOCINTURON = est[26,2]   
gamma_USODISPMOB  = est[27,2]   
gamma_ADULTO40    = est[28,2]   
gamma_EXP_1       = est[29,2]   
gamma_HPICO       = est[30,2]   
gamma_LV1         = est[31,2]  
gamma_EDUBASICA   = est[32,2]   
gamma_JOVEN30     = est[33,2]  
gamma_EXP_2       = est[34,2] 
gamma_HTRB_2      = est[35,2]  
gamma_HTRB_3      = est[36,2]  
gamma_HTRB_4      = est[37,2]   
gamma_LV2         = est[38,2]  

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
P_r4 = array()

for (i in 1:nrow(db)){
  P_r1[i] = exp(V_1[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_4[i]))
  P_r2[i] = exp(V_2[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_4[i]))
  P_r4[i] = exp(V_4[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_4[i]))}

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
