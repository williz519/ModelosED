# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
#library(dplyr)
rm(list = ls())

## Semilla
set.seed(1234)


workingDirectory="/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/Regret/3_Rutas"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)
library(caret)

### Initialise code
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "Modelo_Regret_3Rutas",
  modelDescr = "Modelos de arrepentimiento en Eleccion de Ruta",
  indivID    = "ViajeId"
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_3Rutas.csv",sep="\t", dec=".",header=TRUE)


# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta4  = 0,
              b_tt  = 0,  
              b_dt  = 0,
              b_Sem = 0, 
              b_Acc0 = 0, b_Acc1 = 0, b_Acc2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0,
              b_NO_PANEL = 0, b_SI_PANEL = 0,
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_NO_MTRP = 0, b_SI_MTRP = 0,
              
              
              #b_ACC_0_r1 = 0, b_ACC_0_r2 = 0,  b_ACC_0_r4 = 0,
              #b_ACC_1_r1 = 0, b_ACC_1_r2 = 0,  b_ACC_1_r4 = 0, 
              #b_ACC_2_r1 = 0, b_ACC_2_r2 = 0,  b_ACC_2_r4 = 0,
              
              #b_NO_CAMFD_r1 = 0, b_NO_CAMFD_r2 = 0,  b_NO_CAMFD_r4 = 0,
              #b_SI_CAMFD_r1 = 0, b_SI_CAMFD_r2 = 0,  b_SI_CAMFD_r4 = 0, 
              
              #b_NO_PANEL_r1 = 0, b_NO_PANEL_r2 = 0,  b_NO_PANEL_r4 = 0,
              #b_SI_PANEL_r1 = 0, b_SI_PANEL_r2 = 0,  b_SI_PANEL_r4 = 0,
              
              #b_NO_ZER_r1 = 0, b_NO_ZER_r2 = 0,  b_NO_ZER_r4 = 0,
              #b_SI_ZER_r1 = 0, b_SI_ZER_r2 = 0,  b_SI_ZER_r4 = 0,
               
              #b_No_MTRP_r1 = 0, b_No_MTRP_r2 = 0,  b_No_MTRP_r4 = 0,
              #b_Si_MTRP_r1 = 0, b_Si_MTRP_r2 = 0,  b_Si_MTRP_r4 = 0,
              
              b_No_Info = 0, b_Si_Info = 0,
              
              b_UsoCel_P = 0, #b_UsoCel_A = 0, 
              b_UsoCel_F = 0)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta2", "b_Acc0", "b_NO_CAMFD", "b_NO_PANEL", "b_NO_ZER", "b_NO_MTRP",
                 "b_Si_Info", "b_UsoCel_P")


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Crear una lista de probabilidades P
  P = list()
  
  ### Preparar componentes de arrepentimiento para variables categóricas.
  RSem_ruta1 = SEM_A1_km
  RSem_ruta2 = SEM_A2_km
  RSem_ruta4 = SEM_EC_km
  
  RACC0_2_1 = ACC_A2_0 - ACC_A1_0 
  RACC0_4_1 = ACC_EC_0 - ACC_A1_0
  RACC0_1_2 = ACC_A1_0 - ACC_A2_0
  RACC0_4_2 = ACC_EC_0 - ACC_A2_0
  RACC0_1_4 = ACC_A1_0 - ACC_EC_0
  RACC0_2_4 = ACC_A2_0 - ACC_EC_0
  
  RACC1_2_1 = ACC_A2_1 - ACC_A1_1
  RACC1_4_1 = ACC_EC_1 - ACC_A1_1
  RACC1_1_2 = ACC_A1_1 - ACC_A2_1
  RACC1_4_2 = ACC_EC_1 - ACC_A2_1
  RACC1_1_4 = ACC_A1_1 - ACC_EC_1
  RACC1_2_4 = ACC_A2_1 - ACC_EC_1
  
  RACC2_2_1 = ACC_A2_2 - ACC_A1_2
  RACC2_4_1 = ACC_EC_2 - ACC_A1_2
  RACC2_1_2 = ACC_A1_2 - ACC_A2_2
  RACC2_4_2 = ACC_EC_2 - ACC_A2_2
  RACC2_1_4 = ACC_A1_2 - ACC_EC_2
  RACC2_2_4 = ACC_A2_2 - ACC_EC_2
  
  RCamFDNO_2_1 = NO_CAMFD_A2 - NO_CAMFD_A1
  RCamFDNO_4_1 = NO_CAMFD_EC - NO_CAMFD_A1
  RCamFDNO_1_2 = NO_CAMFD_A1 - NO_CAMFD_A2
  RCamFDNO_4_2 = NO_CAMFD_EC - NO_CAMFD_A2
  RCamFDNO_1_4 = NO_CAMFD_A1 - NO_CAMFD_EC
  RCamFDNO_2_4 = NO_CAMFD_A1 - NO_CAMFD_EC
  
  RCamFDSI_2_1 = SI_CAMFD_A2 - SI_CAMFD_A1
  RCamFDSI_4_1 = SI_CAMFD_EC - SI_CAMFD_A1
  RCamFDSI_1_2 = SI_CAMFD_A1 - SI_CAMFD_A2
  RCamFDSI_4_2 = SI_CAMFD_EC - SI_CAMFD_A2
  RCamFDSI_1_4 = SI_CAMFD_A1 - SI_CAMFD_EC
  RCamFDSI_2_4 = SI_CAMFD_A1 - SI_CAMFD_EC
  
  RPanelNO_2_1 = NO_PANEL_A2 - NO_PANEL_A1
  RPanelNO_4_1 = NO_PANEL_EC - NO_PANEL_A1
  RPanelNO_1_2 = NO_PANEL_A1 - NO_PANEL_A2
  RPanelNO_4_2 = NO_PANEL_EC - NO_PANEL_A2
  RPanelNO_1_4 = NO_PANEL_A1 - NO_PANEL_EC
  RPanelNO_2_4 = NO_PANEL_A2 - NO_PANEL_EC
  
  RPanelSI_2_1 = SI_PANEL_A2 - SI_PANEL_A1
  RPanelSI_4_1 = SI_PANEL_EC - SI_PANEL_A1
  RPanelSI_1_2 = SI_PANEL_A1 - SI_PANEL_A2
  RPanelSI_4_2 = SI_PANEL_EC - SI_PANEL_A2
  RPanelSI_1_4 = SI_PANEL_A1 - SI_PANEL_EC
  RPanelSI_2_4 = SI_PANEL_A2 - SI_PANEL_EC
  
  RZerNO_2_1 = NO_ZER_A2 - NO_ZER_A1
  RZerNO_4_1 = NO_ZER_EC - NO_ZER_A1
  RZerNO_1_2 = NO_ZER_A1 - NO_ZER_A2
  RZerNO_4_2 = NO_ZER_EC - NO_ZER_A2
  RZerNO_1_4 = NO_ZER_A1 - NO_ZER_EC
  RZerNO_2_4 = NO_ZER_A2 - NO_ZER_EC
    
  RZerSI_2_1 = SI_ZER_A2 - SI_ZER_A1
  RZerSI_4_1 = SI_ZER_EC - SI_ZER_A1
  RZerSI_1_2 = SI_ZER_A1 - SI_ZER_A2
  RZerSI_4_2 = SI_ZER_EC - SI_ZER_A2
  RZerSI_1_4 = SI_ZER_A1 - SI_ZER_EC
  RZerSI_2_4 = SI_ZER_A2 - SI_ZER_EC
  
  RMtrNO_2_1 = NO_MTRP_A2 - NO_MTRP_A1
  RMtrNO_4_1 = NO_MTRP_EC - NO_MTRP_A1
  RMtrNO_1_2 = NO_MTRP_A1 - NO_MTRP_A2
  RMtrNO_4_2 = NO_MTRP_EC - NO_MTRP_A2
  RMtrNO_1_4 = NO_MTRP_A1 - NO_MTRP_EC
  RMtrNO_2_4 = NO_MTRP_A2 - NO_MTRP_EC
  
  RMtrSI_2_1 = SI_MTRP_A2 - SI_MTRP_A1
  RMtrSI_4_1 = SI_MTRP_EC - SI_MTRP_A1
  RMtrSI_1_2 = SI_MTRP_A1 - SI_MTRP_A2
  RMtrSI_4_2 = SI_MTRP_EC - SI_MTRP_A2
  RMtrSI_1_4 = SI_MTRP_A1 - SI_MTRP_EC
  RMtrSI_2_4 = SI_MTRP_A2 - SI_MTRP_EC
  
   
  
  ### List of regret functions: these must use the same names as in mnl_settings, order is irrelevant
  R = list()
  R[['ruta1']]  = asc_ruta1  - 
    log(1+exp(b_tt*(T_Alt_2 - T_Alt_1)*(1 + CG_Alt_1))) - 
    log(1+exp(b_tt*(T_Alt_4   - T_Alt_1)*(1 + CG_Alt_1))) - 
    log(1+exp(b_dt*(D_Alt_2 - D_Alt_1))) - 
    log(1+exp(b_dt*(D_Alt_4 - D_Alt_1))) -
    log(1+exp(b_Sem*(RSem_ruta2  - RSem_ruta1)))  - 
    log(1+exp(b_Sem*(RSem_ruta4  - RSem_ruta1)))  -
    log(1+exp(b_Acc0 * RACC0_2_1))  + log(1+exp(b_Acc0 * RACC0_4_1)) -
    log(1+exp(b_Acc1 * RACC1_2_1))  + log(1+exp(b_Acc1 * RACC1_4_1)) -
    log(1+exp(b_Acc2 * RACC2_2_1))  + log(1+exp(b_Acc2 * RACC2_4_1)) -
    log(1+exp(b_NO_CAMFD * RCamFDNO_2_1)) + log(1+exp(b_NO_CAMFD * RCamFDNO_4_1)) - 
    log(1+exp(b_SI_CAMFD * RCamFDSI_2_1)) + log(1+exp(b_SI_CAMFD * RCamFDSI_4_1)) -
    log(1+exp(b_NO_PANEL * RPanelNO_2_1)) + log(1+exp(b_NO_PANEL * RPanelNO_4_1)) -
    log(1+exp(b_SI_PANEL * RPanelSI_2_1)) + log(1+exp(b_SI_PANEL * RPanelSI_4_1)) -
    log(1+exp(b_NO_ZER * RZerNO_2_1)) + log(1+exp(b_NO_ZER * RZerNO_4_1)) -
    log(1+exp(b_SI_ZER * RZerSI_2_1)) + log(1+exp(b_SI_ZER * RZerSI_4_1)) -
    log(1+exp(b_NO_MTRP * RMtrNO_2_1)) + log(1+exp(b_NO_MTRP * RMtrNO_4_1)) -
    log(1+exp(b_SI_MTRP * RMtrSI_2_1)) + log(1+exp(b_SI_MTRP * RMtrSI_4_1))

    
  R[['ruta2']]  = asc_ruta2  - 
    log(1+exp(b_tt*(T_Alt_1 - T_Alt_2)*(1 + CG_Alt_2))) - 
    log(1+exp(b_tt*(T_Alt_4 - T_Alt_2)*(1 + CG_Alt_2))) - 
    log(1+exp(b_dt*(D_Alt_1 - D_Alt_2))) - 
    log(1+exp(b_dt*(D_Alt_4 - D_Alt_2))) -
    log(1+exp(b_Sem*(RSem_ruta1  - RSem_ruta2)))  - 
    log(1+exp(b_Sem*(RSem_ruta4  - RSem_ruta2)))  -
    log(1+exp(b_Acc0 * RACC0_1_2)) + log(1+exp(b_Acc0 * RACC0_4_2)) -
    log(1+exp(b_Acc1 * RACC1_1_2)) + log(1+exp(b_Acc1 * RACC1_4_2)) -
    log(1+exp(b_Acc2 * RACC2_1_2)) + log(1+exp(b_Acc2 * RACC2_4_2)) -
    log(1+exp(b_NO_CAMFD * RCamFDNO_1_2)) + log(1+exp(b_NO_CAMFD * RCamFDNO_4_2)) - 
    log(1+exp(b_SI_CAMFD * RCamFDSI_1_2)) + log(1+exp(b_SI_CAMFD * RCamFDSI_4_2)) -
    log(1+exp(b_NO_PANEL * RPanelNO_1_2)) + log(1+exp(b_NO_PANEL * RPanelNO_4_2)) -
    log(1+exp(b_SI_PANEL * RPanelSI_1_2)) + log(1+exp(b_SI_PANEL * RPanelSI_4_2)) -
    log(1+exp(b_NO_ZER * RZerNO_1_2)) + log(1+exp(b_NO_ZER * RZerNO_4_2)) -
    log(1+exp(b_SI_ZER * RZerSI_1_2)) + log(1+exp(b_SI_ZER * RZerSI_4_2)) -
    log(1+exp(b_NO_MTRP * RMtrNO_1_2)) + log(1+exp(b_NO_MTRP * RMtrNO_4_2)) -
    log(1+exp(b_SI_MTRP * RMtrSI_1_2)) + log(1+exp(b_SI_MTRP * RMtrSI_4_2))
    
  
  
  R[['ruta4']]  = asc_ruta4  - 
    log(1+exp(b_tt*(T_Alt_1 - T_Alt_4)*(1 + CG_Alt_4))) - 
    log(1+exp(b_tt*(T_Alt_2 - T_Alt_4)*(1 + CG_Alt_4))) - 
    log(1+exp(b_dt*(D_Alt_1 - D_Alt_4))) - 
    log(1+exp(b_dt*(D_Alt_2 - D_Alt_4))) -
    log(1+exp(b_Sem*(RSem_ruta1  - RSem_ruta4)))  - 
    log(1+exp(b_Sem*(RSem_ruta2  - RSem_ruta4)))  - 
    log(1+exp(b_Acc0*RACC0_1_4))  + log(1+exp(b_Acc0*RACC0_2_4)) -
    log(1+exp(b_Acc1*RACC1_1_4))  + log(1+exp(b_Acc1*RACC1_2_4)) -
    log(1+exp(b_Acc2*RACC2_1_4))  + log(1+exp(b_Acc2*RACC2_2_4)) -
    log(1+exp(b_NO_CAMFD * RCamFDNO_1_4)) + log(1+exp(b_NO_CAMFD * RCamFDNO_2_4)) - 
    log(1+exp(b_SI_CAMFD * RCamFDSI_1_4)) + log(1+exp(b_SI_CAMFD * RCamFDSI_2_4)) -
    log(1+exp(b_NO_PANEL * RPanelNO_1_4)) + log(1+exp(b_NO_PANEL * RPanelNO_2_4)) -
    log(1+exp(b_SI_PANEL * RPanelSI_1_4)) + log(1+exp(b_SI_PANEL * RPanelSI_2_4)) -
    log(1+exp(b_NO_ZER * RZerNO_1_4)) + log(1+exp(b_NO_ZER * RZerNO_2_4)) -
    log(1+exp(b_SI_ZER * RZerSI_1_4)) + log(1+exp(b_SI_ZER * RZerSI_2_4)) -
    log(1+exp(b_NO_MTRP * RMtrNO_1_4)) + log(1+exp(b_NO_MTRP * RMtrNO_2_4)) -
    log(1+exp(b_SI_MTRP * RMtrSI_1_4)) + log(1+exp(b_SI_MTRP * RMtrSI_2_4)) -
    log(1+exp(b_No_Info * SININFOTRF)) + log(1+exp(b_Si_Info * CONINFOTRF)) -
    log(1+exp(b_UsoCel_P * UsoCel_Poco)) +  log(1+exp(b_UsoCel_F * UsoCel_Frec))
  

  ### Define settings for RRM model, which is MNL with negative regret as utility
  mnl_settings <- list(
    alternatives = c(ruta1=1, ruta2=2, ruta4=4),
    avail        = list(ruta1=1, ruta2=1, ruta4=1),
    choiceVar    = CHOICE,
    V            = lapply(R, "*", -1)
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, modelOutput_settings=list(printPVal=TRUE) )

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, saveOutput_settings=list(printPVal=TRUE) )

