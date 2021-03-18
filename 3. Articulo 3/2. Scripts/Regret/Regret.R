# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
#library(dplyr)
rm(list = ls())

## Semilla
set.seed(1234)


workingDirectory="/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/Prospectiva/3Rutas"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)
library(caret)

### Initialise code
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "Modelo_1_Regret_3Rutas",
  modelDescr = "Modelos de arrepentimiento en Eleccion de Ruta",
  indivID    = "ViajeId"
)


# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_3Rutas.csv",sep="\t", dec=".",header=TRUE)

#summary(database)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1 = 0, asc_ruta2 = 0, asc_ruta4 = 0,
              b_gain  = 0, b_loss = 0,
              b_dt  = 0,
              b_Sem = 0,
              b_ACC_0 = 0, b_ACC_1 = -1, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = -1, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_No_MTRP = 0, b_Si_MTRP = 0,
              b_No_Info = 0, b_Si_Info = 0,
              b_UsoCel_P = 0, #b_UsoCel_A = 0, 
              b_UsoCel_F = 0)


### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta2", "b_ACC_0", "b_NO_CAMFD","b_NO_PANEL" ,"b_No_MTRP", "b_NO_ZER", 
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
  RSem_ruta3 = SEM_A3_km
  RSem_ruta4 = SEM_EC_km
  
  RCong_ruta1 = b_CongAB_ruta1*CONG_AB_A1 + b_CongCD_ruta1*CONG_CD_A1 + b_CongEF_ruta1*CONG_EF_A1
  RCong_ruta2 = b_CongAB_ruta2*CONG_AB_A2 + b_CongCD_ruta2*CONG_CD_A2 + b_CongEF_ruta2*CONG_EF_A2
  RCong_ruta3 = b_CongAB_ruta3*CONG_AB_A3 + b_CongCD_ruta3*CONG_CD_A3 + b_CongEF_ruta3*CONG_EF_A3
  RCong_ruta4 = b_CongAB_ruta4*CONG_AB_EC + b_CongCD_ruta4*CONG_CD_EC + b_CongEF_ruta4*CONG_EF_EC
  
  RAcc_ruta1 = b_ACC_0_ruta1*ACC_A1_0 + b_ACC_1_ruta1*ACC_A1_1 + b_ACC_2_ruta1*ACC_A1_2
  RAcc_ruta2 = b_ACC_0_ruta2*ACC_A2_0 + b_ACC_1_ruta2*ACC_A2_1 + b_ACC_2_ruta2*ACC_A2_2
  RAcc_ruta3 = b_ACC_0_ruta3*ACC_A3_0 + b_ACC_1_ruta3*ACC_A3_1 + b_ACC_2_ruta3*ACC_A3_2
  RAcc_ruta4 = b_ACC_0_ruta4*ACC_EC_0 + b_ACC_1_ruta4*ACC_EC_1 + b_ACC_2_ruta4*ACC_EC_2
  
  RCamFd_ruta1 = b_NO_CAMFD_ruta1*NO_CAMFD_A1 + b_SI_CAMFD_ruta1 * SI_CAMFD_A1
  RCamFd_ruta2 = b_NO_CAMFD_ruta2*NO_CAMFD_A2 + b_SI_CAMFD_ruta2 * SI_CAMFD_A2
  RCamFd_ruta3 = b_NO_CAMFD_ruta3*NO_CAMFD_A3 + b_SI_CAMFD_ruta3 * SI_CAMFD_A3
  RCamFd_ruta4 = b_NO_CAMFD_ruta4*NO_CAMFD_EC + b_SI_CAMFD_ruta4 * SI_CAMFD_EC
  
  RPanel_ruta1 = b_NO_PANEL_ruta1*NO_PANEL_A1 + b_SI_PANEL_ruta1*SI_PANEL_A1
  RPanel_ruta2 = b_NO_PANEL_ruta2*NO_PANEL_A2 + b_SI_PANEL_ruta2*SI_PANEL_A2
  RPanel_ruta3 = b_NO_PANEL_ruta3*NO_PANEL_A3 + b_SI_PANEL_ruta3*SI_PANEL_A3
  RPanel_ruta4 = b_NO_PANEL_ruta4*NO_PANEL_EC + b_SI_PANEL_ruta4*SI_PANEL_EC
  
  RZer_r1 = b_NO_ZER_r1*NO_ZER_A1 + b_SI_ZER_r1*SI_ZER_A1
  RZer_r2 = b_NO_ZER_r2*NO_ZER_A2 + b_SI_ZER_r2*SI_ZER_A2
  RZer_r3 = b_NO_ZER_r3*NO_ZER_A3 + b_SI_ZER_r3*SI_ZER_A3
  RZer_r4 = b_NO_ZER_r4*NO_ZER_EC + b_SI_ZER_r4*SI_ZER_EC
  
  RMtrp_r1 = b_No_MTRP_r1*NO_MTRP_A1 + b_Si_MTRP_r1 * SI_MTRP_A1
  RMtrp_r2 = b_No_MTRP_r2*NO_MTRP_A2 + b_Si_MTRP_r1 * SI_MTRP_A2
  RMtrp_r3 = b_No_MTRP_r3*NO_MTRP_A3 + b_Si_MTRP_r1 * SI_MTRP_A3
  RMtrp_r4 = b_No_MTRP_r4*NO_MTRP_EC + b_Si_MTRP_r1 * SI_MTRP_EC
  
  
  ### List of regret functions: these must use the same names as in mnl_settings, order is irrelevant
  R = list()
  R[['ruta1']]  = asc_ruta1  + 
    log(1+exp(b_tt*(T_Alt_2 - T_Alt_1))) + 
    log(1+exp(b_tt*(T_Alt_3  - T_Alt_1))) + 
    log(1+exp(b_tt*(T_Alt_4   - T_Alt_1))) + 
    log(1+exp(b_dt*(D_Alt_2 - D_Alt_1))) + 
    log(1+exp(b_dt*(D_Alt_3 - D_Alt_1))) +
    log(1+exp(b_dt*(D_Alt_4 - D_Alt_1))) +
    log(1+exp(b_Sem(RSem_ruta2  - RSem_ruta1)))  + 
    log(1+exp(b_Sem(RSem_ruta3  - RSem_ruta1)))  + 
    log(1+exp(b_Sem(RSem_ruta4  - RSem_ruta1)))  +
    log(1+exp(b_Cong(RCong_ruta2  - RCong_ruta1)))  +
    log(1+exp(b_Cong(RCong_ruta3  - RCong_ruta1)))  +
    log(1+exp(b_Cong(RCong_ruta4  - RCong_ruta1)))  +
    log(1+exp(b_Acc(RAcc_ruta2  - RAcc_ruta1)))  +
    log(1+exp(b_Acc(RAcc_ruta3  - RAcc_ruta1)))  +
    log(1+exp(b_Acc(RAcc_ruta4  - RAcc_ruta1))) +
    log(1+exp(b_CamFd(RCamFd_ruta2 - RCamFd_ruta1))) +
    log(1+exp(b_CamFd(RCamFd_ruta3 - RCamFd_ruta1))) +
    log(1+exp(b_CamFd(RCamFd_ruta4 - RCamFd_ruta1))) +
    log(1+exp(b_Panel(RPanel_ruta2 - RPanel_ruta1))) +
    log(1+exp(b_Panel(RPanel_ruta3 - RPanel_ruta1))) +
    log(1+exp(b_Panel(RPanel_ruta4 - RPanel_ruta1))) +
    log(1+exp(b_Zer(RZer_r2 - RZer_r1))) +
    log(1+exp(b_Zer(RZer_r3 - RZer_r1))) +
    log(1+exp(b_Zer(RZer_r4 - RZer_r1))) +
    log(1+exp(b_Mtrp(RMtrp_r2 - RMtrp_r1))) +
    log(1+exp(b_Mtrp(RMtrp_r3 - RMtrp_r1))) +
    log(1+exp(b_Mtrp(RMtrp_r4 - RMtrp_r1)))
  
    
  R[['ruta2']]  = asc_ruta2  + 
    log(1+exp(b_tt*(T_Alt_1 - T_Alt_2))) + 
    log(1+exp(b_tt*(T_Alt_3 - T_Alt_2))) + 
    log(1+exp(b_tt*(T_Alt_4 - T_Alt_2))) + 
    log(1+exp(b_dt*(D_Alt_1 - D_Alt_2))) + 
    log(1+exp(b_dt*(D_Alt_3 - D_Alt_2))) +
    log(1+exp(b_dt*(D_Alt_4 - D_Alt_2))) +
    log(1+exp(b_Sem(RSem_ruta1  - RSem_ruta2)))  + 
    log(1+exp(b_Sem(RSem_ruta3  - RSem_ruta2)))  + 
    log(1+exp(b_Sem(RSem_ruta4  - RSem_ruta2)))  +
    log(1+exp(b_Cong(RCong_ruta1  - RCong_ruta2)))  +
    log(1+exp(b_Cong(RCong_ruta3  - RCong_ruta2)))  +
    log(1+exp(b_Cong(RCong_ruta4  - RCong_ruta2)))  +
    log(1+exp(b_Acc(RAcc_ruta1  - RAcc_ruta2)))  +
    log(1+exp(b_Acc(RAcc_ruta3  - RAcc_ruta2)))  +
    log(1+exp(b_Acc(RAcc_ruta4  - RAcc_ruta2))) +
    log(1+exp(b_CamFd(RCamFd_ruta1 - RCamFd_ruta2))) +
    log(1+exp(b_CamFd(RCamFd_ruta3 - RCamFd_ruta2))) +
    log(1+exp(b_CamFd(RCamFd_ruta4 - RCamFd_ruta2))) +
    log(1+exp(b_Panel(RPanel_ruta1 - RPanel_ruta2))) +
    log(1+exp(b_Panel(RPanel_ruta3 - RPanel_ruta2))) +
    log(1+exp(b_Panel(RPanel_ruta4 - RPanel_ruta2))) +
    log(1+exp(b_Zer(RZer_r1 - RZer_r2))) +
    log(1+exp(b_Zer(RZer_r3 - RZer_r2))) +
    log(1+exp(b_Zer(RZer_r4 - RZer_r2))) +
    log(1+exp(b_Mtrp(RMtrp_r1 - RMtrp_r2))) +
    log(1+exp(b_Mtrp(RMtrp_r3 - RMtrp_r2))) +
    log(1+exp(b_Mtrp(RMtrp_r4 - RMtrp_r2)))
  
    
  R[['ruta3']]  = asc_ruta3  + 
    log(1+exp(b_tt*(T_Alt_1 - T_Alt_3))) + 
    log(1+exp(b_tt*(T_Alt_2 - T_Alt_3))) + 
    log(1+exp(b_tt*(T_Alt_4 - T_Alt_3))) + 
    log(1+exp(b_dt*(D_Alt_1 - D_Alt_3))) + 
    log(1+exp(b_dt*(D_Alt_2 - D_Alt_3))) +
    log(1+exp(b_dt*(D_Alt_4 - D_Alt_3))) +
    log(1+exp(b_Sem(RSem_ruta1  - RSem_ruta3)))  + 
    log(1+exp(b_Sem(RSem_ruta2  - RSem_ruta3)))  + 
    log(1+exp(b_Sem(RSem_ruta4  - RSem_ruta3)))  +
    log(1+exp(b_Cong(RCong_ruta2  - RCong_ruta3)))  +
    log(1+exp(b_Cong(RCong_ruta1  - RCong_ruta3)))  +
    log(1+exp(b_Cong(RCong_ruta4  - RCong_ruta3)))  +
    log(1+exp(b_Acc(RAcc_ruta1  - RAcc_ruta3)))  +
    log(1+exp(b_Acc(RAcc_ruta2  - RAcc_ruta3)))  +
    log(1+exp(b_Acc(RAcc_ruta4  - RAcc_ruta3))) +
    log(1+exp(b_CamFd(RCamFd_ruta1 - RCamFd_ruta3))) +
    log(1+exp(b_CamFd(RCamFd_ruta2 - RCamFd_ruta3))) +
    log(1+exp(b_CamFd(RCamFd_ruta4 - RCamFd_ruta3))) +
    log(1+exp(b_Panel(RPanel_ruta1 - RPanel_ruta3))) +
    log(1+exp(b_Panel(RPanel_ruta2 - RPanel_ruta3))) +
    log(1+exp(b_Panel(RPanel_ruta4 - RPanel_ruta3))) +
    log(1+exp(b_Zer(RZer_r1 - RZer_r3))) +
    log(1+exp(b_Zer(RZer_r2 - RZer_r3))) +
    log(1+exp(b_Zer(RZer_r4 - RZer_r3))) +
    log(1+exp(b_Mtrp(RMtrp_r1 - RMtrp_r3))) +
    log(1+exp(b_Mtrp(RMtrp_r2 - RMtrp_r3))) +
    log(1+exp(b_Mtrp(RMtrp_r4 - RMtrp_r3)))
  
  R[['ruta4']]  = asc_ruta4  + 
    log(1+exp(b_tt*(T_Alt_1 - T_Alt_4))) + 
    log(1+exp(b_tt*(T_Alt_2 - T_Alt_4))) + 
    log(1+exp(b_tt*(T_Alt_3 - T_Alt_4))) + 
    log(1+exp(b_dt*(D_Alt_1 - D_Alt_4))) + 
    log(1+exp(b_dt*(D_Alt_2 - D_Alt_4))) +
    log(1+exp(b_dt*(D_Alt_3 - D_Alt_4))) +
    log(1+exp(b_Sem(RSem_ruta1  - RSem_ruta4)))  + 
    log(1+exp(b_Sem(RSem_ruta2  - RSem_ruta4)))  + 
    log(1+exp(b_Sem(RSem_ruta3  - RSem_ruta4)))  +
    log(1+exp(b_Cong(RCong_ruta1  - RCong_ruta4)))  +
    log(1+exp(b_Cong(RCong_ruta1  - RCong_ruta4)))  +
    log(1+exp(b_Cong(RCong_ruta3  - RCong_ruta4)))  +
    log(1+exp(b_Acc(RAcc_ruta1  - RAcc_ruta4)))  +
    log(1+exp(b_Acc(RAcc_ruta2  - RAcc_ruta4))) +
    log(1+exp(b_Acc(RAcc_ruta3  - RAcc_ruta4))) +
    log(1+exp(b_CamFd(RCamFd_ruta1 - RCamFd_ruta4))) +
    log(1+exp(b_CamFd(RCamFd_ruta2 - RCamFd_ruta4))) +
    log(1+exp(b_CamFd(RCamFd_ruta3 - RCamFd_ruta4))) +
    log(1+exp(b_Panel(RPanel_ruta1 - RPanel_ruta4))) +
    log(1+exp(b_Panel(RPanel_ruta2 - RPanel_ruta4))) +
    log(1+exp(b_Panel(RPanel_ruta3 - RPanel_ruta4))) +
    log(1+exp(b_Zer(RZer_r1 - RZer_r4))) +
    log(1+exp(b_Zer(RZer_r2 - RZer_r4))) +
    log(1+exp(b_Zer(RZer_r3 - RZer_r4))) +
    log(1+exp(b_Mtrp(RMtrp_r1 - RMtrp_r4))) +
    log(1+exp(b_Mtrp(RMtrp_r2 - RMtrp_r4))) +
    log(1+exp(b_Mtrp(RMtrp_r3 - RMtrp_r4)))
  

  ### Define settings for RRM model, which is MNL with negative regret as utility
  mnl_settings <- list(
    alternatives = c(ruta1=1, ruta2=2, ruta3=3, ruta4=4),
    avail        = list(ruta1=1, ruta2=1, ruta3=1, ruta4=1),
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

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

