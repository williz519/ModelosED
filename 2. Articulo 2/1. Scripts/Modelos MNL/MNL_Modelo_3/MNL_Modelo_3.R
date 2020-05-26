# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/1. Articulo 2/1. Scripts/Modelos MNL/MNL_Modelo_3"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "MNL_Modelo_3",
  modelDescr = "MNL Modelo  ",
  indivID    = "ViajeId",
  nCores     = 1
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/1. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
names(database)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_rutaEC  = 0,
              b_tt  = 0,  
              b_dt  = 0,
              b_CongAB_ruta1  = 0, b_CongCD_ruta1  = 0, b_CongEF_ruta1  = 0,
              b_CongAB_ruta2  = 0, b_CongCD_ruta2  = 0, b_CongEF_ruta2  = 0,
              b_CongAB_ruta3  = 0, b_CongCD_ruta3  = 0, b_CongEF_ruta3  = 0,
              b_CongAB_rutaEC  = 0, b_CongCD_rutaEC  = 0, b_CongEF_rutaEC  = 0,
              b_Sem_0_ruta1 = 0, b_Sem_0_ruta2 = 0, b_Sem_0_ruta3 = 0, b_Sem_0_rutaEC = 0,
              b_Sem_1_ruta1 = 0, b_Sem_1_ruta2 = 0, b_Sem_1_ruta3 = 0, b_Sem_1_rutaEC = 0, 
              b_Sem_2_ruta1 = 0, b_Sem_2_ruta2 = 0, b_Sem_2_ruta3 = 0, b_Sem_2_rutaEC = 0, 
              b_Sem_3_ruta1 = 0, b_Sem_3_ruta2 = 0, b_Sem_3_ruta3 = 0, b_Sem_3_rutaEC = 0,
              b_Sem_4_ruta1 = 0, b_Sem_4_ruta2 = 0, b_Sem_4_ruta3 = 0, b_Sem_4_rutaEC = 0,
              b_ACC_0_ruta1 = 0, b_ACC_0_ruta2 = 0, b_ACC_0_ruta3 = 0, b_ACC_0_rutaEC = 0,
              b_ACC_1_ruta1 = 0, b_ACC_1_ruta2 = 0, b_ACC_1_ruta3 = 0, b_ACC_1_rutaEC = 0, 
              b_ACC_2_ruta1 = 0, b_ACC_2_ruta2 = 0, b_ACC_2_ruta3 = 0, b_ACC_2_rutaEC = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, b_NO_PANEL = 0, b_SI_PANEL = 0, b_NO_ZER = 0,
              b_SI_ZER = 0, b_No_MTRP = 0, b_Si_MTRP = 0,
              b_CSECO     = 0,
              b_CLLUVIA   = 0,
              b_Hpico     = 0,
              b_Hvalle    = 0,
              b_Joven30 = 0,
              b_Adulto40 = 0,
              b_Adulto60 = 0,
              b_AdultoMayor = 0,
              b_USOCINTURON = 0,
              b_NOUSOCINTURON = 0,
              b_USODISPMOB = 0, 
              b_NOUSODISPMOB = 0,
              b_EdBasica = 0,
              b_EdSuperior = 0,
              b_Htrab_1 =0,
              b_Htrab_2 = 0,
              b_Htrab_3 = 0,
              b_Htrab_4 = 0,
              b_Exp_1 = 0,
              b_Exp_2 = 0,
              b_Exp_3 = 0,
              b_Exp_4 = 0,
              b_Exp_5 = 0)

apollo_fixed = c("asc_ruta1", "b_CongEF_ruta1", "b_CongEF_ruta2", "b_CongEF_ruta3", "b_CongEF_rutaEC",
                 "b_Sem_4_ruta1", "b_Sem_4_ruta2", "b_Sem_4_ruta3", "b_Sem_4_rutaEC", "b_ACC_2_ruta1",
                 "b_ACC_2_ruta2", "b_ACC_2_ruta3", "b_ACC_2_rutaEC", "b_NO_CAMFD", "b_NO_PANEL", "b_NO_ZER",
                 "b_No_MTRP","b_CLLUVIA","b_Hvalle","b_AdultoMayor","b_NOUSOCINTURON", "b_NOUSODISPMOB",
                 "b_EdSuperior", "b_Htrab_1", "b_Exp_1")

# ################################################################# #
#### ENTRADAS DE GRUPO Y VALIDACIÓN                                ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = asc_ruta1  + b_tt * TIEMPOAlt1 + b_dt * DISTAlt1 + b_CongAB*CONG_AB_A1 + 
    b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 + b_Sem_0_ruta1*SEMF_A1_0 + 
    b_Sem_1_ruta1*SEMF_A1_1 + b_Sem_2_ruta1*SEMF_A1_2 + b_Sem_3_ruta1*SEMF_A1_3 + 
    b_Sem_4_ruta1*SEMF_A1_4 +  b_ACC_0_ruta1*ACC_A1_0 + b_ACC_1_ruta1*ACC_A1_1 + 
    b_ACC_2_ruta1*ACC_A1_2 + b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
    b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
    b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + b_No_MTRP * NO_MTRP_A1 +
    b_Si_MTRP * SI_MTRP_A1
  
  V[['ruta2']]  = asc_ruta2  + b_tt * TIEMPOAlt2 + b_dt * DISTAlt2 + b_CongAB*CONG_AB_A2 + 
    b_CongCD*CONG_CD_A2 + b_CongEF*CONG_EF_A2 + b_Sem_0_ruta2*SEMF_A2_0 + 
    b_Sem_1_ruta2*SEMF_A2_1 + b_Sem_2_ruta2*SEMF_A2_2 + b_Sem_3_ruta2*SEMF_A2_3 + 
    b_Sem_4_ruta2*SEMF_A2_4 + b_ACC_0_ruta2*ACC_A2_0 + b_ACC_1_ruta2*ACC_A2_1 + 
    b_ACC_2_ruta2*ACC_A2_2 + b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
    b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
    b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + b_No_MTRP * NO_MTRP_A2 +
    b_Si_MTRP * SI_MTRP_A2
  
  V[['ruta3']]  = asc_ruta3  + b_tt * TIEMPOAlt3 + b_dt * DISTAlt3 + b_CongAB*CONG_AB_A3 + 
    b_CongCD*CONG_CD_A3 + b_CongEF*CONG_EF_A3 + b_Sem_0_ruta3*SEMF_A3_0 + 
    b_Sem_1_ruta3*SEMF_A3_1 + b_Sem_2_ruta3*SEMF_A3_2 + b_Sem_3_ruta3*SEMF_A3_3 + 
    b_Sem_4_ruta3*SEMF_A3_4 + b_ACC_0_ruta3*ACC_A3_0 + b_ACC_1_ruta3*ACC_A3_1 + 
    b_ACC_2_ruta3*ACC_A3_2 +  b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +
    b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 + 
    b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3 + b_No_MTRP * NO_MTRP_A3 +
    b_Si_MTRP * SI_MTRP_A3
  
  V[['rutaEC']] = asc_rutaEC + b_tt * TIEMPOEC   + b_dt * DISTEC + b_CongAB*CONG_AB_EC + 
    b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC + b_Sem_0_rutaEC*SEMF_EC_0 + 
    b_Sem_1_rutaEC*SEMF_EC_1 + b_Sem_2_rutaEC*SEMF_EC_2 + b_Sem_3_rutaEC*SEMF_EC_3 + 
    b_Sem_4_rutaEC*SEMF_EC_4 + b_ACC_0_rutaEC*ACC_EC_0 + b_ACC_1_rutaEC*ACC_EC_1 + 
    b_ACC_2_rutaEC*ACC_EC_2 + b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
    b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
    b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + b_No_MTRP * NO_MTRP_EC +
    b_Si_MTRP * SI_MTRP_EC + b_CSECO * CSECO + b_CLLUVIA * CLLUVIA + b_Hpico * HPICO + 
    b_Hvalle * HVALLE + b_Joven30 * JOVEN30 + b_Adulto40 * ADULTO40 + b_Adulto60 *ADULTO60 + 
    b_AdultoMayor * ADULTOMAYOR + b_USOCINTURON * USOCINTURON + b_NOUSOCINTURON * NOUSOCINTURON + 
    b_USODISPMOB * USODISPMOB + b_NOUSODISPMOB * NOUSODISPMOB + b_EdBasica * EDUBASICA +
    b_EdSuperior * EDUSUP + b_Htrab_1 * HTRB_1 + b_Htrab_2 * HTRB_2 + b_Htrab_3 * HTRB_3 +
    b_Htrab_4 * HTRB_4 + b_Exp_1 * EXP_1 + b_Exp_2 * EXP_2 + b_Exp_3 * EXP_3 +
    b_Exp_4 * EXP_4 + b_Exp_5 * EXP_5
    
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ruta1=1, ruta2=2, ruta3=3, rutaEC=4), 
    avail         = list(ruta1=1, ruta2=1, ruta3=1, rutaEC=1), 
    choiceVar     = CHOICE,
    V             = V
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
#### ESTIMACION DEL MODELO                                       ####
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


