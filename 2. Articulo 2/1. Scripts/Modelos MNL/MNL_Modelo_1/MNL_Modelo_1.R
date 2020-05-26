
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/1. Articulo 2/1. Scripts/Modelos MNL/MNL_Modelo_1"
setwd(workingDirectory)

###Cargar la libreria knitr
library(knitr)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "MNL_Modelo_1",
  modelDescr = "Modelo MNL Simple",
  indivID    = "ViajeId",
  nCores     = 1
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/1. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)



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
              b_Sem_4_ruta1 = 0, b_Sem_4_ruta2 = 0, b_Sem_4_ruta3 = 0, b_Sem_4_rutaEC = 0)
              


### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta3", 
                 "b_CongEF_ruta1", "b_CongEF_ruta2", "b_CongEF_ruta3", "b_CongEF_rutaEC",
                 "b_Sem_4_ruta1", "b_Sem_4_ruta2", "b_Sem_4_ruta3", "b_Sem_4_rutaEC")

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
  
  V[['ruta1']]  = asc_ruta1  + b_tt * TIEMPOAlt1 + b_dt * DISTAlt1 + b_CongAB_ruta1*CONG_AB_A1 + 
    b_CongCD_ruta1*CONG_CD_A1 + b_CongEF_ruta1*CONG_EF_A1 + b_Sem_0_ruta1*SEMF_A1_0 + 
    b_Sem_1_ruta1*SEMF_A1_1 + b_Sem_2_ruta1*SEMF_A1_2 + b_Sem_3_ruta1*SEMF_A1_3 + 
    b_Sem_4_ruta1*SEMF_A1_4 
  
  V[['ruta2']]  = asc_ruta2  + b_tt * TIEMPOAlt2 + b_dt * DISTAlt2 + b_CongAB_ruta2*CONG_AB_A2 + 
    b_CongCD_ruta2*CONG_CD_A2 + b_CongEF_ruta2*CONG_EF_A2 + b_Sem_0_ruta2*SEMF_A2_0 + 
    b_Sem_1_ruta2*SEMF_A2_1 + b_Sem_2_ruta2*SEMF_A2_2 + b_Sem_3_ruta2*SEMF_A2_3 + 
    b_Sem_4_ruta2*SEMF_A2_4 
  
  V[['ruta3']]  = asc_ruta3  + b_tt * TIEMPOAlt3 + b_dt * DISTAlt3 + b_CongAB_ruta3*CONG_AB_A3 + 
    b_CongCD_ruta3*CONG_CD_A3 + b_CongEF_ruta3*CONG_EF_A3 + b_Sem_0_ruta3*SEMF_A3_0 + 
    b_Sem_1_ruta3*SEMF_A3_1 + b_Sem_2_ruta3*SEMF_A3_2 + b_Sem_3_ruta3*SEMF_A3_3 + 
    b_Sem_4_ruta3*SEMF_A3_4 
  
  V[['rutaEC']] = asc_rutaEC + b_tt * TIEMPOEC   + b_dt * DISTEC + b_CongAB_rutaEC*CONG_AB_EC + 
    b_CongCD_rutaEC*CONG_CD_EC + b_CongEF_rutaEC*CONG_EF_EC + b_Sem_0_rutaEC*SEMF_EC_0 + 
    b_Sem_1_rutaEC*SEMF_EC_1 + b_Sem_2_rutaEC*SEMF_EC_2 + b_Sem_3_rutaEC*SEMF_EC_3 + 
    b_Sem_4_rutaEC*SEMF_EC_4 
 
     
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

apollo_saveOutput(model, saveOutput_settings=list(printPVal=TRUE))



