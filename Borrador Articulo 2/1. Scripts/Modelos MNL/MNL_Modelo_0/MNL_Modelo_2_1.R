
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Elimino la ruta 3 debido a que no es significativa en el modelo

### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos MNL/MNL_Modelo_2"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "MNL_Modelo_2_1 Sin la ruta 3",
  modelDescr = "Modelo MNL Con Camaras, Paneles, MTRP y ZER ",
  indivID    = "ViajeId",
  nCores     = 1
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

table(database$CHOICE)

#database <-  database %>%
  # Filtrar viajes Eliminados en la primera revisión
#  filter(!(CHOICE %in% c("3")))

#Reemplazar 
database$CHOICE[database$CHOICE == 3]<-2

table(database$CHOICE)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_rutaEC  = 0,
              b_tt  = 0,  
              b_dt  = 0,
              b_CongAB  = 0, b_CongCD  = 0, b_CongEF  = 0,
              b_Sem_1 = 0, b_Sem_2 = 0, b_Sem_3 = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = 0, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_No_MTRP = 0, b_Si_MTRP = 0)


### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta2", "b_CongEF", "b_Sem_3", "b_ACC_2", "b_NO_CAMFD", "b_NO_PANEL", "b_NO_ZER",
                 "b_No_MTRP")

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
    b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 +
    b_Sem_1*SEMF_A1_1 + b_Sem_2*SEMF_A1_2 + b_Sem_3*SEMF_A1_3 + 
    b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
    b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
    b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
    b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + b_No_MTRP * NO_MTRP_A1 +
    b_Si_MTRP * SI_MTRP_A1
  
  V[['ruta2']]  = asc_ruta2  + b_tt * TIEMPOAlt2 + b_dt * DISTAlt2 + b_CongAB*CONG_AB_A2 + 
    b_CongCD*CONG_CD_A2 + b_CongEF*CONG_EF_A2 + 
    b_Sem_1*SEMF_A2_1 + b_Sem_2*SEMF_A2_2 + b_Sem_3*SEMF_A2_3 + 
    b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 + 
    b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
    b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
    b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + b_No_MTRP * NO_MTRP_A2 +
    b_Si_MTRP * SI_MTRP_A2
  
  V[['rutaEC']] = asc_rutaEC + b_tt * TIEMPOEC   + b_dt * DISTEC + b_CongAB*CONG_AB_EC + 
    b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +
    b_Sem_1*SEMF_EC_1 + b_Sem_2*SEMF_EC_2 + b_Sem_3*SEMF_EC_3 + 
    b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
    b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
    b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
    b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + b_No_MTRP * NO_MTRP_EC +
    b_Si_MTRP * SI_MTRP_EC
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ruta1=1, ruta2=2, rutaEC=4), 
    avail         = list(ruta1=1, ruta2=1, rutaEC=1), 
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

predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs)


