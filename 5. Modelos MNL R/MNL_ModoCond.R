
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

### Limpiar memoria
rm(list = ls())

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "MNL_ModoCond",
  modelDescr = "MNL model",
  indivID    = "ViajeId",
  nCores     = 1
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
names(database)


# ################################################################# #
#### ANALISIS DE ELECCIONES                                     ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(ruta1=1, ruta2=2, ruta3=3, rutaEC=4),
  avail        = list(ruta1=1, ruta2=1, ruta3=1, rutaEC=1),
  choiceVar    = database$CHOICE,
  explanators = database[,c("NIVEL_EDUCATIVO")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0,
              asc_ruta2   = 0,
              asc_ruta3   = 0,
              asc_rutaEC  = 0,
              b_time      = 0,
              b_dist      = 0,
              b_semaf     = 0,
              b_CamFD     = 0,
              b_ZER       = 0,
              b_MtrP      = 0,
              b_Acc       = 0,
              b_Panel     = 0,
              b_clima     = 0,
              b_cong      = 0,
              b_Hpico     = 0,
              b_Hvalle    = 0,
              b_T_prof    = 0,
              b_Hor_trab  = 0,
              b_EduBasica = 0,
              b_EduSuperior = 0,
              b_Info_traf =0,
              b_Joven30 = 0,
              b_Adulto40 = 0,
              b_Adulto60 = 0,
              b_AdultoMayor = 0
              )

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta1", "b_AdultoMayor", "b_EduSuperior", "b_Hpico")

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
  V[['ruta1']]  = asc_ruta1  + b_time * TIEMPOAlt1 + b_dist * DISTAlt1 + b_cong * CONG_A1 +
    b_semaf * Semaf_A1 + b_CamFD * CamFD_A1 + b_ZER * ZER_A1 + b_MtrP * MtrP_A1 + 
    b_Acc * Acc_rutas_1 + b_Panel * Paneles_rutas_1
  
  V[['ruta2']]  = asc_ruta2  + b_time * TIEMPOAlt2 + b_dist * DISTAlt2 + b_cong * CONG_A2 +
    b_semaf * Semaf_A2 + b_CamFD * CamFD_A2 + b_ZER * ZER_A2 + b_MtrP * MtrP_A2 + 
    b_Acc * Acc_rutas_2 + b_Panel * Paneles_rutas_2
  
  V[['ruta3']]  = asc_ruta3  + b_time * TIEMPOAlt3 + b_dist * DISTAlt3 + b_cong * CONG_A3 +
    b_semaf * Semaf_A3 + b_CamFD * CamFD_A3 + b_ZER * ZER_A3 + b_MtrP * MtrP_A3 + 
    b_Acc * Acc_rutas_3 + b_Panel * Paneles_rutas_3
  
  V[['rutaEC']] = asc_rutaEC + b_time * TIEMPOEC   + b_dist * DISTEC   + b_cong * CONGESTION +
    b_semaf * Semaf_EC + b_CamFD * CamFD_EC + b_ZER * ZER_EC + b_MtrP * MtrP_EC + 
    b_Acc * Acc_EC + b_Panel * Paneles_EC +
    b_clima * CLIMA + b_Hpico * HPICO + b_Hvalle * HVALLE + b_T_prof * TIEMPO_PROFESION + 
    b_Hor_trab * HORAS_TRABAJO + b_EduBasica * EDUBASICA + b_EduSuperior * EDUSUP +
    b_Info_traf *(INFOTRAFICO == 2) + b_Joven30 * JOVEN30 + b_Adulto40 * ADULTO40 + 
    b_Adulto60 *ADULTO60 + b_AdultoMayor * ADULTOMAYOR 
  
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

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
