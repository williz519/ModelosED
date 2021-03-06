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
  modelName  = "ICLV2 Modelo Continuo",
  modelDescr = "ICLV modelo sobre datos de elección de ruta, utilizando el modelo de medición ordenado para indicadores",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
names(database)

### Subtract mean of indicator variables to centre them on zero
database$FRbr=database$FRbr-mean(database$FRbr)
database$EnfCond=database$EnfCond-mean(database$EnfCond)
database$AFrSem=database$AFrSem-mean(database$AFrSem)
database$CulFr=database$CulFr-mean(database$CulFr)
#database$IgPare=database$IgPare-mean(database$IgPare)
#database$UsoPito=database$UsoPito-mean(database$UsoPito)
#database$UsoCel=database$UsoCel-mean(database$UsoCel)
#database$UsoDirec=database$UsoDirec-mean(database$UsoDirec)


# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1     = 0,
              asc_ruta2     = 0,
              asc_ruta3     = 0,
              asc_rutaEC    = 0,
              b_time        = 0,
              b_dist        = 0,
              b_clima       = 0,
              b_cong        = 0,
              b_Hpico       = 0,
              b_Hvalle      = 0,
              b_T_prof      = 0,
              b_Hor_trab    = 0,
              b_EduBasica   = 0,
              b_EduSuperior = 0,
              b_Info_traf   = 0,
              b_Joven30     = 0,
              b_Adulto40    = 0,
              b_Adulto60    = 0,
              b_AdultoMayor = 0,
              b_USOCINTURON = 0,
              b_NOUSOCINTURON = 0,
              b_USODISPMOB = 0,
              b_NOUSODISPMOB = 0,
              lambda        = 1,
              gamma_EDUBASICA = 0.158,
              gamma_CSECO = 0.144, 
              gamma_CONG_EF = 0.205,
              gamma_SININFO = -0.233,
              zeta_FRbr     = 1, 
              zeta_EnfCond  = 1, 
              zeta_AFrSem   = 1, 
              zeta_CulFr    = 1, 
              sigma_FRbr    = 1, 
              sigma_EnfCond  = 1, 
              sigma_AFrSem   = 1, 
              sigma_CulFr    = 1
)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta1", "b_AdultoMayor", "b_EduSuperior", "b_Hpico", "b_NOUSOCINTURON",
                 "b_NOUSODISPMOB")


# ################################################################# #
#### DEFINE COMPONENTES ALEATORIOS                              ####
# ################################################################# #

### Establecer parámetros para generar sorteos
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=100,          
  interUnifDraws=c(),      
  interNormDraws=c("eta")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_1"]] = gamma_EDUBASICA * EDUBASICA + gamma_CSECO * CSECO + 
    gamma_CONG_EF * CONG_EF + gamma_SININFO * SININFOTRF + eta
  
  return(randcoeff)
}

# ################################################################# #
#### ENTRADAS DE GRUPO Y VALIDACIÓN                              ####
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
  
  ### Likelihood of indicators
  normalDensity_settings1 = list(outcomeNormal=FRbr, 
                                 xNormal=zeta_FRbr*LV_1, 
                                 mu=0, 
                                 sigma=sigma_FRbr)
  
  normalDensity_settings2 = list(outcomeNormal=EnfCond, 
                                 xNormal=zeta_EnfCond*LV_1, 
                                 mu=0, 
                                 sigma=sigma_EnfCond) 
  
  normalDensity_settings3 = list(outcomeNormal=AFrSem, 
                                 xNormal=zeta_AFrSem*LV_1, 
                                 mu=0, 
                                 sigma=sigma_AFrSem)
  
  normalDensity_settings4 = list(outcomeNormal=CulFr, 
                                 xNormal=zeta_CulFr*LV_1, 
                                 mu=0, 
                                 sigma=sigma_CulFr)
  
  
  
  P[["indic_FRbr"]]     = apollo_normalDensity(normalDensity_settings1, functionality)
  P[["indic_EnfCond"]]  = apollo_normalDensity(normalDensity_settings2, functionality)
  P[["indic_AFrSem"]]   = apollo_normalDensity(normalDensity_settings3, functionality)
  P[["indic_CulFr"]]    = apollo_normalDensity(normalDensity_settings4, functionality)
  
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_time * TIEMPOAlt1 + b_dist * DISTAlt1 + b_cong * CONG_A1)
  
  
  V[['ruta2']]  = (asc_ruta2  + b_time * TIEMPOAlt2 + b_dist * DISTAlt2 + b_cong * CONG_A2)
  
  V[['ruta3']]  = (asc_ruta3  + b_time * TIEMPOAlt3 + b_dist * DISTAlt3 + b_cong * CONG_A3)
  
  V[['rutaEC']] = (asc_rutaEC + b_time * TIEMPOEC   + b_dist * DISTEC   + b_cong * CONGESTION +
                     b_clima * CLIMA + b_Hpico * HPICO + b_Hvalle * HVALLE + b_T_prof * TIEMPO_PROFESION + 
                     b_Hor_trab * HORAS_TRABAJO + b_EduBasica * EDUBASICA + b_EduSuperior * EDUSUP +
                     b_Info_traf *(INFOTRAFICO == 2) + b_Joven30 * JOVEN30 + b_Adulto40 * ADULTO40 + 
                     b_Adulto60 *ADULTO60 + b_AdultoMayor * ADULTOMAYOR + b_USOCINTURON * USOCINTURON +
                     b_NOUSOCINTURON * NOUSOCINTURON + b_USODISPMOB * USODISPMOB + b_NOUSODISPMOB * NOUSODISPMOB +
                     lambda * LV_1)
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ruta1=1, ruta2=2, ruta3=3, rutaEC=4), 
    avail         = list(ruta1=1, ruta2=1, ruta3=1, rutaEC=1), 
    choiceVar     = CHOICE,
    V             = V
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional: calculate LL before model estimation
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
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

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="indic_FRbr")

# ----------------------------------------------------------------- #
#---- CONDITIONALS AND UNCONDITIONALS                            ----
# ----------------------------------------------------------------- #

conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)

unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()
