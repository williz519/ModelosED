
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
  modelName  = "ICLV_ModoCond",
  modelDescr = "ICLV modelo sobre datos de elección de ruta",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
names(database)
database$EXP_1 <- ifelse((database$Experiencia == 1),1,0)
database$EXP_2 <- ifelse((database$Experiencia == 2),1,0)
database$EXP_3 <- ifelse((database$Experiencia == 3),1,0)
database$EXP_4 <- ifelse((database$Experiencia == 4),1,0)
database$EXP_5 <- ifelse((database$Experiencia == 5),1,0)


database = subset(database, database$MODELO == 1)
dplyr::glimpse(database)  
summary(database)


# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_rutaEC  = 0,
              b_time      = 0, b_dist      = 0, b_cong      = 0, b_Sem_0     = 0,
              b_Sem_1     = 0, b_Sem_2     = 0, b_Sem_3     = 0, b_Sem_4     = 0,
              b_ACC_0     = 0, b_ACC_1     = 0, b_ACC_2     = 0, b_NO_CAMFD  = 0,
              b_SI_CAMFD  = 0, b_NO_PANEL  = 0, b_SI_PANEL  = 0, b_NO_ZER    = 0,
              b_SI_ZER    = 0,
              b_CSECO     = 0,
              b_CLLUVIA   = 0,
              b_Hpico     = 0,
              b_Hvalle    = 0,
              b_Joven30   = 0,
              b_Adulto40  = 0,
              b_Adulto60  = 0,
              b_AdultoMayor   = 0,
              b_USOCINTURON   = 0,
              b_NOUSOCINTURON = 0,
              b_USODISPMOB    = 0, 
              b_NOUSODISPMOB  = 0,
              b_HTRB_1      = 0,
              b_HTRB_2      = 0,
              b_HTRB_3      = 0,
              b_HTRB_4      = 0,
              b_EduBasica   = 0,
              b_EduSuperior = 0,
              b_Exper_1     = 0,
              b_Exper_2     = 0,
              b_Exper_3     = 0,
              b_Exper_4     = 0,
              b_Exper_5     = 0,
              b_Info_traf   = 0,
              b_NoInfo_traf = 0,
              lambda        = 1,
              gamma_EDUBASICA = 0,
              gamma_CSECO     = 0, 
              gamma_CONG_EF   = 0,
              gamma_SININFO   = 0,
              zeta_FRbr      = 1, 
              zeta_EnfCond   = 1, 
              zeta_AFrSem    = 1, 
              zeta_CulFr     = 1, 
              tau_FRbr_1     =-2, 
              tau_FRbr_2     =-1, 
              tau_FRbr_3     = 1, 
              tau_FRbr_4     = 2,
              tau_EnfCond_1   =-2, 
              tau_EnfCond_2   =-1, 
              tau_EnfCond_3   = 1, 
              tau_EnfCond_4   = 2, 
              tau_AFrSem_1    =-2, 
              tau_AFrSem_2    =-1, 
              tau_AFrSem_3    = 1, 
              tau_AFrSem_4    = 2, 
              tau_CulFr_1     =-2, 
              tau_CulFr_2     =-1, 
              tau_CulFr_3     = 1, 
              tau_CulFr_4     = 2
)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_rutaEC","b_CLLUVIA","b_Hpico","b_AdultoMayor","b_NOUSOCINTURON","b_NOUSODISPMOB",
                 "b_Sem_0","b_ACC_0","b_NO_CAMFD","b_NO_PANEL","b_NO_ZER","b_HTRB_4","b_EduBasica",
                 "b_Exper_1","b_NoInfo_traf")


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
  ol_settings1 = list(outcomeOrdered=FRbr, 
                      V=zeta_FRbr*LV_1, 
                      tau=c(tau_FRbr_1, tau_FRbr_2, tau_FRbr_3, tau_FRbr_4))
  ol_settings2 = list(outcomeOrdered=EnfCond, 
                      V=zeta_EnfCond*LV_1, 
                      tau=c(tau_EnfCond_1, tau_EnfCond_2, tau_EnfCond_3, tau_EnfCond_4))
  ol_settings3 = list(outcomeOrdered=AFrSem, 
                      V=zeta_AFrSem*LV_1, 
                      tau=c(tau_AFrSem_1, tau_AFrSem_2, tau_AFrSem_3, tau_AFrSem_4))
  ol_settings4 = list(outcomeOrdered=CulFr, 
                      V=zeta_CulFr*LV_1, 
                      tau=c(tau_CulFr_1, tau_CulFr_2, tau_CulFr_3, tau_CulFr_4))
  
  P[["indic_FRbr"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_EnfCond"]]  = apollo_ol(ol_settings2, functionality)
  P[["indic_AFrSem"]]   = apollo_ol(ol_settings3, functionality)
  P[["indic_CulFr"]]    = apollo_ol(ol_settings4, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_time * TIEMPOAlt1 + b_dist * DISTAlt1 + b_cong * CONG_A1 +
                     b_Sem_0 * SEMF_A1_0 + b_Sem_1 * SEMF_A1_1 + b_Sem_2 * SEMF_A1_2 + b_Sem_3 * SEMF_A1_3 + b_Sem_4 * SEMF_A1_4 +
                     b_ACC_0 * ACC_A1_0 + b_ACC_1 * ACC_A1_1 + b_ACC_2 * ACC_A1_2 +
                     b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
                     b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
                     b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1) 
  
  
  V[['ruta2']]  = (asc_ruta2  + b_time * TIEMPOAlt2 + b_dist * DISTAlt2 + b_cong * CONG_A2 +
                     b_Sem_0 * SEMF_A2_0 + b_Sem_1 * SEMF_A2_1 + b_Sem_2 * SEMF_A2_2 + b_Sem_3 * SEMF_A2_3 + b_Sem_4 * SEMF_A2_4 +
                     b_ACC_0 * ACC_A2_0 + b_ACC_1 * ACC_A2_1 + b_ACC_2 * ACC_A2_2 +
                     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
                     b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
                     b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2)
  
  
  V[['ruta3']]  = (asc_ruta3  + b_time * TIEMPOAlt3 + b_dist * DISTAlt3 + b_cong * CONG_A3 +
                     b_Sem_0 * SEMF_A3_0 + b_Sem_1 * SEMF_A3_1 + b_Sem_2 * SEMF_A3_2 + b_Sem_3 * SEMF_A3_3 + b_Sem_4 * SEMF_A3_4 +
                     b_ACC_0 * ACC_A3_0 + b_ACC_1 * ACC_A3_1 + b_ACC_2 * ACC_A3_2 +
                     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +
                     b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 + 
                     b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3)
  
  
  V[['rutaEC']] = (asc_rutaEC + b_time * TIEMPOEC   + b_dist * DISTEC   + b_cong * CONGESTION +
                     b_Sem_0 * SEMF_EC_0 + b_Sem_1 * SEMF_EC_1 + b_Sem_2 * SEMF_EC_2 + b_Sem_3 * SEMF_EC_3 + b_Sem_4 * SEMF_EC_4 +
                     b_ACC_0 * ACC_EC_0 + b_ACC_1 * ACC_EC_1 + b_ACC_2 * ACC_EC_2 + 
                     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
                     b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
                     b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC +
                     b_CSECO * CSECO + b_CLLUVIA * CLLUVIA + b_Hpico * HPICO + b_Hvalle * HVALLE +
                     b_Joven30 * JOVEN30 + b_Adulto40 * ADULTO40 + b_Adulto60 *ADULTO60 + b_AdultoMayor * ADULTOMAYOR +
                     b_USOCINTURON * USOCINTURON + b_NOUSOCINTURON * NOUSOCINTURON + b_USODISPMOB * USODISPMOB + 
                     b_NOUSODISPMOB * NOUSODISPMOB +
                     b_HTRB_1 * HTRB_1 + b_HTRB_2 * HTRB_2 + b_HTRB_3 * HTRB_3 + b_HTRB_4 * HTRB_4 +
                     b_EduBasica * EDUBASICA + b_EduSuperior * EDUSUP + 
                     b_Exper_1 * (Experiencia == 1) +   b_Exper_2 * (Experiencia == 2) +   b_Exper_3 * (Experiencia == 3) + 
                     b_Exper_4 * (Experiencia == 4) +   b_Exper_5 * (Experiencia == 5) +
                     b_Info_traf *(INFOTRAFICO == 2) + b_NoInfo_traf * (INFOTRAFICO == 1) +
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
#model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(maxIterations = 400))

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

