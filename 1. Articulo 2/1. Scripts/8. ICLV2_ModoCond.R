
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
  modelName  = "ICLV2_ModoCond",
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
              b_usodisp = 0,
              b_Joven30 = 0,
              b_Adulto40 = 0,
              b_Adulto60 = 0,
              b_AdultoMayor = 0,
              b_USOCINTURON = 0,
              b_NOUSOCINTURON = 0,
              b_USODISPMOB = 0,
              b_NOUSODISPMOB = 0,
              gamma_EDUBASICA = 0.158,
              gamma_CSECO = 0.144, 
              gamma_CONG_EF = 0.205,
              gamma_SININFO = -0.233,
              gamma_EDUBASICA = 0.158, gamma_CSECO = 0.144, gamma_CONG_EF = 0.205,
              gamma_SININFO = -0.233, gamma_HORAS_TRABAJO = 0, gamma_HPICO = 0, gamma_CONG_CD = 0,
              gamma_USOCINTURON = 0, gamma_USODISPMOB =0,
              zeta_FRbr     = 1, 
              zeta_EnfCond  = 1, 
              zeta_AFrSem   = 1, 
              zeta_CulFr    = 1, 
              zeta_OmLmVel  = 1, 
              zeta_IgPare    = 1, 
              zeta_UsoCel    = 1,
              lambda1        = 1,
              lambda2       = 1,
              tau_FRbr_1      =-2, 
              tau_FRbr_2      =-1, 
              tau_FRbr_3      = 1, 
              tau_FRbr_4      = 2,
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
              tau_CulFr_4     = 2,
              tau_OmLmVel_1     =-2, 
              tau_OmLmVel_2     =-1, 
              tau_OmLmVel_3     = 1, 
              tau_OmLmVel_4     = 2,
              tau_IgPare_1     =-2, 
              tau_IgPare_2     =-1, 
              tau_IgPare_3     = 1, 
              tau_IgPare_4     = 2,
              tau_UsoCel_1     =-2, 
              tau_UsoCel_2     =-1, 
              tau_UsoCel_3     = 1, 
              tau_UsoCel_4     = 2
)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta1", "b_AdultoMayor", "b_EduSuperior", "b_Hpico", "b_NOUSOCINTURON",
                 "b_NOUSODISPMOB")

### Lea los valores iniciales para al menos algunos parámetros del archivo de salida del modelo existente
#apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "ICLV_ModoCond", overwriteFixed=FALSE)

# ################################################################# #
#### DEFINE COMPONENTES ALEATORIOS                              ####
# ################################################################# #

### Establecer parámetros para generar sorteos
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=100,          
  interUnifDraws=c(),      
  interNormDraws=c("eta1","eta2")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_1"]] = gamma_EDUBASICA * EDUBASICA + gamma_CSECO * CSECO + 
    gamma_CONG_EF * CONG_EF + gamma_SININFO * SININFOTRF + eta1
  
  randcoeff[["LV_2"]] = gamma_HORAS_TRABAJO * HORAS_TRABAJO + gamma_HPICO * HPICO + 
    gamma_CONG_CD * CONG_CD + gamma_CONG_EF * CONG_EF + gamma_USOCINTURON * USOCINTURON + 
    gamma_USODISPMOB * USODISPMOB + eta2
  
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
  
  ol_settings5 = list(outcomeOrdered=OmLmVel, 
                      V=zeta_OmLmVel*LV_2, 
                      tau=c(tau_OmLmVel_1, tau_OmLmVel_2, tau_OmLmVel_3, tau_OmLmVel_4))
  ol_settings6 = list(outcomeOrdered=IgPare, 
                      V=zeta_IgPare*LV_2, 
                      tau=c(tau_IgPare_1, tau_IgPare_2, tau_IgPare_3, tau_IgPare_4))
  ol_settings7 = list(outcomeOrdered=UsoCel, 
                      V=zeta_UsoCel*LV_2, 
                      tau=c(tau_UsoCel_1, tau_UsoCel_2,tau_UsoCel_3,tau_UsoCel_4))
  
  
  P[["indic_FRbr"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_EnfCond"]]  = apollo_ol(ol_settings2, functionality)
  P[["indic_AFrSem"]]   = apollo_ol(ol_settings3, functionality)
  P[["indic_CulFr"]]    = apollo_ol(ol_settings4, functionality)
  P[["indic_OmLmVel"]]  = apollo_ol(ol_settings5, functionality)
  P[["indic_IgPare"]]   = apollo_ol(ol_settings6, functionality)
  P[["indic_UsoCel"]]   = apollo_ol(ol_settings7, functionality)
  

  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_time * TIEMPOAlt1 + b_dist * DISTAlt1 + b_cong * CONG_A1 +
                     b_semaf * Semaf_A1 + b_CamFD * CamFD_A1 + b_ZER * ZER_A1 + b_MtrP * MtrP_A1 + 
                     b_Acc * Acc_rutas_1 + b_Panel * Paneles_rutas_1)
  
  V[['ruta2']]  = (asc_ruta2  + b_time * TIEMPOAlt2 + b_dist * DISTAlt2 + b_cong * CONG_A2 +
                     b_semaf * Semaf_A2 + b_CamFD * CamFD_A2 + b_ZER * ZER_A2 + b_MtrP * MtrP_A2 + 
                     b_Acc * Acc_rutas_2 + b_Panel * Paneles_rutas_2)
  
  V[['ruta3']]  = (asc_ruta3  + b_time * TIEMPOAlt3 + b_dist * DISTAlt3 + b_cong * CONG_A3 +
                     b_semaf * Semaf_A3 + b_CamFD * CamFD_A3 + b_ZER * ZER_A3 + b_MtrP * MtrP_A3 + 
                     b_Acc * Acc_rutas_3 + b_Panel * Paneles_rutas_3)
  
  V[['rutaEC']] = (asc_rutaEC + b_time * TIEMPOEC   + b_dist * DISTEC   + b_cong * CONGESTION +
                     b_semaf * Semaf_EC + b_CamFD * CamFD_EC + b_ZER * ZER_EC + b_MtrP * MtrP_EC + 
                     b_Acc * Acc_EC + b_Panel * Paneles_EC +
                     b_clima * CLIMA + b_Hpico * HPICO + b_Hvalle * HVALLE + b_T_prof * TIEMPO_PROFESION + 
                     b_Hor_trab * HORAS_TRABAJO + b_EduBasica * EDUBASICA + b_EduSuperior * EDUSUP +
                     b_Info_traf *(INFOTRAFICO == 2) + b_Joven30 * JOVEN30 + b_Adulto40 * ADULTO40 + 
                     b_Adulto60 *ADULTO60 + b_AdultoMayor * ADULTOMAYOR + b_USOCINTURON * USOCINTURON +
                     b_NOUSOCINTURON * NOUSOCINTURON + b_USODISPMOB * USODISPMOB + b_NOUSODISPMOB * NOUSODISPMOB +
                     lambda1 * LV_1 + lambda2 * LV_2)
  
  
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
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(maxIterations = 700))

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

