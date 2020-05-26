# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV Modelo Continuo",
  modelDescr = "ICLV modelo sobre datos de elección de ruta, utilizando el modelo de medición ordenado para indicadores",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
names(database)



# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1 = 0, asc_ruta2 = 0, asc_ruta3 = 0, asc_rutaEC = 0,
              b_tt = 0, b_dt = 0, 
              b_CongAB  = 0, b_CongCD  = 0, b_CongEF  = 0,
              b_Sem_1 = 0, b_Sem_2 = 0, b_Sem_3 = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = 0, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_No_MTRP = 0, b_Si_MTRP = 0, 
              lambda1        = 1,
              lambda2        = 1, 
              lambda3       = 1,
              gamma_USOCINTURON = 0,
              gamma_USODISPMOB =0,
              gamma_ADULTO40 = 0, 
              gamma_EXP_1 = 0, 
              gamma_HPICO = 0,
              gamma_LV1 = 0,
              gamma_EDUBASICA = 0,
              gamma_JOVEN30 = 0,  
              gamma_EXP_2 = 0, gamma_HTRB_2 = 0, gamma_HTRB_3 = 0, gamma_HTRB_4 = 0, gamma_LV2 = 0,
              zeta_FRbr     = 1, zeta_EnfCond  = 1, zeta_AFrSem   = 1, zeta_CulFr    = 1, zeta_OmLmVel  = 1, 
              zeta_IgPare    = 1,  zeta_UsoCel    = 1,  zeta_PasoPeaton = 1, 
              zeta_UsoDirec = 1, zeta_UsoPito = 1, 
              sigma_FRbr    = 1, sigma_EnfCond  = 1, sigma_AFrSem   = 1, sigma_CulFr    = 1,
              sigma_OmLmVel  = 1, sigma_IgPare    = 1,  sigma_UsoCel = 1,  sigma_PasoPeaton = 1, 
              sigma_UsoDirec = 1, sigma_UsoPito = 1 
)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta3", "b_CongEF", "b_Sem_3", "b_ACC_2", "b_NO_CAMFD", "b_NO_PANEL", "b_NO_ZER",
                 "b_No_MTRP")



# ################################################################# #
#### DEFINE COMPONENTES ALEATORIOS                              ####
# ################################################################# #

### Establecer parámetros para generar sorteos
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=100,          
  interUnifDraws=c(),      
  interNormDraws=c("eta1","eta2","eta3")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_1"]] = gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB + eta1
  
  randcoeff[["LV_2"]] = gamma_ADULTO40*ADULTO40 + gamma_EXP_1*EXP_1 + gamma_HPICO*HPICO + 
    gamma_USOCINTURON*USOCINTURON  + gamma_LV1*randcoeff[["LV_1"]] + eta2
  
  randcoeff[["LV_3"]] = gamma_EDUBASICA * EDUBASICA + gamma_JOVEN30*JOVEN30 +  gamma_ADULTO40*ADULTO40+
    gamma_EXP_2*EXP_2 + gamma_HTRB_2*HTRB_2 + gamma_HTRB_3*HTRB_3 + gamma_HTRB_4*HTRB_4 + 
    gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB + gamma_LV2*randcoeff[["LV_2"]] + eta3
  
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
  
  # LV1
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
  
  normalDensity_settings5 = list(outcomeNormal=UsoPito, 
                                 xNormal=zeta_UsoPito*LV_1, 
                                 mu=0, 
                                 sigma=sigma_UsoPito)
  
  #LV2
  normalDensity_settings6 = list(outcomeNormal=OmLmVel, 
                                 xNormal=zeta_OmLmVel*LV_2, 
                                 mu=0, 
                                 sigma=sigma_OmLmVel)
  
  normalDensity_settings7 = list(outcomeNormal=IgPare, 
                                 xNormal=zeta_IgPare*LV_2, 
                                 mu=0, 
                                 sigma=sigma_IgPare)
  
  normalDensity_settings8 = list(outcomeNormal=UsoCel, 
                                 xNormal=zeta_UsoCel*LV_2, 
                                 mu=0, 
                                 sigma=sigma_UsoCel)
  
  #LV3
  normalDensity_settings9 = list(outcomeNormal=PasoPeaton, 
                                 xNormal=zeta_PasoPeaton*LV_3, 
                                 mu=0, 
                                 sigma=sigma_PasoPeaton)
  
  normalDensity_settings10 = list(outcomeNormal=UsoDirec, 
                                 xNormal=zeta_UsoDirec*LV_3, 
                                 mu=0, 
                                 sigma=sigma_UsoDirec)
  
  normalDensity_settings11 = list(outcomeNormal=UsoCel, 
                                 xNormal=zeta_UsoCel*LV_3, 
                                 mu=0, 
                                 sigma=sigma_UsoCel)
                                
  
  
  P[["indic_FRbr"]]     = apollo_normalDensity(normalDensity_settings1, functionality)
  P[["indic_EnfCond"]]  = apollo_normalDensity(normalDensity_settings2, functionality)
  P[["indic_AFrSem"]]   = apollo_normalDensity(normalDensity_settings3, functionality)
  P[["indic_CulFr"]]    = apollo_normalDensity(normalDensity_settings4, functionality)
  P[["indic_UsoPito"]]  = apollo_normalDensity(normalDensity_settings5, functionality)
  P[["indic_OmLmVel"]]  = apollo_normalDensity(normalDensity_settings6, functionality)
  P[["indic_IgPare"]]   = apollo_normalDensity(normalDensity_settings7, functionality)
  P[["indic_UsoCel"]]   = apollo_normalDensity(normalDensity_settings8, functionality)
  P[["indic_PasoPeaton"]]  = apollo_normalDensity(normalDensity_settings9, functionality)
  P[["indic_UsoDirec"]]   = apollo_normalDensity(normalDensity_settings10, functionality)
  P[["indic_UsoCel"]]   = apollo_normalDensity(normalDensity_settings11, functionality)
  
  
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_tt * TIEMPOAlt1 + b_dt * DISTAlt1 + b_CongAB*CONG_AB_A1 + 
                     b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 +
                     b_Sem_1*SEMF_A1_1 + b_Sem_2*SEMF_A1_2 + b_Sem_3*SEMF_A1_3 + 
                     b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
                     b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
                     b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
                     b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + b_No_MTRP * NO_MTRP_A1 +
                     b_Si_MTRP * SI_MTRP_A1)
  
  
  V[['ruta2']]  = (asc_ruta2  + b_tt * TIEMPOAlt2 + b_dt * DISTAlt2 + b_CongAB*CONG_AB_A2 + 
                     b_CongCD*CONG_CD_A2 + b_CongEF*CONG_EF_A2 + 
                     b_Sem_1*SEMF_A2_1 + b_Sem_2*SEMF_A2_2 + b_Sem_3*SEMF_A2_3 + 
                     b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 + 
                     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
                     b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
                     b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + b_No_MTRP * NO_MTRP_A2 +
                     b_Si_MTRP * SI_MTRP_A2)
  
  V[['ruta3']]  = (asc_ruta3  + b_tt * TIEMPOAlt3 + b_dt * DISTAlt3 + b_CongAB*CONG_AB_A3 + 
                     b_CongCD*CONG_CD_A3 + b_CongEF*CONG_EF_A3 +  
                     b_Sem_1*SEMF_A3_1 + b_Sem_2*SEMF_A3_2 + b_Sem_3*SEMF_A3_3 + 
                     b_ACC_0*ACC_A3_0 + b_ACC_1*ACC_A3_1 + b_ACC_2*ACC_A3_2 +  
                     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +
                     b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 + 
                     b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3 + b_No_MTRP * NO_MTRP_A3 +
                     b_Si_MTRP * SI_MTRP_A3)
  
  V[['rutaEC']] = (asc_rutaEC + b_tt * TIEMPOEC   + b_dt * DISTEC + b_CongAB*CONG_AB_EC + 
                     b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +
                     b_Sem_1*SEMF_EC_1 + b_Sem_2*SEMF_EC_2 + b_Sem_3*SEMF_EC_3 + 
                     b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
                     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
                     b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
                     b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + b_No_MTRP * NO_MTRP_EC +
                     b_Si_MTRP * SI_MTRP_EC +
                     lambda1 * LV_1 + lambda2 * LV_2 + lambda3 * LV_3)
  
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

apollo_modelOutput(model, modelOutput_settings=list(printPVal=TRUE) )


# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, saveOutput_settings=list(printPVal=TRUE) )

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
