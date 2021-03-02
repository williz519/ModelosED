
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Modelo sin la ruta 3
### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/ICVL_MCond_2F"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV_MCond_2F ",
  modelDescr = "ICLV Modo Conduccion de 2F",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

# Normalización de las variables tiempo

# Normalización de los viajes

for (i in 1:nrow(database)){
  # Normalización de las variables tiempo
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  
  database$CG_Alt_1[i] = (database$DISTAlt1[i]/(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-4.25))/(database$DISTAlt1[i]/database$TIEMPOAlt1[i])
  database$CG_Alt_2[i] = (database$DISTAlt2[i]/(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-4.25))/(database$DISTAlt2[i]/database$TIEMPOAlt2[i])
  database$CG_Alt_3[i] = (database$DISTAlt3[i]/(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-4.25))/(database$DISTAlt3[i]/database$TIEMPOAlt3[i])
  database$CG_Alt_4[i] = (database$DISTEC[i]/(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-4.25))/(database$DISTEC[i]/database$TIEMPOEC[i])
}

database$UsoCel_Poco <- (ifelse((database$UsoCel == 1), 1,0))
#database$UsoCel_AlgVec <- (ifelse((database$UsoCel == 3 ), 1,0))
database$UsoCel_Frec <- (ifelse((database$UsoCel > 1 ), 1,0))




# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_ruta4  = 0,
              b_tt  = 0,  
              b_dt  = 0,
              b_Cong  = 0,
              b_Sem = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = 0, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_No_MTRP = 0, b_Si_MTRP = 0,
              b_No_Info = 0, b_Si_Info = 0,
              b_UsoCel_P = 0, #b_UsoCel_A = 0, 
              b_UsoCel_F = 0,
              lambda1        = 1,
              lambda2        = 1, 
              
              #LV_1
              gamma_EXP_3_LV1 = -0.326,
              gamma_HTRB_2_LV1 = 0.337,
              gamma_USODISPMOB_LV1 = 0.211,

              
              #LV_2
              gamma_EDUBASICA_LV2 = -0.399,
              gamma_JOVEN30_LV2 = -0.409,
              gamma_ADULTO40_LV2 = 0.339,
              gamma_EXP_2_LV2 = 0.339,
              gamma_USODISPMOB_LV2 = -0.290,
              
              
              zeta_FRbr     = 1, zeta_EnfCond  = 1, zeta_AFrSem   = 1, zeta_CulFr    = 1, zeta_OmLmVel  = 1, 
              zeta_IgPare    = 1,  #zeta_UsoCel    = 1,  
              zeta_PasoPeaton = 1, 
              zeta_UsoDirec = 1, zeta_UsoPito = 1,
              tau_FRbr_1      =-2, tau_FRbr_2      =-1, tau_FRbr_3      = 1, tau_FRbr_4      = 2,
              tau_EnfCond_1   =-2, tau_EnfCond_2   =-1, tau_EnfCond_3   = 1, tau_EnfCond_4   = 2, 
              tau_AFrSem_1    =-2, tau_AFrSem_2    =-1, tau_AFrSem_3    = 1, tau_AFrSem_4    = 2, 
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
              #tau_UsoCel_1     =-2, 
              #tau_UsoCel_2     =-1, 
              #tau_UsoCel_3     = 1, 
              #tau_UsoCel_4     = 2,
              tau_PasoPeaton_1     =-2, 
              tau_PasoPeaton_2    =-1, 
              tau_PasoPeaton_3    = 1, 
              tau_PasoPeaton_4    = 2,
              tau_UsoDirec_1     =-2, 
              tau_UsoDirec_2    =-1, 
              tau_UsoDirec_3    = 1, 
              tau_UsoDirec_4    = 2,
              tau_UsoPito_1 = -2, 
              tau_UsoPito_2 = -1, 
              tau_UsoPito_3 = 1, 
              tau_UsoPito_4 = 2)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta3", "b_ACC_0", "b_NO_CAMFD", "b_No_MTRP", "b_NO_PANEL", "b_NO_ZER", 
                 "b_Si_Info", "b_UsoCel_P")

### Lea los valores iniciales para al menos algunos parámetros del archivo de salida del modelo existente
#apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "ICLV2_ModoCond", overwriteFixed=FALSE)

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
  
  randcoeff[["LV_1"]] = gamma_EXP_3_LV1*EXP_3 + gamma_HTRB_2_LV1*HTRB_2 + gamma_USODISPMOB_LV1 * USODISPMOB + eta1
  
  randcoeff[["LV_2"]] = gamma_EDUBASICA_LV2*EDUBASICA + gamma_JOVEN30_LV2*JOVEN30 + gamma_ADULTO40_LV2*ADULTO40 +
    gamma_EXP_2_LV2*EXP_2 + gamma_USODISPMOB_LV2 * USODISPMOB + eta2
    

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
  ol_settings2 = list(outcomeOrdered=AFrSem, 
                      V=zeta_AFrSem*LV_1, 
                      tau=c(tau_AFrSem_1, tau_AFrSem_2, tau_AFrSem_3, tau_AFrSem_4))
  ol_settings3 = list(outcomeOrdered=EnfCond, 
                      V=zeta_EnfCond*LV_1, 
                      tau=c(tau_EnfCond_1, tau_EnfCond_2, tau_EnfCond_3, tau_EnfCond_4))
  ol_settings4 = list(outcomeOrdered=IgPare, 
                      V=zeta_IgPare*LV_1, 
                      tau=c(tau_IgPare_1, tau_IgPare_2, tau_IgPare_3, tau_IgPare_4))
  ol_settings5 = list(outcomeOrdered=OmLmVel, 
                      V=zeta_OmLmVel*LV_1, 
                      tau=c(tau_OmLmVel_1, tau_OmLmVel_2, tau_OmLmVel_3, tau_OmLmVel_4))
  ol_settings6 = list(outcomeOrdered=UsoPito, 
                      V=zeta_UsoPito*LV_1, 
                      tau=c(tau_UsoPito_1, tau_UsoPito_2, tau_UsoPito_3, tau_UsoPito_4))
  ol_settings7 = list(outcomeOrdered=CulFr, 
                      V=zeta_CulFr*LV_1, 
                      tau=c(tau_CulFr_1, tau_CulFr_2, tau_CulFr_3, tau_CulFr_4))
  ol_settings8 = list(outcomeOrdered=UsoDirec, 
                       V=zeta_UsoDirec*LV_1, 
                       tau=c(tau_UsoDirec_1, tau_UsoDirec_2,tau_UsoDirec_3,tau_UsoDirec_4))
  
  ol_settings9 = list(outcomeOrdered=PasoPeaton, 
                       V=zeta_PasoPeaton*LV_2, 
                       tau=c(tau_PasoPeaton_1, tau_PasoPeaton_2,tau_PasoPeaton_3,tau_PasoPeaton_4))
  ol_settings10 = list(outcomeOrdered=UsoDirec, 
                      V=zeta_UsoDirec*LV_2, 
                      tau=c(tau_UsoDirec_1, tau_UsoDirec_2,tau_UsoDirec_3,tau_UsoDirec_4))
  
  
  P[["indic_FRbr"]]       = apollo_ol(ol_settings1, functionality)
  P[["indic_AFrSem"]]     = apollo_ol(ol_settings2, functionality)
  P[["indic_EnfCond"]]    = apollo_ol(ol_settings3, functionality)
  P[["indic_IgPare"]]     = apollo_ol(ol_settings4, functionality)
  P[["indic_OmLmVel"]]    = apollo_ol(ol_settings5, functionality)
  P[["indic_UsoPito"]]    = apollo_ol(ol_settings6, functionality)
  P[["indic_CulFr"]]      = apollo_ol(ol_settings7, functionality)
  P[["indic_UsoDirec"]]   = apollo_ol(ol_settings8, functionality)
  
  P[["indic_PasoPeaton"]] = apollo_ol(ol_settings9, functionality)
  P[["indic_UsoDirec"]]   = apollo_ol(ol_settings10, functionality)
  

  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_tt * T_Alt_1 + b_dt * D_Alt_1 + 
                     b_Cong * CG_Alt_1 +
                     b_Sem*SEM_A1_km + 
                     b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
                     b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
                     b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
                     b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + 
                     b_No_MTRP * NO_MTRP_A1 + b_Si_MTRP * SI_MTRP_A1)
  
  V[['ruta2']]  = (asc_ruta2  + b_tt * T_Alt_2 + b_dt * D_Alt_2 + 
                     b_Cong * CG_Alt_2 +
                     b_Sem*SEM_A2_km + 
                     b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 + 
                     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
                     b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
                     b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + 
                     b_No_MTRP * NO_MTRP_A2 + b_Si_MTRP * SI_MTRP_A2)
  
  V[['ruta3']]  = (asc_ruta3  + b_tt * T_Alt_3 + b_dt * D_Alt_3 + 
                     b_Cong * CG_Alt_3 + 
                     b_Sem*SEM_A3_km +
                     b_ACC_0*ACC_A3_0 + b_ACC_1*ACC_A3_1 + b_ACC_2*ACC_A3_2 +  
                     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +
                     b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 + 
                     b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3 + 
                     b_No_MTRP * NO_MTRP_A3 + b_Si_MTRP * SI_MTRP_A3)
  
  V[['rutaEC']] = (asc_ruta4 + b_tt * T_Alt_4   + b_dt * D_Alt_4 + 
                     b_Cong * CG_Alt_4 +
                     b_Sem*SEM_EC_km +
                     b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
                     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
                     b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
                     b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + 
                     b_No_MTRP * NO_MTRP_EC + b_Si_MTRP * SI_MTRP_EC +
                     b_No_Info * SININFOTRF + b_Si_Info * CONINFOTRF +
                     b_UsoCel_P * UsoCel_Poco +  b_UsoCel_F * UsoCel_Frec +
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

