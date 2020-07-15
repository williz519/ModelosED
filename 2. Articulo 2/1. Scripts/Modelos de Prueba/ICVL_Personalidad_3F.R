

# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Modelo sin la ruta 3
### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/ICVL_Personalidad_3F"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV_Personalidad_3F ",
  modelDescr = "ICLV Personalidad de 3F",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

# Normalización de las variables tiempo

for (i in 1:nrow(database)){
  # Normalización de las variables tiempo
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
}


# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_rutaEC  = 0,
              b_tt  = 0,  
              b_dt  = 0,
              b_CongAB  = 0, b_CongCD  = 0, b_CongEF  = 0,
              b_Sem = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = 0, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_No_MTRP = 0, b_Si_MTRP = 0,
              lambda1        = 1,
              lambda2        = 1, 
              lambda3       = 1,
              
              # Regresion LV_P1
              gamma_EDUBASICA_LV_P1 = 0.287,
              gamma_JOVEN30_LV_P1 = -0.432,
              gamma_ADULTO40_LV_P1 = -0.504,
              gamma_ADULTO60_LV_P1 = -0.503,
              gamma_EXP_2_LV_P1 = -0.510,
              gamma_EXP_3_LV_P1 = -0.585,
              gamma_HPICO_LV_P1 = 0.192,
              gamma_SININFOTRF_LV_P1 = -0.382,
              gamma_USODISPMOB_LV_P1 = 0.332,
              
              #Regresion LV_P2
              gamma_EDUBASICA_LV_P2 = 0.240,
              gamma_ADULTO60_LV_P2 = -0.154,
              gamma_EXP_2_LV_P2 = 0.738,
              gamma_EXP_3_LV_P2 = 0.349,
              gamma_EXP_4_LV_P2 = 0.371,
              gamma_EXP_5_LV_P2 = 0.378,
              gamma_CSECO_LV_P2 = -0.169,
              gamma_LV_P3_P2 = -0.593,
              
              #Regresion LV_P3
              gamma_EDUBASICA_LV_P3 = 0.227,
              gamma_JOVEN30_LV_P3 = -0.558,
              gamma_ADULTO40_LV_P3 = -0.335,
              gamma_ADULTO60_LV_P3 = -0.428,
              gamma_EXP_3_LV_P3 = 0.182,
              gamma_HTRB_2_LV_P3 = -0.210,
              gamma_HPICO_LV_P3 = 0.122,
              gamma_SININFOTRF_LV_P3 = -0.265,
              gamma_USODISPMOB_LV_P3 = -0.140,
              gamma_LV_P1_P3 = -0.621,
              
              zeta_PrPer   = 1, 
              zeta_AmbTr   = 1, 
              zeta_ComVrb  = 1,
              zeta_ComAfec = 1, 
              zeta_ConCl   = 1, 
              zeta_Ans     = 1,  
              zeta_StrC    = 1,
              tau_PrPer_1   =-2, 
              tau_PrPer_2   =-1, 
              tau_PrPer_3   = 1, 
              tau_PrPer_4   = 2,
              tau_AmbTr_1   =-2, 
              tau_AmbTr_2   =-1, 
              tau_AmbTr_3   = 1, 
              tau_AmbTr_4   = 2, 
              tau_ComVrb_1  =-2, 
              tau_ComVrb_2  =-1, 
              tau_ComVrb_3  = 1, 
              tau_ComVrb_4  = 2, 
              tau_ComAfec_1 =-2, 
              tau_ComAfec_2 =-1, 
              tau_ComAfec_3 = 1, 
              tau_ComAfec_4 = 2,
              tau_ConCl_1   =-4, 
              tau_ConCl_2   =-2, 
              tau_ConCl_3   = 2, 
              tau_ConCl_4   = 4,
              tau_Ans_1     =-2, 
              tau_Ans_2     =-1, 
              tau_Ans_3     = 1, 
              tau_Ans_4     = 2,
              tau_StrC_1    =-2, 
              tau_StrC_2    =-1, 
              tau_StrC_3    = 1, 
              tau_StrC_4    = 2)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta3", "b_CongAB", "b_ACC_0", "b_NO_CAMFD", "b_No_MTRP", "b_NO_PANEL", "b_NO_ZER",
                 "gamma_EDUBASICA_LV_P1",
                 "gamma_JOVEN30_LV_P1",
                 "gamma_ADULTO40_LV_P1",
                 "gamma_ADULTO60_LV_P1",
                 "gamma_EXP_2_LV_P1",
                 "gamma_EXP_3_LV_P1",
                 "gamma_HPICO_LV_P1",
                 "gamma_SININFOTRF_LV_P1",
                 "gamma_USODISPMOB_LV_P1",
                 "gamma_EDUBASICA_LV_P2",
                 "gamma_ADULTO60_LV_P2",
                 "gamma_EXP_2_LV_P2",
                 "gamma_EXP_3_LV_P2",
                 "gamma_EXP_4_LV_P2",
                 "gamma_EXP_5_LV_P2",
                 "gamma_CSECO_LV_P2",
                 "gamma_LV_P3_P2",
                 "gamma_EDUBASICA_LV_P3",
                 "gamma_JOVEN30_LV_P3",
                 "gamma_ADULTO40_LV_P3",
                 "gamma_ADULTO60_LV_P3",
                 "gamma_EXP_3_LV_P3",
                 "gamma_HTRB_2_LV_P3",
                 "gamma_HPICO_LV_P3",
                 "gamma_SININFOTRF_LV_P3",
                 "gamma_USODISPMOB_LV_P3",
                 "gamma_LV_P1_P3")

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
  interNormDraws=c("eta4","eta5","eta6")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_P1"]] = gamma_EDUBASICA_LV_P1*EDUBASICA + gamma_JOVEN30_LV_P1*JOVEN30 + gamma_ADULTO40_LV_P1*ADULTO40 + 
    gamma_ADULTO60_LV_P1*ADULTO60 + gamma_EXP_2_LV_P1*EXP_2 + gamma_EXP_3_LV_P1*EXP_3 + gamma_HPICO_LV_P1*HPICO + 
    gamma_SININFOTRF_LV_P1*SININFOTRF + gamma_USODISPMOB_LV_P1 * USODISPMOB + eta4
  
  randcoeff[["LV_P3"]] =  gamma_EDUBASICA_LV_P3*EDUBASICA + gamma_JOVEN30_LV_P3*JOVEN30 + gamma_ADULTO40_LV_P3*ADULTO40 + 
    gamma_ADULTO60_LV_P3*ADULTO60 + gamma_EXP_3_LV_P3*EXP_3 + gamma_HTRB_2_LV_P3*HTRB_2 + gamma_HPICO_LV_P3*HPICO + 
    gamma_SININFOTRF_LV_P3*SININFOTRF + gamma_USODISPMOB_LV_P3 * USODISPMOB +
    gamma_LV_P1_P3*randcoeff[["LV_P1"]] + eta6
  
  randcoeff[["LV_P2"]] = gamma_EDUBASICA_LV_P2*EDUBASICA +  gamma_ADULTO60_LV_P2*ADULTO60 + gamma_EXP_2_LV_P2*EXP_2 + gamma_EXP_3_LV_P2*EXP_3 +
    gamma_EXP_4_LV_P2*EXP_4 + gamma_EXP_5_LV_P2*EXP_5 + gamma_CSECO_LV_P2*CSECO + 
    gamma_LV_P3_P2*randcoeff[["LV_P3"]] + eta5
  
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
  
  ol_settings1 = list(outcomeOrdered=Ans, 
                      V=zeta_Ans * LV_P1, 
                      tau=c(tau_Ans_1, tau_Ans_2, tau_Ans_3, tau_Ans_4))
  ol_settings2 = list(outcomeOrdered=StrC, 
                      V=zeta_StrC * LV_P1, 
                      tau=c(tau_StrC_1, tau_StrC_2, tau_StrC_3, tau_StrC_4))
  ol_settings3 = list(outcomeOrdered=ComVrb, 
                      V=zeta_ComVrb * LV_P1, 
                      tau=c(tau_ComVrb_1, tau_ComVrb_2, tau_ComVrb_3, tau_ComVrb_4))
  
  ol_settings4 = list(outcomeOrdered=PrPer, 
                      V=zeta_PrPer * LV_P2, 
                      tau=c(tau_PrPer_1, tau_PrPer_2, tau_PrPer_3, tau_PrPer_4))
  ol_settings5 = list(outcomeOrdered=AmbTr, 
                      V=zeta_AmbTr * LV_P2, 
                      tau=c(tau_AmbTr_1, tau_AmbTr_2, tau_AmbTr_3, tau_AmbTr_4))
  
  ol_settings6 = list(outcomeOrdered=ComVrb, 
                      V=zeta_ComVrb * LV_P3, 
                      tau=c(tau_ComVrb_1, tau_ComVrb_2, tau_ComVrb_3, tau_ComVrb_4))
  ol_settings7 = list(outcomeOrdered=ComAfec, 
                      V=zeta_ComAfec * LV_P3, 
                      tau=c(tau_ComAfec_1, tau_ComAfec_2, tau_ComAfec_3, tau_ComAfec_4))
  ol_settings8 = list(outcomeOrdered=ConCl, 
                      V=zeta_ConCl * LV_P3, 
                      tau=c(tau_ConCl_1, tau_ConCl_2, tau_ConCl_3, tau_ConCl_4))
  
  
  P[["indic_Ans"]]      = apollo_ol(ol_settings1, functionality)
  P[["indic_StrC"]]     = apollo_ol(ol_settings2, functionality)
  P[["indic_ComVrb"]]   = apollo_ol(ol_settings3, functionality)
  
  P[["indic_PrPer"]]    = apollo_ol(ol_settings4, functionality)
  P[["indic_AmbTr"]]    = apollo_ol(ol_settings5, functionality)
  
  P[["indic_ComVrb"]]   = apollo_ol(ol_settings6, functionality)
  P[["indic_ComAfec"]]  = apollo_ol(ol_settings7, functionality)
  P[["indic_ConCl"]]    = apollo_ol(ol_settings8, functionality)
  
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_tt * T_Alt_1 + b_dt * D_Alt_1 + 
                     b_CongAB*CONG_AB_A1 + b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 +
                     b_Sem*SEM_A1_km + 
                     b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
                     b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
                     b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
                     b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + 
                     b_No_MTRP * NO_MTRP_A1 + b_Si_MTRP * SI_MTRP_A1)
  
  V[['ruta2']]  = (asc_ruta2  + b_tt * T_Alt_2 + b_dt * D_Alt_2 + 
                     b_CongAB*CONG_AB_A2 + b_CongCD*CONG_CD_A2 + b_CongEF*CONG_EF_A2 + 
                     b_Sem*SEM_A2_km + 
                     b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 + 
                     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
                     b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
                     b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + 
                     b_No_MTRP * NO_MTRP_A2 + b_Si_MTRP * SI_MTRP_A2)
  
  V[['ruta3']]  = (asc_ruta3  + b_tt * T_Alt_3 + b_dt * D_Alt_3 + 
                     b_CongAB*CONG_AB_A3 + b_CongCD*CONG_CD_A3 + b_CongEF*CONG_EF_A3 +  
                     b_Sem*SEM_A3_km +
                     b_ACC_0*ACC_A3_0 + b_ACC_1*ACC_A3_1 + b_ACC_2*ACC_A3_2 +  
                     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +
                     b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 + 
                     b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3 + 
                     b_No_MTRP * NO_MTRP_A3 + b_Si_MTRP * SI_MTRP_A3)
  
  V[['rutaEC']] = (asc_rutaEC + b_tt * T_Alt_4   + b_dt * D_Alt_4 + 
                     b_CongAB*CONG_AB_EC + b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +
                     b_Sem*SEM_EC_km +
                     b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
                     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
                     b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
                     b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + 
                     b_No_MTRP * NO_MTRP_EC + b_Si_MTRP * SI_MTRP_EC +
                     lambda1 * LV_P1 + lambda2 * LV_P2 + lambda3 * LV_P3)
  
  
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