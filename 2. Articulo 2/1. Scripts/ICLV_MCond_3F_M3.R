
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #


### Limpiar memoria
rm(list = ls())

## Semilla
set.seed(1234)


### Cargar libreria Apollo
library(apollo)
library(caret)

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/3. Resultados"
setwd(workingDirectory)


### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV_MCond_3F_M3 ",
  modelDescr = "ICLV Modo Conduccion de 3F",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #


#database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Bases de datos/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

database = readRDS("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Bases de datos/DBCOMPLETA.rds")


# Normalización de las variables tiempo
for (i in 1:nrow(database)){ 
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0-1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  
  if(database$HPICOHVALLE[i] == 1){
    database$Vel_fl_Alt1[i] = 28/60;
    database$Vel_fl_Alt2[i] = 28/60;
    database$Vel_fl_Alt3[i] = 28/60;
    database$Vel_fl_Alt4[i] = 28/60}
  else{database$Vel_fl_Alt1[i] = 22/60;
  database$Vel_fl_Alt2[i] = 22/60;
  database$Vel_fl_Alt3[i]= 22/60; 
  database$Vel_fl_Alt4[i]= 22/60}
  
  database$T_fl_Alt1[i] = database$DISTAlt1[i]/database$Vel_fl_Alt1[i]
  database$T_fl_Alt2[i] = database$DISTAlt2[i]/database$Vel_fl_Alt2[i]
  database$T_fl_Alt3[i] = database$DISTAlt3[i]/database$Vel_fl_Alt3[i]
  database$T_fl_Alt4[i] = database$DISTEC[i]/database$Vel_fl_Alt4[i]
  
  database$CG_Alt_1[i] = database$TIEMPOAlt1[i]/database$T_fl_Alt1[i]
  database$CG_Alt_2[i] = database$TIEMPOAlt2[i]/database$T_fl_Alt2[i]
  database$CG_Alt_3[i] = database$TIEMPOAlt3[i]/database$T_fl_Alt3[i]
  database$CG_Alt_4[i] = database$TIEMPOEC[i]/database$T_fl_Alt4[i]
}

database$UsoCel_Poco <- (ifelse((database$UsoCel == 1), 1,0))
#database$UsoCel_AlgVec <- (ifelse((database$UsoCel == 3 ), 1,0))
database$UsoCel_Frec <- (ifelse((database$UsoCel > 1 ), 1,0))


### PARTICION DE LA BASE DE DATOS ####

registros <- createDataPartition(database$CHOICE, p = 0.75, list = FALSE)
database <- database[registros,]
db_test <- database[-registros,]


# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_ruta4  = 0,
              b_tt  = 0,  
              b_dt  = 0,
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
              lambda3       = 1,
              
              #LV_1
              gamma_EDUBASICA_LV1 = -0.158,
              gamma_EDUSUP_LV1 = -0.418,
              gamma_EXP_5_LV1 = 0.085,
              gamma_HPICO_LV1 = -0.087,
              gamma_SININFOTRF_LV1 = -0.246,
              gamma_LV2_LV1 = 0.993,
              
              #LV_2
              gamma_EXP_4_LV2 = -0.125,
              gamma_HPICO_LV2 = 0.179,
              gamma_USODISPMOB_LV2 = -0.144,
              
              #LV_3
              gamma_EDUBASICA_LV3 = -0.435,
              gamma_JOVEN30_LV3 = -0.552,
              gamma_ADULTO40_LV3 = -0.360,
              gamma_EXP_2_LV3 = 0.459,
              gamma_HTRB_2_LV3 = -0.919,
              gamma_HTRB_3_LV3 = -0.727,
              gamma_SININFOTRF_LV3 = 0.196,
              
              zeta_FRbr     = 1, zeta_EnfCond  = 1, zeta_AFrSem   = 1, zeta_CulFr    = 1, zeta_OmLmVel  = 1, 
              zeta_IgPare    = 1,  
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
  interNormDraws=c("eta1","eta2","eta3")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_2"]] =  gamma_EXP_4_LV2*EXP_4 + gamma_HPICO_LV2*HPICO + gamma_USODISPMOB_LV2 * USODISPMOB + eta2
  
  randcoeff[["LV_1"]] = gamma_EDUBASICA_LV1*EDUBASICA + gamma_EDUSUP_LV1*EDUSUP +
    gamma_EXP_5_LV1*EXP_5 + gamma_HPICO_LV1*HPICO + 
    gamma_SININFOTRF_LV1*SININFOTRF + gamma_LV2_LV1*randcoeff[["LV_2"]] + eta1
  
  randcoeff[["LV_3"]] = gamma_EDUBASICA_LV3*EDUBASICA + gamma_JOVEN30_LV3*JOVEN30 + 
    gamma_ADULTO40_LV3*ADULTO40 + gamma_EXP_2_LV3*EXP_2 + gamma_HTRB_2_LV3*HTRB_2 + gamma_HTRB_3_LV3*HTRB_3 +
    gamma_SININFOTRF_LV3*SININFOTRF + eta3
  
  
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
  ol_settings4 = list(outcomeOrdered=UsoPito, 
                      V=zeta_UsoPito*LV_1, 
                      tau=c(tau_UsoPito_1, tau_UsoPito_2, tau_UsoPito_3, tau_UsoPito_4))
  ol_settings5 = list(outcomeOrdered=CulFr, 
                      V=zeta_CulFr*LV_1, 
                      tau=c(tau_CulFr_1, tau_CulFr_2, tau_CulFr_3, tau_CulFr_4))
  
  ol_settings6 = list(outcomeOrdered=IgPare, 
                      V=zeta_IgPare*LV_2, 
                      tau=c(tau_IgPare_1, tau_IgPare_2, tau_IgPare_3, tau_IgPare_4))
  ol_settings7 = list(outcomeOrdered=OmLmVel, 
                      V=zeta_OmLmVel*LV_2, 
                      tau=c(tau_OmLmVel_1, tau_OmLmVel_2, tau_OmLmVel_3, tau_OmLmVel_4))
  
  ol_settings8 = list(outcomeOrdered=PasoPeaton, 
                       V=zeta_PasoPeaton*LV_3, 
                       tau=c(tau_PasoPeaton_1, tau_PasoPeaton_2,tau_PasoPeaton_3,tau_PasoPeaton_4))
  ol_settings9 = list(outcomeOrdered=UsoDirec, 
                      V=zeta_UsoDirec*LV_3, 
                      tau=c(tau_UsoDirec_1, tau_UsoDirec_2,tau_UsoDirec_3,tau_UsoDirec_4))
  
  
  P[["indic_FRbr"]]       = apollo_ol(ol_settings1, functionality)
  P[["indic_AFrSem"]]     = apollo_ol(ol_settings2, functionality)
  P[["indic_EnfCond"]]    = apollo_ol(ol_settings3, functionality)
  P[["indic_UsoPito"]]    = apollo_ol(ol_settings4, functionality)
  P[["indic_CulFr"]]      = apollo_ol(ol_settings5, functionality)
  
  P[["indic_IgPare"]]     = apollo_ol(ol_settings6, functionality)
  P[["indic_OmLmVel"]]    = apollo_ol(ol_settings7, functionality)
  
  P[["indic_PasoPeaton"]] = apollo_ol(ol_settings8, functionality)
  P[["indic_UsoDirec"]]   = apollo_ol(ol_settings9, functionality)
  

  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_tt * T_Alt_1*(1 + CG_Alt_1) + b_dt * D_Alt_1 +
                     b_Sem*SEM_A1_km + 
                     b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
                     b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
                     b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
                     b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + 
                     b_No_MTRP * NO_MTRP_A1 + b_Si_MTRP * SI_MTRP_A1)
  
  V[['ruta2']]  = ( asc_ruta2  + b_tt * T_Alt_2*(1 + CG_Alt_2) + b_dt * D_Alt_2 +
                     b_Sem*SEM_A2_km + 
                     b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 + 
                     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
                     b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
                     b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + 
                     b_No_MTRP * NO_MTRP_A2 + b_Si_MTRP * SI_MTRP_A2)
  
  V[['ruta3']]  = (asc_ruta3  + b_tt * T_Alt_3*(1 + CG_Alt_3) + b_dt * D_Alt_3 + 
                     b_Sem*SEM_A3_km +
                     b_ACC_0*ACC_A3_0 + b_ACC_1*ACC_A3_1 + b_ACC_2*ACC_A3_2 +  
                     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +
                     b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 + 
                     b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3 + 
                     b_No_MTRP * NO_MTRP_A3 + b_Si_MTRP * SI_MTRP_A3)
  
  V[['rutaEC']] = (asc_ruta4 + b_tt * T_Alt_4*(1 + CG_Alt_4) + b_dt * D_Alt_4 +  
                     b_Sem*SEM_EC_km +
                     b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
                     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
                     b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
                     b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + 
                     b_No_MTRP * NO_MTRP_EC + b_Si_MTRP * SI_MTRP_EC +
                     b_No_Info * SININFOTRF + b_Si_Info * CONINFOTRF +
                     b_UsoCel_P * UsoCel_Poco +  b_UsoCel_F * UsoCel_Frec +
                     lambda1 * LV_1 + lambda2 * LV_2 + lambda3 * LV_3 )
  
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
#sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

#forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                              modelComponent="indic_FRbr")

# ----------------------------------------------------------------- #
#---- CONDITIONALS AND UNCONDITIONALS                            ----
# ----------------------------------------------------------------- #

#conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)

#unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

#if(sink.number()>0) sink()


#### VALIDACION DEL MODELO ##########

## Aciertos sobre la base del modelo

db <- database

ce <- lastFuncParam        # coefficients b1, b2
#eta1 = rnorm(nrow(db), 0, 1)
eta1 = apollo_inputs[["draws"]][["eta1"]]
#eta2 = rnorm(nrow(db), 0, 1)
eta2 = apollo_inputs[["draws"]][["eta2"]]
#eta3 = rnorm(nrow(db), 0, 1)
eta3 = apollo_inputs[["draws"]][["eta3"]]

## Variables latentes
LV_1 = array()
LV_2 = array()
LV_3 = array()

LV_2 =  ce[27]*db$EXP_3 + ce[28]*db$EXP_4 +
  ce[29]*db$EXP_5 + ce[30]*db$HPICO + ce[31] * db$USODISPMOB + eta2

LV_1 = ce[18]*db$EDUBASICA + ce[19]*db$EDUSUP + ce[20]*db$EXP_2 +
  ce[21]*db$EXP_5 + ce[22]*db$HTRB_2 + ce[23]*db$HPICO + ce[24]*db$CSECO + 
  ce[25]*db$SININFOTRF + ce[26]*LV_2 + eta1

LV_3 = ce[32]*db$EDUBASICA + ce[33]*db$EDUSUP + ce[34]*db$JOVEN30 + 
  ce[35]*db$ADULTO40 + ce[36]*db$EXP_2 + ce[37]*db$HTRB_2 + ce[38]*db$HTRB_3 +
  ce[39]*db$SININFOTRF + ce[40] * db$USODISPMOB + eta3


V_1 = array()
V_2 = array()
V_3 = array()
V_4 = array()


V_1 = ce[1]  + ce[4] * db$T_Alt_1*(1 + db$CG_Alt_1) + ce[5] * db$D_Alt_1 +
         ce[6] * db$SEM_A1_km + 
         0 * db$ACC_A1_0 + ce[7] * db$ACC_A1_1 + ce[8] * db$ACC_A1_2 + 
         0 * db$NO_CAMFD_A1 + ce[9] * db$SI_CAMFD_A1 +
         0 * db$NO_PANEL_A1 + ce[10] * db$SI_PANEL_A1 + 
         0 * db$NO_ZER_A1 + ce[11] * db$SI_ZER_A1 + 
         0 * db$NO_MTRP_A1 + ce[12] * db$SI_MTRP_A1
  
V_2 = ce[2]  + ce[4] * db$T_Alt_2*(1 + db$CG_Alt_2) + ce[5] * db$D_Alt_2 +
  ce[6]*db$SEM_A2_km + 
  0 * db$ACC_A2_0 + ce[7]* db$ACC_A2_1 + ce[8]* db$ACC_A2_2 + 
  0 * db$NO_CAMFD_A2 + ce[9] * db$SI_CAMFD_A2 +
  0 * db$NO_PANEL_A2 + ce[10] * db$SI_PANEL_A2 + 
  0 * db$NO_ZER_A2 + ce[11] * db$SI_ZER_A2 + 
  0 * db$NO_MTRP_A2 + ce[12] * db$SI_MTRP_A2

V_3 = 0  + ce[4] * db$T_Alt_3*(1 + db$CG_Alt_3) + ce[5] * db$D_Alt_3 +
  ce[6] * db$SEM_A3_km +
  0 * db$ACC_A3_0 + ce[7]* db$ACC_A3_1 + ce[8]* db$ACC_A3_2 +  
  0 * db$NO_CAMFD_A3 + ce[9] * db$SI_CAMFD_A3 +
  0 * db$NO_PANEL_A3 + ce[10] * db$SI_PANEL_A3 + 
  0 * db$NO_ZER_A3 + ce[11] * db$SI_ZER_A3 + 
  0 * db$NO_MTRP_A3 + ce[12] * db$SI_MTRP_A3

V_4 = ce[3] + ce[4] * db$T_Alt_4*(1 + db$CG_Alt_4) + ce[5] * db$D_Alt_4 + 
  ce[6] * db$SEM_EC_km +
  0 * db$ACC_EC_0 + ce[7] * db$ACC_EC_1 + ce[8] * db$ACC_EC_2 + 
  0 * db$NO_CAMFD_EC + ce[9] * db$SI_CAMFD_EC +
  0 * db$NO_PANEL_EC + ce[10] * db$SI_PANEL_EC + 
  0 * db$NO_ZER_EC + ce[11] * db$SI_ZER_EC + 
  0 * db$NO_MTRP_EC + ce[12] * db$SI_MTRP_EC +
  0 * db$CONINFOTRF + ce[13] * db$SININFOTRF + 
  0 * db$UsoCel_Poco +  ce[14] * db$UsoCel_Frec +
  ce[15] * LV_1 + ce[16] * LV_2 + ce[17] * LV_3

P_r1 = array()
P_r2 = array()
P_r3 = array()
P_r4 = array()

for (i in 1:nrow(db)){
  P_r1[i] = exp(V_1[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r2[i] = exp(V_2[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r3[i] = exp(V_3[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r4[i] = exp(V_4[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))}

db$P_r1 <- P_r1
db$P_r2 <- P_r2
db$P_r3 <- P_r3
db$P_r4 <- P_r4


Eleccion = array()

for (i in 1:nrow(db)){
  if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r1[i]) {Eleccion[i] = 1}
  else
  {if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r2[i]) {Eleccion[i] = 2}
    else
    {if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r3[i]) {Eleccion[i] = 3}
      else
      {Eleccion[i] = 4
      }}}}

Prob <- cbind(P_r1, P_r2, P_r3, P_r4)
Prob
table(predicted = Eleccion, Real = db$CHOICE)
mean(factor(Eleccion, ordered = TRUE) == db$CHOICE)


Umbral = array()
err = 0.15

for (i in 1:nrow(db)){
  if (db$CHOICE[i] != Eleccion[i]){
    if (Eleccion[i] == 1){
      if (abs(db$P_r1[i]-db$P_r2[i]) < err){Umbral[i] = 2}
      else
      {if (abs(db$P_r1[i]-db$P_r3[i]) < err){Umbral[i] = 3}
        else
        {if (abs(db$P_r1[i]-db$P_r4[i]) < err){Umbral[i] = 4}
          else
          {Umbral[i] = 1}}}}
    else {if (Eleccion[i] == 2){
      if (abs(db$P_r2[i]-db$P_r1[i]) < err){Umbral[i] = 1}
      else
      {if (abs(db$P_r2[i]-db$P_r3[i]) < err){Umbral[i] = 3}
        else
        {if (abs(db$P_r2[i]-db$P_r4[i]) < err){Umbral[i] = 4}
          else
          {Umbral[i] = 2}}}}
      else
      { if (Eleccion[i] == 3){
        if (abs(db$P_r3[i]-db$P_r1[i]) < err){Umbral[i] = 1}
        else
        {if (abs(db$P_r3[i]-db$P_r2[i]) < err){Umbral[i] = 2}
          else
          {if (abs(db$P_r3[i]-db$P_r4[i]) < err){Umbral[i] = 4}
            else
            {Umbral[i] = 3}}}}
        else
        {if (Eleccion[i] == 4){
          if (abs(db$P_r4[i]-db$P_r1[i]) < err){Umbral[i] = 1}
          else
          {if (abs(db$P_r4[i]-db$P_r2[i]) < err){Umbral[i] = 2}
            else
            {if (abs(db$P_r4[i]-db$P_r3[i]) < err){Umbral[i] = 3}
              else
              {Umbral[i] = 4}}}}}}}}
  else
  {Umbral[i] = db$CHOICE[i]}}

Umbral
Eleccion
NewProb <- cbind(P_r1, P_r2, P_r3, P_r4, db$CHOICE, Eleccion, Umbral)
NewProb

table(predicted = Umbral, Real = db$CHOICE)
mean(factor(Umbral, ordered = TRUE) == db$CHOICE)




#### VALIDACION DEL MODELO EN TESTING  ##########

## Aciertos sobre la base de testeo

db <-db_test

ce <- lastFuncParam        # coefficients b1, b2

etaT1 = rnorm(nrow(db), 0, 0)
etaT2 = rnorm(nrow(db), 0, 0)
etaT3 = rnorm(nrow(db), 0, 0)

## Variables latentes
LVT_1 = array()
LVT_2 = array()
LVT_3 = array()

LVT_2 =  ce[27]*db$EXP_3 + ce[28]*db$EXP_4 +
  ce[29]*db$EXP_5 + ce[30]*db$HPICO + ce[31] * db$USODISPMOB 

LVT_1 = ce[18]*db$EDUBASICA + ce[19]*db$EDUSUP + ce[20]*db$EXP_2 +
  ce[21]*db$EXP_5 + ce[22]*db$HTRB_2 + ce[23]*db$HPICO + ce[24]*db$CSECO + 
  ce[25]*db$SININFOTRF + ce[26]*LVT_2 

LVT_3 = ce[32]*db$EDUBASICA + ce[33]*db$EDUSUP + ce[34]*db$JOVEN30 + 
  ce[35]*db$ADULTO40 + ce[36]*db$EXP_2 + ce[37]*db$HTRB_2 + ce[38]*db$HTRB_3 +
  ce[39]*db$SININFOTRF + ce[40] * db$USODISPMOB 


VT_1 = array()
VT_2 = array()
VT_3 = array()
VT_4 = array()


VT_1 = ce[1]  + ce[4] * db$T_Alt_1*(1 + db$CG_Alt_1) + ce[5] * db$D_Alt_1 +
  ce[6] * db$SEM_A1_km + 
  0 * db$ACC_A1_0 + ce[7] * db$ACC_A1_1 + ce[8] * db$ACC_A1_2 + 
  0 * db$NO_CAMFD_A1 + ce[9] * db$SI_CAMFD_A1 +
  0 * db$NO_PANEL_A1 + ce[10] * db$SI_PANEL_A1 + 
  0 * db$NO_ZER_A1 + ce[11] * db$SI_ZER_A1 + 
  0 * db$NO_MTRP_A1 + ce[12] * db$SI_MTRP_A1


VT_2 = ce[2]  + ce[4] * db$T_Alt_2*(1 + db$CG_Alt_2) + ce[5] * db$D_Alt_2 +
  ce[6]*db$SEM_A2_km + 
  0 * db$ACC_A2_0 + ce[7]* db$ACC_A2_1 + ce[8]* db$ACC_A2_2 + 
  0 * db$NO_CAMFD_A2 + ce[9] * db$SI_CAMFD_A2 +
  0 * db$NO_PANEL_A2 + ce[10] * db$SI_PANEL_A2 + 
  0 * db$NO_ZER_A2 + ce[11] * db$SI_ZER_A2 + 
  0 * db$NO_MTRP_A2 + ce[12] * db$SI_MTRP_A2

VT_3 = 0  + ce[4] * db$T_Alt_3*(1 + db$CG_Alt_3) + ce[5] * db$D_Alt_3 +
  ce[6] * db$SEM_A3_km +
  0 * db$ACC_A3_0 + ce[7]* db$ACC_A3_1 + ce[8]* db$ACC_A3_2 +  
  0 * db$NO_CAMFD_A3 + ce[9] * db$SI_CAMFD_A3 +
  0 * db$NO_PANEL_A3 + ce[10] * db$SI_PANEL_A3 + 
  0 * db$NO_ZER_A3 + ce[11] * db$SI_ZER_A3 + 
  0 * db$NO_MTRP_A3 + ce[12] * db$SI_MTRP_A3

VT_4 = ce[3] + ce[4] * db$T_Alt_4*(1 + db$CG_Alt_4) + ce[5] * db$D_Alt_4 + 
  ce[6] * db$SEM_EC_km +
  0 * db$ACC_EC_0 + ce[7] * db$ACC_EC_1 + ce[8] * db$ACC_EC_2 + 
  0 * db$NO_CAMFD_EC + ce[9] * db$SI_CAMFD_EC +
  0 * db$NO_PANEL_EC + ce[10] * db$SI_PANEL_EC + 
  0 * db$NO_ZER_EC + ce[11] * db$SI_ZER_EC + 
  0 * db$NO_MTRP_EC + ce[12] * db$SI_MTRP_EC +
  0 * db$CONINFOTRF + ce[13] * db$SININFOTRF + 
  0 * db$UsoCel_Poco +  ce[14] * db$UsoCel_Frec +
  ce[15] * LVT_1 + ce[16] * LVT_2 + ce[17] * LVT_3

PT_r1 = array()
PT_r2 = array()
PT_r3 = array()
PT_r4 = array()

for (i in 1:nrow(db)){
  PT_r1[i] = exp(VT_1[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))
  PT_r2[i] = exp(VT_2[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))
  PT_r3[i] = exp(VT_3[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))
  PT_r4[i] = exp(VT_4[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))}

db$PT_r1 <- PT_r1
db$PT_r2 <- PT_r2
db$PT_r3 <- PT_r3
db$PT_r4 <- PT_r4


EleccionTest = array()

for (i in 1:nrow(db)){
  if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r3[i],db$PT_r4[i]) == db$PT_r1[i]) {EleccionTest[i] = 1}
  else
  {if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r3[i],db$PT_r4[i]) == db$PT_r2[i]) {EleccionTest[i] = 2}
    else
    {if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r3[i],db$PT_r4[i]) == db$PT_r3[i]) {EleccionTest[i] = 3}
      else
      {EleccionTest[i] = 4
      }}}}

ProbT <- cbind(PT_r1, PT_r2, PT_r3, PT_r4)
ProbT
table(predicted = EleccionTest, Real = db$CHOICE)
mean(factor(EleccionTest, ordered = TRUE) == db$CHOICE)


UmbralTest = array()
errT = 0.15

for (i in 1:nrow(db)){
  if (db$CHOICE[i] != EleccionTest[i]){
    if (EleccionTest[i] == 1){
      if (abs(db$PT_r1[i]-db$PT_r2[i]) < errT){UmbralTest[i] = 2}
      else
      {if (abs(db$PT_r1[i]-db$PT_r3[i]) < err){UmbralTest[i] = 3}
        else
        {if (abs(db$PT_r1[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
          else
          {UmbralTest[i] = 1}}}}
    else {if (EleccionTest[i] == 2){
      if (abs(db$PT_r2[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
      else
      {if (abs(db$PT_r2[i]-db$PT_r3[i]) < err){UmbralTest[i] = 3}
        else
        {if (abs(db$PT_r2[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
          else
          {UmbralTest[i] = 2}}}}
      else
      { if (EleccionTest[i] == 3){
        if (abs(db$PT_r3[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
        else
        {if (abs(db$PT_r3[i]-db$PT_r2[i]) < err){UmbralTest[i] = 2}
          else
          {if (abs(db$PT_r3[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
            else
            {UmbralTest[i] = 3}}}}
        else
        {if (EleccionTest[i] == 4){
          if (abs(db$PT_r4[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
          else
          {if (abs(db$PT_r4[i]-db$PT_r2[i]) < err){UmbralTest[i] = 2}
            else
            {if (abs(db$PT_r4[i]-db$PT_r3[i]) < err){UmbralTest[i] = 3}
              else
              {UmbralTest[i] = 4}}}}}}}}
  else
  {UmbralTest[i] = db$CHOICE[i]}}

UmbralTest
EleccionTest
NewProbT <- cbind(PT_r1, PT_r2, PT_r3, PT_r4, db$CHOICE, EleccionTest, UmbralTest)
NewProbT

table(predicted = UmbralTest, Real = db$CHOICE)
mean(factor(UmbralTest, ordered = TRUE) == db$CHOICE)

