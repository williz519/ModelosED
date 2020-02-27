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
  modelName  = "MNL_ModVSG",
  modelDescr = "MNL model con variación sistematica de gustos",
  indivID    = "ViajeId"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
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
              b_AdultoMayor = 0
)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta1", "b_AdultoMayor", "b_EduSuperior", "b_Hpico")

### Lea los valores iniciales para al menos algunos parámetros del archivo de salida del modelo existente
apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "MNL_ModoCond", overwriteFixed=FALSE)
#apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed)


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
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
  
  ### Cree constantes y coeficientes específicos alternativos utilizando interacciones con datos sociodemográficos.
  # Variacion sistematica de gustos
  
  b_time_vsg = b_time + b_cong * CONG_CD + b_cong * CONG_EF
  b_dist_vsg = b_dist + b_Info_traf * (INFOTRAFICO == 2) + b_usodisp *  USODISPMOB
  
  
  
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
  
  V[['rutaEC']] = asc_rutaEC + b_time_vsg * TIEMPOEC   + b_dist_vsg * DISTEC   + b_cong * CONGESTION +
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
#### MODEL ESTIMATION                                            ####
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

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST SIMPLE MNL MODEL                           ----
# ----------------------------------------------------------------- #

#apollo_lrTest("Apollo_example_2", "Apollo_example_3")
apollo_lrTest("MNL_ModoCond", model)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, apollo_probabilities, apollo_inputs)

### Look at summary of the predicted choice probabilities
summary(predictions_base)

### Now imagine the cost for rail increases by 10%
#database$cost_rail = 1.1*database$cost_rail

### Rerun predictions with the new data, and save into a separate matrix
predictions_new = apollo_prediction(model, apollo_probabilities, apollo_inputs)

### Look at summary of the predicted choice probabilities
summary(predictions_new)

### Return to original data
#database$cost_rail = 1/1.1*database$cost_rail

### Compute change in probabilities
change=(predictions_new-predictions_base)/predictions_base

### Not interested in chosen alternative now, so drop last column
change=change[,-ncol(change)]

### Look at first individual
change[database$ID==1,]
### And person 9, who has all 4 modes available
change[database$ID==9,]

### Summary of changes (possible presence of NAs for unavailable alternatives)
summary(change)

### Look at mean changes for subsets of the data, ignoring NAs
colMeans(change,na.rm=TRUE)
colMeans(subset(change,database$business==1),na.rm=TRUE)
colMeans(subset(change,database$business==0),na.rm=TRUE)
colMeans(subset(change,(database$income<quantile(database$income,0.25))),na.rm=TRUE)
colMeans(subset(change,(database$income>=quantile(database$income,0.25))|(database$income<=quantile(database$income,0.75))),na.rm=TRUE)
colMeans(subset(change,(database$income>quantile(database$income,0.75))),na.rm=TRUE)

### Compute own elasticity for rail:
log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)

### Compute cross-elasticities for other modes
log(sum(predictions_new[,1])/sum(predictions_base[,1]))/log(1.1)
log(sum(predictions_new[,2])/sum(predictions_base[,2]))/log(1.1)
log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)

# ----------------------------------------------------------------- #
#---- RECOVERY OF SHARES FOR ALTERNATIVES IN DATABASE            ----
# ----------------------------------------------------------------- #

sharesTest_settings = list()
sharesTest_settings=list()
sharesTest_settings[["alternatives"]] = c(car=1, bus=2, air=3, rail=4)
sharesTest_settings[["choiceVar"]]    = database$choice
sharesTest_settings[["subsamples"]] = list(business=(database$business==1),
                                           leisure=(database$business==0))

apollo_sharesTest(model,apollo_probabilities,apollo_inputs,sharesTest_settings)

# ----------------------------------------------------------------- #
#---- MODEL PERFORMANCE IN SUBSETS OF DATABASE                   ----
# ----------------------------------------------------------------- #

fitsTest_settings = list()

fitsTest_settings[["subsamples"]] = list()
fitsTest_settings$subsamples[["business"]] = database$business==1
fitsTest_settings$subsamples[["leisure"]] = database$business==0
apollo_fitsTest(model,apollo_probabilities,apollo_inputs,fitsTest_settings)

# ----------------------------------------------------------------- #
#---- FUNCTIONS OF MODEL PARAMETERS                              ----
# ----------------------------------------------------------------- #

deltaMethod_settings=list(operation="ratio", parName1="b_tt_car", parName2="b_cost")
apollo_deltaMethod(model, deltaMethod_settings)

deltaMethod_settings=list(operation="ratio", parName1="b_tt_car", parName2="b_cost", multPar1 = 60)
apollo_deltaMethod(model, deltaMethod_settings)

deltaMethod_settings=list(operation="diff", parName1="b_tt_car", parName2="b_tt_rail")
apollo_deltaMethod(model, deltaMethod_settings)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()
  

