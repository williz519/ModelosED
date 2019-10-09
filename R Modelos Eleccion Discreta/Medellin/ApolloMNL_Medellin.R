
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BASICOS                 ####
# ################################################################# #

### Limpiar memoria
rm(list = ls())

### Cargar libreria de  Apollo
library(apollo)

### C?digo de inicializaci?n
apollo_initialise()

### Establecer controles principales
apollo_control = list(
  modelName  ="Apollo_MNL_1",
  modelDescr ="Modelo MNL simple en datos RP de elecci?n de ruta",
  indivID    ="ViajeId"
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR ALGUNA TRANSFORMACI?N                ####
# ################################################################# #

database = read.csv("DataBaseVLCE.csv", header=TRUE, sep = "\t")

database$CONG_A4 <- lapply(database$CONGESTION, function(x){
  ifelse(x==1 | x==2, 1,ifelse(x==3 |x==4, 2, 3))
})

CONG_A4 <- as.numeric(database$CONG_A4)

# ################################################################# #
#### ANALISIS DE ELECCION                                        ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(Alt1=1, Alt2=2, Alt3=3, Alt4=4),
  avail        = list(Alt1=1, Alt2=1, Alt3=1, Alt4=1),
  choiceVar    = database$CHOICE,
  explanators  = database[,c("EXPERIENCIA","NIVELEDUCATIVO","EDAD")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

MNL_1_ChoiceAnalysis = read.csv("Apollo_MNL_1_choiceAnalysis.csv", header=TRUE)

# ################################################################# #
#### PARÁMETROS DEL MODELO                                       ####
# ################################################################# #

### Vector de parámetros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_Alt1   = 0,
              asc_Alt2   = 0,
              asc_Alt3   = 0,
              asc_Alt4   = 0,
              b_tt  = 0,
              b_dt  = 0,
              b_cg  = 0)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, 
# use apollo_beta_fixed = c () si ninguno

apollo_fixed = c("asc_Alt4")

# ################################################################# #
#### ENTRADAS GRUPALES Y VALIDADAS                               ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### MODELO Y FUNCION DE LIKELIHOOD                              ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Crear lista de probabilidades P
  P = list()
  
  ### Lista de utilidades: deben usar los mismos nombres que en mnl_settings, el orden es irrelevante
  V = list()
  V[['Alt1']]  = asc_Alt1  + b_tt  * TIEMPOAlt1 + b_dt * DISTAlt1 + b_cg * CONG_A1
  V[['Alt2']]  = asc_Alt2  + b_tt  * TIEMPOAlt2 + b_dt * DISTAlt2 + b_cg * CONG_A2 
  V[['Alt3']]  = asc_Alt3  + b_tt  * TIEMPOAlt3 + b_dt * DISTAlt3 + b_cg * CONG_A3  
  V[['Alt4']]  = asc_Alt4  + b_tt  * TIEMPOEC   + b_dt * DISTEC   + b_cg * CONG_A4
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(Alt1=1, Alt2=2, Alt3=3, Alt4=4),
    avail         = list(Alt1=1, Alt2=1, Alt3=1, Alt4=1),
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

MNL_1_Estimates = read.csv("Apollo_MNL_1_estimates.csv", header=TRUE)

