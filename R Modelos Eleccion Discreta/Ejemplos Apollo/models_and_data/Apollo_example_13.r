# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_example_13",
  modelDescr ="MDCNEV model on time use data, alpha-gamma profile with outside good and socio-demographics",
  indivID    ="indivID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_timeUseData.csv",header=TRUE)

### Create consumption variables for combined activities
database$t_outside = rowSums(database[,c("t_a01", "t_a06", "t_a10", "t_a11", "t_a12")]) # outside good: time spent at home and travelling
database$t_leisure = rowSums(database[,c("t_a07", "t_a08", "t_a09")])

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(alpha_base         = 0,
                gamma_work         = 1,
                gamma_school       = 1,
                gamma_shopping     = 1,
                gamma_private      = 1,
                gamma_leisure      = 1,
                delta_work         = 0,
                delta_school       = 0,
                delta_shopping     = 0,
                delta_private      = 0,
                delta_leisure      = 0,
                delta_work_FT      = 0,
                delta_work_wknd    = 0,
                delta_school_young = 0,
                delta_leisure_wknd = 0,
                theta_mandatory    = 1,
                theta_optional     = 1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("theta_mandatory")

### Read in starting values for at least some parameters from existing model output file
apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"Apollo_example_11",overwriteFixed=FALSE)

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
  
  ### Define individual alternatives
  alternatives  = c("outside",
                    "work",
                    "school",
                    "shopping",
                    "private", 
                    "leisure")
  
  ### Define availabilities
  avail = list(outside  = 1, 
               work     = 1, 
               school   = 1, 
               shopping = 1, 
               private  = 1,
               leisure  = 1)
  
  ### Define continuous consumption for individual alternatives
  continuousChoice = list(outside  = t_outside/60,
                          work     = t_a02/60,
                          school   = t_a03/60,
                          shopping = t_a04/60,
                          private  = t_a05/60,
                          leisure  = t_leisure/60)
  
  ### Define utilities for individual alternatives
  V = list()
  V[["outside"]]  = 0
  V[["work"]]     = delta_work     + delta_work_FT * occ_full_time + delta_work_wknd * weekend
  V[["school"]]   = delta_school   + delta_school_young * (age<=30)
  V[["shopping"]] = delta_shopping
  V[["private"]]  = delta_private
  V[["leisure"]]  = delta_leisure  + delta_leisure_wknd*weekend
  
  ### Define alpha parameters
  alpha = list(outside  = 1 /(1 + exp(-alpha_base)), 
               work     = 1 /(1 + exp(-alpha_base)), 
               school   = 1 /(1 + exp(-alpha_base)), 
               shopping = 1 /(1 + exp(-alpha_base)), 
               private  = 1 /(1 + exp(-alpha_base)),
               leisure  = 1 /(1 + exp(-alpha_base)))
  
  ### Define gamma parameters
  gamma = list(outside  = 1,
               work     = gamma_work,    
               school   = gamma_school,
               shopping = gamma_shopping,
               private  = gamma_private,
               leisure  = gamma_leisure)
  
  ### Define nesting structure
  mdcnevNests = list(mandatory = theta_mandatory, 
                     optional  = theta_optional)
  
  mdcnevStructure  = matrix(0, nrow=length(mdcnevNests), ncol=length(V))
  ###                    outside work school shopping private leisure
  mdcnevStructure[1,] = c(     0,   1,     1,       0,      1,      0) # mandatory
  mdcnevStructure[2,] = c(     1,   0,     0,       1,      0,      1) # optional
  
  ### Define costs for individual alternatives
  cost = list(outside  = 1, 
              work     = 1, 
              school   = 1, 
              shopping = 1, 
              private  = 1,
              leisure  = 1)
  
  ### Define budget
  budget = budget/60
  
  ### Define settings for MDCNEV model
  mdcnev_settings <- list(alternatives      = alternatives,
                          avail             = avail,
                          continuousChoice  = continuousChoice,
                          V                 = V,
                          alpha             = alpha,
                          gamma             = gamma, 
                          cost              = cost,
                          budget            = budget,
                          mdcnevNests       = mdcnevNests,
                          mdcnevStructure   = mdcnevStructure)
  
  ### Compute probabilities using MDCNEV model
  P[["model"]] = apollo_mdcnev(mdcnev_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
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
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, apollo_probabilities, apollo_inputs)

colMeans(predictions_base)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()