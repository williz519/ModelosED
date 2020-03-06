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
  modelName  ="Apollo_example_6",
  modelDescr ="CNL model with socio-demographics on mode choice SP data",
  indivID    ="ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_modeChoiceData.csv",header=TRUE)

### Use only SP data
database = subset(database,database$SP==1)

### Create new variable with average income
database$mean_income = mean(database$income)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car                 = 0,
              asc_bus                 = 0,
              asc_air                 = 0,
              asc_rail                = 0,
              asc_bus_shift_female    = 0,
              asc_air_shift_female    = 0,
              asc_rail_shift_female   = 0,
              b_tt_car                = 0,
              b_tt_bus                = 0,
              b_tt_air                = 0,
              b_tt_rail               = 0,
              b_tt_shift_business     = 0,
              b_access                   = 0,
              b_cost                  = 0,
              b_cost_shift_business   = 0,
              cost_income_elast       = 0,
              b_no_frills             = 0,
              b_wifi                  = 0,
              b_food                  = 0,
              lambda_fastPT           = 0.95,
              lambda_groundPT         = 0.95,
              alpha0_rail_fastPT      = 0,
              alpha0_rail_groundPT    = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","b_no_frills","alpha0_rail_groundPT")

### Read in starting values for at least some parameters from existing model output file
apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"Apollo_example_3",overwriteFixed=FALSE)

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

  ### Create alternative specific constants and coefficients using interactions with socio-demographics
  asc_bus_value   = asc_bus  + asc_bus_shift_female * female
  asc_air_value   = asc_air  + asc_air_shift_female * female
  asc_rail_value  = asc_rail + asc_rail_shift_female * female
  b_tt_car_value  = b_tt_car + b_tt_shift_business * business
  b_tt_bus_value  = b_tt_bus + b_tt_shift_business * business
  b_tt_air_value  = b_tt_air + b_tt_shift_business * business
  b_tt_rail_value = b_tt_rail + b_tt_shift_business * business
  b_cost_value    = ( b_cost +  b_cost_shift_business * business ) * ( income / mean_income ) ^ cost_income_elast
  
  ### List of utilities: these must use the same names as in cnl_settings, order is irrelevant
  V = list()
  V[['car']]  = asc_car        + b_tt_car_value  * time_car                        + b_cost_value * cost_car
  V[['bus']]  = asc_bus_value  + b_tt_bus_value  * time_bus  + b_access * access_bus  + b_cost_value * cost_bus 
  V[['air']]  = asc_air_value  + b_tt_air_value  * time_air  + b_access * access_air  + b_cost_value * cost_air   + b_no_frills * ( service_air == 1 )  + b_wifi * ( service_air == 2 )  + b_food * ( service_air == 3 )
  V[['rail']] = asc_rail_value + b_tt_rail_value * time_rail + b_access * access_rail + b_cost_value * cost_rail  + b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )
  
  ### Specify nests for CNL model
  cnlNests = list(fastPT=lambda_fastPT, groundPT=lambda_groundPT, car=1)

  ### Specify nest allocation parameters for alternatives included in multiple nests
  alpha_rail_fastPT   = exp(alpha0_rail_fastPT)/(exp(alpha0_rail_fastPT) + exp(alpha0_rail_groundPT))
  alpha_rail_groundPT = 1 - alpha_rail_fastPT

  ### Specify tree structure, showing membership in nests (one row per nest, one column per alternative)
  cnlStructure      = matrix(0, nrow=length(cnlNests), ncol=length(V))
  cnlStructure[1,] = c( 0,  0, 1, alpha_rail_fastPT  ) # fastPT
  cnlStructure[2,] = c( 0,  1, 0, alpha_rail_groundPT) # groundPT
  cnlStructure[3,] = c( 1,  0, 0, 0                  ) # car
  
  ### Define settings for CNL model
  cnl_settings <- list(
    alternatives = c(car=1, bus=2, air=3, rail=4),
    avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
    choiceVar    = choice,
    V            = V,
    cnlNests     = cnlNests,
    cnlStructure = cnlStructure
  )
  
  ### Compute probabilities using CNL model
  P[["model"]] = apollo_cnl(cnl_settings, functionality)
  
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

apollo_modelOutput(model, list(printT1=TRUE) )

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, list(printT1=TRUE) )

# ----------------------------------------------------------------- #
#---- COMBINED RESULTS OF GEV MODELS                             ----
# ----------------------------------------------------------------- #

apollo_combineResults(combineResults_settings = list(modelNames=c("Apollo_example_1",
                                                             "Apollo_example_2",
                                                             "Apollo_example_3",
                                                             "Apollo_example_4",
                                                             "Apollo_example_5",
                                                             "Apollo_example_6")))                                                             
