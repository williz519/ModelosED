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
  modelName  ="Apollo_example_7",
  modelDescr ="Simple RRM model on mode choice SP data",
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
apollo_beta=c(asc_car               = 0,
              asc_bus               = 0,
              asc_air               = 0,
              asc_rail              = 0,
              b_tt_car              = 0,
              b_tt_bus              = 0,
              b_tt_air              = 0,
              b_tt_rail             = 0,
              b_access                 = 0,
              b_cost                = 0,
              b_no_frills           = 0,
              b_wifi                = 0,
              b_food                = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","b_no_frills")

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
  
  ### Prepare regret components for categorical variables
  access_car  = 0
  RFrills_car = b_no_frills
  RFrills_bus = b_no_frills
  RFrills_air  = b_no_frills * ( service_air  == 1 ) + b_wifi * ( service_air  == 2 ) + b_food * ( service_air  == 3 )
  RFrills_rail = b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )

  ### List of regret functions: these must use the same names as in mnl_settings, order is irrelevant
  R = list()
  R[['car']]  = asc_car  + 
    log(1+exp(b_tt_bus*time_bus   - b_tt_car*time_car)) + 
    log(1+exp(b_tt_air*time_air   - b_tt_car*time_car)) + 
    log(1+exp(b_tt_rail*time_rail - b_tt_car*time_car)) + 
    log(1+exp(b_cost*(cost_bus  - cost_car))) + 
    log(1+exp(b_cost*(cost_air  - cost_car))) + 
    log(1+exp(b_cost*(cost_rail - cost_car))) + 
    log(1+exp(b_access*(access_bus - access_car))) + 
    log(1+exp(b_access*(access_air - access_car))) + 
    log(1+exp(b_access*(access_rail- access_car))) +
    log(1+exp(RFrills_bus  - RFrills_car))  + 
    log(1+exp(RFrills_air  - RFrills_car))  + 
    log(1+exp(RFrills_rail - RFrills_car))
  R[['bus']]  = asc_bus  + 
    log(1+exp(b_tt_car*time_car   - b_tt_bus*time_bus)) + 
    log(1+exp(b_tt_air*time_air   - b_tt_bus*time_bus)) + 
    log(1+exp(b_tt_rail*time_rail - b_tt_bus*time_bus)) + 
    log(1+exp(b_cost*(cost_car  - cost_bus))) + 
    log(1+exp(b_cost*(cost_air  - cost_bus))) + 
    log(1+exp(b_cost*(cost_rail - cost_bus))) + 
    log(1+exp(b_access*(access_car - access_bus))) + 
    log(1+exp(b_access*(access_air - access_bus))) + 
    log(1+exp(b_access*(access_rail- access_bus))) +
    log(1+exp(RFrills_car  - RFrills_bus))  + 
    log(1+exp(RFrills_air  - RFrills_bus))  + 
    log(1+exp(RFrills_rail - RFrills_bus))
  R[['air']]  = asc_air  + 
    log(1+exp(b_tt_car*time_car   - b_tt_air*time_air)) + 
    log(1+exp(b_tt_bus*time_bus   - b_tt_air*time_air)) + 
    log(1+exp(b_tt_rail*time_rail - b_tt_air*time_air)) + 
    log(1+exp(b_cost*(cost_car  - cost_air))) + 
    log(1+exp(b_cost*(cost_bus  - cost_air))) + 
    log(1+exp(b_cost*(cost_rail - cost_air))) + 
    log(1+exp(b_access*(access_car - access_air))) + 
    log(1+exp(b_access*(access_bus - access_air))) + 
    log(1+exp(b_access*(access_rail- access_air))) +
    log(1+exp(RFrills_car  - RFrills_air))  + 
    log(1+exp(RFrills_bus  - RFrills_air))  + 
    log(1+exp(RFrills_rail - RFrills_air))
  R[['rail']] = asc_rail  + 
    log(1+exp(b_tt_car*time_car - b_tt_rail*time_rail)) + 
    log(1+exp(b_tt_bus*time_bus - b_tt_rail*time_rail)) + 
    log(1+exp(b_tt_air*time_air - b_tt_rail*time_rail)) + 
    log(1+exp(b_cost*(cost_car - cost_rail))) + 
    log(1+exp(b_cost*(cost_bus - cost_rail))) + 
    log(1+exp(b_cost*(cost_air - cost_rail))) + 
    log(1+exp(b_access*(access_car - access_rail))) + 
    log(1+exp(b_access*(access_bus - access_rail))) + 
    log(1+exp(b_access*(access_air - access_rail))) +
    log(1+exp(RFrills_car - RFrills_rail))  + 
    log(1+exp(RFrills_bus - RFrills_rail))  + 
    log(1+exp(RFrills_air - RFrills_rail))
  
  ### Define settings for RRM model, which is MNL with negative regret as utility
  mnl_settings <- list(
    alternatives = c(car=1, bus=2, air=3, rail=4),
    avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
    choiceVar    = choice,
    V            = lapply(R, "*", -1)
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
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
