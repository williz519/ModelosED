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
  modelName  ="Apollo_example_22",
  modelDescr ="RP-SP model on mode choice data",
  indivID    ="ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_modeChoiceData.csv",header=TRUE)

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
              mu_RP                   = 1,
              mu_SP                   = 1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","b_no_frills","mu_RP")

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

  ### List of utilities (before applying scales): these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['car']]  = asc_car        + b_tt_car_value  * time_car                        + b_cost_value * cost_car
  V[['bus']]  = asc_bus_value  + b_tt_bus_value  * time_bus  + b_access * access_bus  + b_cost_value * cost_bus 
  V[['air']]  = asc_air_value  + b_tt_air_value  * time_air  + b_access * access_air  + b_cost_value * cost_air   + b_no_frills * ( service_air == 1 )  + b_wifi * ( service_air == 2 )  + b_food * ( service_air == 3 )
  V[['rail']] = asc_rail_value + b_tt_rail_value * time_rail + b_access * access_rail + b_cost_value * cost_rail  + b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )

  ### Compute probabilities for the RP part of the data using MNL model
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
    choiceVar     = choice, 
    V             = lapply(V, "*", mu_RP),
    rows          = (RP==1)
  )
  
  P[['RP']] = apollo_mnl(mnl_settings, functionality)

  ### Compute probabilities for the SP part of the data using MNL model
  mnl_settings$V = lapply(V, "*", mu_SP)
  mnl_settings$rows = (SP==1)
  
  P[['SP']] = apollo_mnl(mnl_settings, functionality)

  ### Combined model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
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
#---- MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #

### RP elasticities

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs,
                                     modelComponent = "RP")

### Look at a summary of the predicted choice probabilities
summary(predictions_base)

### Now imagine the cost for rail increases by 10%
database$cost_rail = 1.1*database$cost_rail

### Rerun predictions with the new data, and save into a separate matrix
predictions_new = apollo_prediction(model, 
                                    apollo_probabilities, 
                                    apollo_inputs,
                                    modelComponent = "RP")

### Look at a summary of the predicted choice probabilities
summary(predictions_new)

### Return to original data
database$cost_rail = 1/1.1*database$cost_rail

### Compute own elasticity for rail:
log(sum(predictions_new[,4],na.rm=TRUE)/sum(predictions_base[,4],na.rm=TRUE))/log(1.1)

### SP elasticities

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs,
                                     modelComponent = "SP")

### Look at a summary of the predicted choice probabilities
summary(predictions_base)

### Now imagine the cost for rail increases by 10%
database$cost_rail = 1.1*database$cost_rail

### Rerun predictions with the new data, and save into a separate matrix
predictions_new = apollo_prediction(model, 
                                    apollo_probabilities, 
                                    apollo_inputs,
                                    modelComponent = "SP")

### Look at a summary of the predicted choice probabilities
summary(predictions_new)

### Return to original data
database$cost_rail = 1/1.1*database$cost_rail

### Compute own elasticity for rail:
log(sum(predictions_new[,4],na.rm=TRUE)/sum(predictions_base[,4],na.rm=TRUE))/log(1.1)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()