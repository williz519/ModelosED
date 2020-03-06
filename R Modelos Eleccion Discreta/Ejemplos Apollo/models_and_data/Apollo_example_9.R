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
  modelName  ="Apollo_example_9",
  modelDescr ="DFT model on mode choice SP data",
  indivID    ="ID",
  nCores    = 1
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
              asc_bus_shift_female  = 0,
              asc_air_shift_female  = 0,
              asc_rail_shift_female = 0,
              b_tt_car              = -0.01,
              b_tt_bus              = -0.01,
              b_tt_air              = -0.01,
              b_tt_rail             = -0.01,
              b_tt_shift_business   = 0,
              b_acc                 = -0.01,
              b_cost                = -0.01,
              b_cost_shift_business = 0,
              cost_income_elast     = 0,
              b_no_frills           = 0,
              b_wifi                = -0.01,
              b_food                = -0.01,
              p_error_sd            = 1,
              p_timesteps           = 1,
              p_phi1                = -2,
              p_phi2                = -2)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","b_no_frills","p_error_sd")

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
  
  ### List of attribute values
  attrValues = list()
  attrValues[['car']]  = list(time=time_car,  access=0          ,cost=cost_car ,wifi=0                    ,food=0                    )
  attrValues[['bus']]  = list(time=time_bus,  access=access_bus ,cost=cost_bus ,wifi=0                    ,food=0                    )
  attrValues[['air']]  = list(time=time_air,  access=access_air ,cost=cost_air ,wifi=1*(service_air  == 2),food=1*(service_air ==  3))
  attrValues[['rail']] = list(time=time_rail, access=access_rail,cost=cost_rail,wifi=1*(service_rail == 2),food=1*(service_rail == 3))
  
  ### List of initial preference values
  altStart = list()
  altStart[['car']]  = asc_car
  altStart[['bus']]  = asc_bus_value
  altStart[['air']]  = asc_air_value
  altStart[['rail']] = asc_rail_value
  
  ### List of attribute scaling factors
  attrScalings = list(time      = list(car = b_tt_car_value, bus = b_tt_bus_value, air = b_tt_air_value, rail = b_tt_rail_value),
                      access    = b_acc,
                      cost      = b_cost_value,
                      wifi      = b_wifi,
                      food      = b_food)
  
  ### List of process parameters
  procPars  = list(
    error_sd=p_error_sd,
    timesteps=1+exp(p_timesteps),
    phi1=exp(p_phi1),
    phi2=exp(p_phi2)/(1+exp(p_phi2))
  )
  
  ### Define settings for DFT model component
  dft_settings <- list(
    alternatives = c(car=1, bus=2, air=3, rail=4),
    avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
    choiceVar    = choice,
    attrValues   = attrValues,
    altStart     = altStart,
    attrWeights  = 1,            ### Using scaling factors, so attrWeights must be set to 1.
    attrScalings = attrScalings,
    procPars     = procPars
  )
  
  ### Compute choice probabilities using DFT model
  P[['model']] = apollo_dft(dft_settings, functionality)

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