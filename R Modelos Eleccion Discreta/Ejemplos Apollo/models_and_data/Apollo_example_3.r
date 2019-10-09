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
  modelName  ="Apollo_example_3",
  modelDescr ="MNL model with socio-demographics on mode choice SP data",
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
              asc_bus_shift_female  = 0,
              asc_air_shift_female  = 0,
              asc_rail_shift_female = 0,
              b_tt_car              = 0,
              b_tt_bus              = 0,
              b_tt_air              = 0,
              b_tt_rail             = 0,
              b_tt_shift_business   = 0,
              b_access              = 0,
              b_cost                = 0,
              b_cost_shift_business = 0,
              cost_income_elast     = 0,
              b_no_frills           = 0,
              b_wifi                = 0,
              b_food                = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","b_no_frills")

### Read in starting values for at least some parameters from existing model output file
apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "Apollo_example_1", overwriteFixed=FALSE)

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
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['car']]  = asc_car        + b_tt_car_value  * time_car                        + b_cost_value * cost_car
  V[['bus']]  = asc_bus_value  + b_tt_bus_value  * time_bus  + b_access * access_bus  + b_cost_value * cost_bus 
  V[['air']]  = asc_air_value  + b_tt_air_value  * time_air  + b_access * access_air  + b_cost_value * cost_air   + b_no_frills * ( service_air == 1 )  + b_wifi * ( service_air == 2 )  + b_food * ( service_air == 3 )
  V[['rail']] = asc_rail_value + b_tt_rail_value * time_rail + b_access * access_rail + b_cost_value * cost_rail  + b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(car=1, bus=2, air=3, rail=4),
    avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
    choiceVar    = choice,
    V            = V
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

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST SIMPLE MNL MODEL                           ----
# ----------------------------------------------------------------- #

apollo_lrTest("Apollo_example_2", "Apollo_example_3")
apollo_lrTest("Apollo_example_2", model)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, apollo_probabilities, apollo_inputs)

### Look at summary of the predicted choice probabilities
summary(predictions_base)

### Now imagine the cost for rail increases by 10%
database$cost_rail = 1.1*database$cost_rail

### Rerun predictions with the new data, and save into a separate matrix
predictions_new = apollo_prediction(model, apollo_probabilities, apollo_inputs)

### Look at summary of the predicted choice probabilities
summary(predictions_new)

### Return to original data
database$cost_rail = 1/1.1*database$cost_rail

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