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
  modelName  ="Apollo_example_8",
  modelDescr ="DFT model on Swiss choice SP data",
  indivID    ="ID",
  nCores    = 1
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_swissRouteChoiceData.csv",header=TRUE)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_1                     = 0,
                asc_2                     = 0,
                b_tc                      = 0,
                b_tt                      = 0,
                b_hw                      = 0,
                b_ch                      = 0,
                p_phi1                    = 0,
                p_phi2                    = 0,
                p_error_sd                = 1,
                p_timesteps               = 1)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("p_phi1","p_phi2","b_tt","asc_2")

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

  ### List of attribute values
  attrValues = list()
  attrValues[['alt1']]  = list(time=-tt1, cost=-tc1, headway = -hw1, changes=-ch1)
  attrValues[['alt2']]  = list(time=-tt2, cost=-tc2, headway = -hw2, changes=-ch2)
  
  ### List of initial preference values
  altStart = list()
  altStart[['alt1']]  = asc_1
  altStart[['alt2']]  = asc_2
  
  ### List of attribute weights
  attrWeights = list(time      = exp(b_tt)/(exp(b_tt)+exp(b_tc)+exp(b_hw)+exp(b_ch)),
                     cost      = exp(b_tc)/(exp(b_tt)+exp(b_tc)+exp(b_hw)+exp(b_ch)),
                     headway   = exp(b_hw)/(exp(b_tt)+exp(b_tc)+exp(b_hw)+exp(b_ch)),
                     changes   = exp(b_ch)/(exp(b_tt)+exp(b_tc)+exp(b_hw)+exp(b_ch)))
  
  ### List of process parameters
  procPars  = list(
    error_sd=p_error_sd,
    timesteps=1+exp(p_timesteps),
    phi1=p_phi1,
    phi2=p_phi2
  )
  
  ### Define settings for DFT model component
  dft_settings <- list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1,alt2=1),
    choiceVar    = choice,
    attrValues   = attrValues,
    altStart     = altStart,
    attrWeights  = attrWeights,           
    attrScalings = 1,           ### Using weights, so attrScalings must be set to 1.
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