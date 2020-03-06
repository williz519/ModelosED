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
  modelName  ="Apollo_example_19",
  modelDescr ="Simple DM model on Swiss route choice data",
  indivID    ="ID",
  nCores     = 3,
  noDiagnostics = TRUE,
  noValidation = TRUE
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_swissRouteChoiceData.csv",header=TRUE)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_tt_a       = 0,
                beta_tt_b       = 0,
                beta_tc_a       = 0,
                beta_tc_b       = 0,
                beta_hw_a       = -0.0396,
                beta_hw_b       = -0.0479,
                beta_ch_a       = -0.7624,
                beta_ch_b       = -2.1725,
                delta_tt_a      = 0,
                delta_tc_a      = 0,
                delta_hw_a      = 0,
                delta_ch_a      = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_2")

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()

  ### Create empty lists for parameters in classes and class allocation probabilities
  lcpars[["beta_tt"]] = list()
  lcpars[["beta_tc"]] = list()
  lcpars[["beta_hw"]] = list()
  lcpars[["beta_ch"]] = list()
  lcpars[["pi_values"]] = list()
  
  ### Generic settings for class allocation models
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = list()
  )

  ### Create class allocation probabilities at level of individual attributes
  mnl_settings$V[["class_a"]] = delta_tt_a
  mnl_settings$V[["class_b"]] = 0
  ttProbs=apollo_mnl(mnl_settings, functionality="raw")
  mnl_settings$V[["class_a"]] = delta_tc_a
  tcProbs=apollo_mnl(mnl_settings, functionality="raw")
  mnl_settings$V[["class_a"]] = delta_hw_a
  hwProbs=apollo_mnl(mnl_settings, functionality="raw")
  mnl_settings$V[["class_a"]] = delta_ch_a
  chProbs=apollo_mnl(mnl_settings, functionality="raw")
  
  ### Loop over combinations, determining parameter values and class allocation probabilities (multiplicatively)
  s=1
  while(s<=16){
    if(s<=8){
     lcpars[["beta_tt"]][[s]]=beta_tt_a
     lcpars[["pi_values"]][[s]]=ttProbs[[1]]
    } else {
      lcpars[["beta_tt"]][[s]]=beta_tt_b
      lcpars[["pi_values"]][[s]]=ttProbs[[2]]
    }
    if((s-(s>8)*8)<=4){
      lcpars[["beta_tc"]][[s]]=beta_tc_a
      lcpars[["pi_values"]][[s]]=lcpars[["pi_values"]][[s]]*tcProbs[[1]]
    } else {
      lcpars[["beta_tc"]][[s]]=beta_tc_b
      lcpars[["pi_values"]][[s]]=lcpars[["pi_values"]][[s]]*tcProbs[[2]]
    }
    if((ceiling(s/2)%%2)!=0){
      lcpars[["beta_hw"]][[s]]=beta_hw_a
      lcpars[["pi_values"]][[s]]=lcpars[["pi_values"]][[s]]*hwProbs[[1]]
    } else {
      lcpars[["beta_hw"]][[s]]=beta_hw_b
      lcpars[["pi_values"]][[s]]=lcpars[["pi_values"]][[s]]*hwProbs[[2]]
    }
    if(s%%2!=0){
      lcpars[["beta_ch"]][[s]]=beta_ch_a
      lcpars[["pi_values"]][[s]]=lcpars[["pi_values"]][[s]]*chProbs[[1]]
    } else {
      lcpars[["beta_ch"]][[s]]=beta_ch_b
      lcpars[["pi_values"]][[s]]=lcpars[["pi_values"]][[s]]*chProbs[[2]]
    }
    s=s+1
    }
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

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
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = choice
  )
  
  ### Loop over classes
  s=1
  while(s<=16){
    
    ### Compute class-specific utilities
    V=list()
    V[['alt1']]  = asc_1 + beta_tc[[s]]*tc1 + beta_tt[[s]]*tt1 + beta_hw[[s]]*hw1 + beta_ch[[s]]*ch1
    V[['alt2']]  = asc_2 + beta_tc[[s]]*tc2 + beta_tt[[s]]*tt2 + beta_hw[[s]]*hw2 + beta_ch[[s]]*ch2
    
    mnl_settings$V = V
    
    ### Compute within-class choice probabilities using MNL model
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional starting values search
# apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

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

