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
  modelName ="Apollo_example_16",
  modelDescr ="Mixed logit model on Swiss route choice data, WTP space with correlated and flexible distributions, inter and intra-individual heterogeneity",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 3
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
                mu_log_b_tc               =-3,
                sigma_log_b_tc_inter      = 0,
                mu_log_v_tt               =-3,
                sigma_log_v_tt_inter      = 0,
                sigma_log_v_tt_inter_2    = 0,
                sigma_log_v_tt_intra      = 0,
                mu_log_v_hw               =-3,
                sigma_log_v_hw_inter      = 0,
                sigma_log_v_hw_v_tt_inter = 0,
                v_ch                      = 0,
                gamma_vtt_business        = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_2")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interUnifDraws = c("draws_tc_inter"),
  interNormDraws = c("draws_hw_inter","draws_tt_inter"),
  intraDrawsType = "mlhs",
  intraNDraws    = 100,
  intraUnifDraws = c(),
  intraNormDraws = c("draws_tt_intra")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()

  randcoeff[["b_tc"]] = -exp( mu_log_b_tc
                              + sigma_log_b_tc_inter      * draws_tc_inter )

  randcoeff[["v_tt"]] =  ( exp( mu_log_v_tt
                              + sigma_log_v_tt_inter      * draws_tt_inter
                              + sigma_log_v_tt_inter_2    * draws_tt_inter ^ 2
                              + sigma_log_v_tt_intra      * draws_tt_intra   ) 
                           * ( gamma_vtt_business    * business + ( 1 - business ) ) )

  randcoeff[["v_hw"]] =  exp( mu_log_v_hw
                              + sigma_log_v_hw_inter      * draws_hw_inter
                              + sigma_log_v_hw_v_tt_inter * draws_tt_inter )

  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']] = asc_1 + b_tc*(v_tt*tt1 + tc1 + v_hw*hw1 + v_ch*ch1)
  V[['alt2']] = asc_2 + b_tc*(v_tt*tt2 + tc2 + v_hw*hw2 + v_ch*ch2)

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = list(alt1=1, alt2=1),
    choiceVar     = choice,
    V             = V
  )

  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)

  ### Average across intra-individual draws
  P = apollo_avgIntraDraws(P, apollo_inputs, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional speedTest
#speedTest_settings=list(
#   nDrawsTry = c(50, 75, 100),
#   nCoresTry = 1:3,
#   nRep      = 10
#)

#apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))

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

### Optional: load previously estimated model object
#model = apollo_loadModel(apollo_control$modelName)

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- CONDITIONALS AND UNCONDITIONALS                            ----
# ----------------------------------------------------------------- #

unconditionals <- apollo_unconditionals(model,apollo_probabilities, apollo_inputs)

plot(density(as.vector(unconditionals[["v_tt"]])))

conditionals <- apollo_conditionals(model,apollo_probabilities, apollo_inputs)

mean(unconditionals[["v_tt"]])

sd(unconditionals[["v_tt"]])

summary(conditionals[["v_tt"]])

income_n = apollo_firstRow(database$hh_inc_abs, apollo_inputs)

summary(lm(conditionals[["v_tt"]][,2]~income_n))

write.csv(conditionals,paste(model$apollo_control$modelName,"_conditionals.csv",sep=""))

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()