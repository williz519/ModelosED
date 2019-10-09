# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

apollo_control = list(
  modelName  ="Apollo_example_20",
  modelDescr ="LC model with class allocation model on Swiss route choice data",
  indivID    ="ID",
  nCores     = 1
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
                beta_hw_a       =-0.0396,
                beta_hw_b       =-0.0479,
                beta_ch_a       =-0.7624,
                beta_ch_b       =-2.1725,
                delta_a         = 0.0329,
                gamma_commute_a = 0,
                gamma_car_av_a  = 0,
                delta_b         = 0,
                gamma_commute_b = 0,
                gamma_car_av_b  = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_2","delta_b","gamma_commute_b","gamma_car_av_b")

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_tt"]] = list(beta_tt_a, beta_tt_b)
  lcpars[["beta_tc"]] = list(beta_tc_a, beta_tc_b)
  lcpars[["beta_hw"]] = list(beta_hw_a, beta_hw_b)
  lcpars[["beta_ch"]] = list(beta_ch_a, beta_ch_b)
  
  V=list()
  V[["class_a"]] = delta_a + gamma_commute_a*commute + gamma_car_av_a*car_availability
  V[["class_b"]] = delta_b + gamma_commute_b*commute + gamma_car_av_b*car_availability
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
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
  while(s<=2){
    
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

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs,
                        estimate_settings=list(writeIter=FALSE))

### Show output in screen
apollo_modelOutput(model)

### Save output to file(s)
apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- OUT OF SAMPLE TESTING                                     ----
# ----------------------------------------------------------------- #

apollo_outOfSample(apollo_beta, apollo_fixed,
                   apollo_probabilities, apollo_inputs)

# ----------------------------------------------------------------- #
#---- POSTERIOR ANALYSIS                                     ----
# ----------------------------------------------------------------- #

### Compute unconditionals
unconditionals = apollo_lcUnconditionals(model,apollo_probabilities,apollo_inputs)

vtt_class_a = unconditionals[["beta_tt"]][[1]]/unconditionals[["beta_tc"]][[1]]
vtt_class_b = unconditionals[["beta_tt"]][[2]]/unconditionals[["beta_tc"]][[2]]
vtt_unconditional = unconditionals[["pi_values"]][[1]]*vtt_class_a + 
                    unconditionals[["pi_values"]][[2]]*vtt_class_b

### Compute conditionals
conditionals = apollo_lcConditionals(model,apollo_probabilities, apollo_inputs)

summary(conditionals)
summary(as.data.frame(unconditionals[["pi_values"]]))

vtt_conditional=conditionals[,1]*vtt_class_a+conditionals[,2]*vtt_class_b

summary(vtt_unconditional)
summary(vtt_conditional)

### Take first value of covariates for each person
commute_n          = apollo_firstRow(database$commute, apollo_inputs)
car_availability_n = apollo_firstRow(database$car_availability, apollo_inputs)

### Compute posterior values for covariates
post_commute=colSums(commute_n*conditionals)/colSums(conditionals)
post_car_availability=colSums(car_availability_n*conditionals)/colSums(conditionals)

post_commute
post_car_availability

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()