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
  modelName  ="Apollo_example_27",
  modelDescr ="Simple LC model on Swiss route choice data, EM algorithm",
  indivID    ="ID",
  nCores     = 3,
  weights    = "weights",
  noValidation = TRUE,
  noDiagnostics = TRUE
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_swissRouteChoiceData.csv",header=TRUE)
database$weights = 1

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
                pi_a            = 0.5)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_2")

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_tt"]] = list(beta_tt_a, beta_tt_b)
  lcpars[["beta_tc"]] = list(beta_tc_a, beta_tc_b)
  lcpars[["beta_hw"]] = list(beta_hw_a, beta_hw_b)
  lcpars[["beta_ch"]] = list(beta_ch_a, beta_ch_b)
  
  lcpars[["pi_values"]] = list(pi_a,1-pi_a)
  
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
    
    s=s+1
  }
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
   
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION FOR WITHIN CLASS       ####
# ################################################################# #

apollo_probabilities_within_class=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  
  ### Determine which class we're working in
  s=apollo_inputs$s
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V=list()
  V[['alt1']]  = asc_1 + beta_tc[[s]]*tc1 + beta_tt[[s]]*tt1 + beta_hw[[s]]*hw1 + beta_ch[[s]]*ch1
  V[['alt2']]  = asc_2 + beta_tc[[s]]*tc2 + beta_tt[[s]]*tt2 + beta_hw[[s]]*hw2 + beta_ch[[s]]*ch2
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = choice,
    V            = V)
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Apply weights
  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### APPLY EM ALGORITHM                                          ####
# ################################################################# #

### Keep backup of vector of fixed parameters as this changes throughout
apollo_fixed_base = apollo_fixed

# ########## #
### Step 1 ###
# ########## #

### Initialise class allocation probabilities
apollo_beta["pi_a"]=0.5

### Loop over repeated EM iterations until convergence has been reached
stopping_criterion=10^-5
iteration=1
stop=0
while(stop==0){
  cat("Starting iteration: ",iteration,"\n",sep="")
  
  # ########## #
  ### Step 2 ###
  # ########## #
  
  ### Calculate model likelihood and class specific likelihoods
  L=apollo_probabilities(apollo_beta, apollo_inputs, functionality="output")
  
  ### Calculate class specific conditional likelihoods
  h1=as.vector(apollo_beta["pi_a"]*L[[1]]/L[[3]])
  h2=as.vector((1-apollo_beta["pi_a"])*L[[2]]/L[[3]])
  
  ### Calculate current log-likelihood for LC model
  Lcurrent=sum(log(L[[3]]))
  cat("Current LL: ",Lcurrent,"\n",sep="")
  
  # ########## #
  ### Step 3 ###
  # ########## #
  
  
  ### Update shares in classes
  apollo_beta["pi_a"]=sum(h1)/(sum(h1)+sum(h2))
  
  # ########## #
  ### Step 4 ###
  # ########## #
  
  ### Update coefficients in class specific models by estimating class specific models 
  ### using posterior class allocation probabilities as weights
  
  ### Class 1
  
  ### Replicate individual-specific weights for each observation 
  nObsPerIndiv <- as.vector(table(database[,apollo_control$indivID]))
  apollo_inputs$database$weights=rep(h1,times=nObsPerIndiv)
  
  ### Set fixed parameters (only estimating parameters for class 1)
  apollo_fixed=c("asc_2","beta_tt_b","beta_tc_b","beta_hw_b","beta_ch_b","pi_a")
  
  ### Set class index to use inside apollo_probabilities_within_class
  apollo_inputs$s=1
  
  ### Estimate class-specific weighted MNL model
  model = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities_within_class, apollo_inputs,
                          estimate_settings=list(writeIter=FALSE,silent=TRUE,hessianRoutine="none"))
  
  ### Update overall parameters
  apollo_beta=model$estimate
  
  ### Class 2
  
  ### Replicate individual-specific weights for each observation 
  nObsPerIndiv <- as.vector(table(database[,apollo_control$indivID]))
  apollo_inputs$database$weights=rep(h2,times=nObsPerIndiv)
  
  ### Set fixed parameters (only estimating parameters for class 2)
  apollo_fixed=c("asc_2","beta_tt_a","beta_tc_a","beta_hw_a","beta_ch_a","pi_a")
  
  ### Set class index to use inside apollo_probabilities_within_class
  apollo_inputs$s=2
  
  ### Estimate class-specific weighted MNL model
  model = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities_within_class, apollo_inputs,
                          estimate_settings=list(writeIter=FALSE,silent=TRUE,hessianRoutine="none"))
  
  ### Update overall parameters
  apollo_beta=model$estimate
  
  # ########## #
  ### Step 5 ###
  # ########## #
  
  ### Calculate new log-likelihood and compute improvement
  Lnew=sum(log(apollo_probabilities(apollo_beta, apollo_inputs, functionality="output")[[3]]))
  change=Lnew-Lcurrent
  cat("New LL: ",Lnew,"\n",sep="")
  cat("Improvement: ",change,"\n\n",sep="")
  
  ### Determine whether convergence has been reached
  if(change<stopping_criterion) stop=1
  iteration=iteration+1
}

# ################################################################# #
#### CLASSICAL ESTIMATION FOR COVARIANCE MATRIX                  ####
# ################################################################# #

### Reinstate original vector of fixed parameters
apollo_fixed=apollo_fixed_base

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=0))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
