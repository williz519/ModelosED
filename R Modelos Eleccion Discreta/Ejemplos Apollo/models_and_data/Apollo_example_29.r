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
  modelName ="Apollo_example_29",
  modelDescr ="Mixed logit model on Swiss route choice data, correlated Lognormals in utility space, EM algorithm",
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
apollo_beta = c(mu_log_b_tt               =-3,
                mu_log_b_tc               =-3,
                mu_log_b_hw               =-3,
                mu_log_b_ch               =-3,
                sigma_log_b_tt            = 0.4880,
                sigma_log_b_tt_tc         = 0,
                sigma_log_b_tc            = 1.0369,
                sigma_log_b_tt_hw         = 0,
                sigma_log_b_tc_hw         = 0,
                sigma_log_b_hw            = 0.8141,
                sigma_log_b_tt_ch         = 0,
                sigma_log_b_tc_ch         = -0.3,
                sigma_log_b_hw_ch         = 0,
                sigma_log_b_ch            = -0.8298)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draws_tt","draws_tc","draws_hw","draws_ch"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()

  randcoeff[["b_tt"]] = -exp( mu_log_b_tt  + sigma_log_b_tt    * draws_tt )
  randcoeff[["b_tc"]] = -exp( mu_log_b_tc  + sigma_log_b_tt_tc * draws_tt + sigma_log_b_tc    * draws_tc )
  randcoeff[["b_hw"]] = -exp( mu_log_b_hw  + sigma_log_b_tt_hw * draws_tt + sigma_log_b_tc_hw * draws_tc + sigma_log_b_hw    * draws_hw )
  randcoeff[["b_ch"]] = -exp( mu_log_b_ch  + sigma_log_b_tt_ch * draws_tt + sigma_log_b_tc_ch * draws_tc + sigma_log_b_hw_ch * draws_hw + sigma_log_b_ch * draws_ch )

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
  V[['alt1']] = b_tt * tt1 + b_tc * tc1 + b_hw * hw1 + b_ch * ch1
  V[['alt2']] = b_tt * tt2 + b_tc * tc2 + b_hw * hw2 + b_ch * ch2

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = list(alt1=1, alt2=1),
    choiceVar     = choice,
    V             = V
  )

  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### APPLY EM ALGORITHM                                          ####
# ################################################################# #

### Create temporary model object
model=list()

### Calculate initial likelihood at the level of each draw
Ln=apollo_probabilities(apollo_beta, apollo_inputs, functionality="conditionals")
cat("Initial LL:",sum(log(rowMeans(Ln))),"\n")

### Loop over repeated EM iterations until convergence has been reached
stopping_criterion=10^-5
iteration=1
stop=0

while(stop==0){
  cat("Starting iteration: ",iteration,"\n",sep="")
  cat("Current LL: ",sum(log(rowMeans(Ln))),"\n",sep="")

  ### Calculate weight for each individual and for each draw 
  wn=Ln/(rowMeans(Ln))
  
  ### Copy current parameter values into temporary model object
  model$estimate=apollo_beta
  
  ### Produce draws for random coefficients with current vector of parameters
  d=apollo_unconditionals(model,apollo_probabilities,apollo_inputs)
  
  ### Translate draws back to Normal from negative Lognormal
  d=lapply(d,function(x) log(-x))
  
  ### Apply weights to individual draws and turn into a matrix
  dwn=lapply(d,"*",wn)
  dwn=lapply(dwn,as.vector)
  dwn=do.call(cbind,dwn)
  
  ### Calculate means for weighted draws
  mu=colMeans(dwn)
  
  ### Calculate weighted covariance matrix
  K = length(mu)   # n of coefficients
  R = ncol(d[[1]]) # n of draws
  N = nrow(d[[1]]) # n of individuals
  tmp=matrix(0, nrow=N, ncol=K)
  Omega=matrix(0, nrow=K, ncol=K)
  for(r in 1:R){
    for(k in 1:K) tmp[,k] = d[[k]][,r] - mu[k]
    for(n in 1:N) Omega = Omega + wn[n,r]*(tmp[n,] %*% t(tmp[n,]))
  } 
  
  ### Compute Cholesky of average weighted covariance matrix
  cholesky = chol(Omega/(N*R))
  cholesky = cholesky[upper.tri((cholesky),diag=TRUE)]
  
  ### Update vector of model parameters on the basis of calculated mu and Omega
  apollo_beta[1:4]=mu
  apollo_beta[5:14]=cholesky
  
  ### Calculate likelihood with new parameters
  Lnew=((apollo_probabilities(apollo_beta, apollo_inputs, functionality="conditionals")))
  
  ### Compute improvement
  change=sum(log(rowMeans(Lnew)))-sum(log(rowMeans(Ln)))
  cat("New LL: ",sum(log(rowMeans(Lnew))),"\n",sep="")
  cat("Improvement: ",change,"\n\n",sep="")
  Ln=Lnew
  
  ### Determine whether convergence has been reached
  if(change<stopping_criterion) stop=1
  iteration=iteration+1
}

# ################################################################# #
#### CLASSICAL ESTIMATION FOR COVARIANCE MATRIX                  ####
# ################################################################# #

### Reinstate original vector of fixed parameters
apollo_fixed=apollo_fixed_base

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
                        estimate_settings=list(hessianRoutine="maxLik",maxIterations=0))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)