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
  modelName  ="Apollo_example_21",
  modelDescr ="Latent class with continuous random parameters on Swiss route choice data",
  mixing     = TRUE, 
  indivID    ="ID",
  nCores     = 3
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_swissRouteChoiceData.csv",header=TRUE)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc1            =  0,
                asc2            =  0,
                log_tt_a_mu     =  -3,
                log_tt_b_mu     =  -3,
                log_tt_a_sig    =  0.2,
                log_tt_b_sig    =  0.1,
                tc_a            =  0,
                tc_b            =  0,
                hw_a            = -0.0396,
                hw_b            = -0.0479,
                ch_a            = -0.7624,
                ch_b            = -2.1725,
                delta_a_mu      =  0.0329,
                delta_a_sig     =  0.0329,
                gamma_commute_a =  0,
                gamma_car_av_a  =  0,
                delta_b         =  0,
                gamma_commute_b =  0,
                gamma_car_av_b  =  0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc2", "delta_b", "gamma_commute_b","gamma_car_av_b")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton",           
  interNDraws=100,                   
  interUnifDraws=c(),                
  interNormDraws=c("draws_tt","draws_pi"),    
  
  intraDrawsType="mlhs",
  intraNDraws=0,        
  intraUnifDraws=c(),   
  intraNormDraws=c()    
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["tt_a"]] = -exp(log_tt_a_mu + log_tt_a_sig*draws_tt)
  randcoeff[["tt_b"]] = -exp(log_tt_b_mu + log_tt_b_sig*draws_tt)
  randcoeff[["delta_a"]] = delta_a_mu  + delta_a_sig*draws_pi
  
  return(randcoeff)
}

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars = function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["tt"]] = list(tt_a, tt_b)
  lcpars[["tc"]] = list(tc_a, tc_b)
  lcpars[["hw"]] = list(hw_a, hw_b)
  lcpars[["ch"]] = list(ch_a, ch_b)
  
  V=list()
  V[["class_a"]] = delta_a     + gamma_commute_a*commute + gamma_car_av_a*car_availability
  V[["class_b"]] = delta_b     + gamma_commute_b*commute + gamma_car_av_b*car_availability
  
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
    V[['alt1']]  = asc1 + tc[[s]]*tc1 + tt[[s]]*tt1 + hw[[s]]*hw1 + ch[[s]]*ch1
    V[['alt2']]  = asc2 + tc[[s]]*tc2 + tt[[s]]*tt2 + hw[[s]]*hw2 + ch[[s]]*ch2
    
    mnl_settings$V = V
    
    ### Compute within-class choice probabilities using MNL model
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    
    ### Average across inter-individual draws within classes
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    s=s+1
  }
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)

  ### Average across inter-individual draws in class allocation probabilities
  P[["model"]] = apollo_avgInterDraws(P[["model"]], apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))

### Show output in screen
apollo_modelOutput(model)

### Save output to file(s)
apollo_saveOutput(model)

