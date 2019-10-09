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
  modelName  ="Apollo_example_17",
  modelDescr ="Mixed MDCEV model on time use data, alpha-gamma profile, no outside good and random constants only in utilities",
  indivID    ="indivID",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_timeUseData.csv",header=TRUE)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(alpha_base           = 0,
                gamma_dropOff        = 1,
                gamma_work           = 1,
                gamma_school         = 1,
                gamma_shopping       = 1,
                gamma_privBusiness       = 1,
                gamma_petrol         = 1,
                gamma_leisure        = 1,
                gamma_vacation       = 1,
                gamma_exercise       = 1,
                gamma_home           = 1,
                gamma_travel         = 1,
                gamma_other       = 1,
                delta_dropOff_mu     = 0,
                delta_work_mu        = 0,
                delta_school_mu      = 0,
                delta_shopping_mu    = 0,
                delta_privBusiness_mu    = 0,
                delta_petrol_mu      = 0,
                delta_leisure_mu     = 0,
                delta_vacation_mu    = 0,
                delta_exercise_mu    = 0,
                delta_home_mu        = 0,
                delta_travel_mu      = 0,
                delta_other_mu    = 0,
                delta_dropOff_sigma  = 0,
                delta_work_sigma     = 0,
                delta_school_sigma   = 0,
                delta_shopping_sigma = 0,
                delta_privBusiness_sigma = 0,
                delta_petrol_sigma   = 0,
                delta_leisure_sigma  = 0,
                delta_vacation_sigma = 0,
                delta_exercise_sigma = 0,
                delta_home_sigma     = 0,
                delta_travel_sigma   = 0,
                delta_other_sigma = 0,
                sigma                = 1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_home_mu", "delta_home_sigma", "sigma")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = paste0("draws_", c("dropOff", "work", 
                                      "school", "shopping", 
                                      "privBusiness", "petrol", 
                                      "leisure", "vacation", 
                                      "exercise", "home", 
                                      "travel", "other")),
  intraDrawsType = "",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["delta_dropOff"]]  = delta_dropOff_mu  + delta_dropOff_sigma *draws_dropOff
  randcoeff[["delta_work"]]     = delta_work_mu     + delta_work_sigma    *draws_work 
  randcoeff[["delta_school"]]   = delta_school_mu   + delta_school_sigma  *draws_school
  randcoeff[["delta_shopping"]] = delta_shopping_mu + delta_shopping_sigma*draws_shopping
  randcoeff[["delta_privBusiness"]] = delta_privBusiness_mu + delta_privBusiness_sigma*draws_privBusiness
  randcoeff[["delta_petrol"]]   = delta_petrol_mu   + delta_petrol_sigma  *draws_petrol
  randcoeff[["delta_leisure"]]  = delta_leisure_mu  + delta_leisure_sigma *draws_leisure
  randcoeff[["delta_vacation"]] = delta_vacation_mu + delta_vacation_sigma*draws_vacation
  randcoeff[["delta_exercise"]] = delta_exercise_mu + delta_exercise_sigma*draws_exercise
  randcoeff[["delta_home"]]     = delta_home_mu     + delta_home_sigma    *draws_home 
  randcoeff[["delta_travel"]]   = delta_travel_mu   + delta_travel_sigma  *draws_travel
  randcoeff[["delta_other"]] = delta_other_mu + delta_other_sigma*draws_other
  
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
    
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  
  ### Define individual alternatives
  alternatives = c("dropOff", 
                   "work", 
                   "school", 
                   "shopping", 
                   "privBusiness", 
                   "petrol", 
                   "leisure", 
                   "vacation", 
                   "exercise", 
                   "home", 
                   "travel", 
                   "other")
  
  ### Define availabilities
  avail = list(dropOff  = 1, 
               work     = 1,
               school   = 1,
               shopping = 1,
               privBusiness = 1,
               petrol   = 1, 
               leisure  = 1,
               vacation = 1,
               exercise = 1,
               home     = 1,
               travel   = 1,
               other = 1)

  ### Define continuous consumption for individual alternatives
  continuousChoice = list(dropOff  = t_a01/60, 
                          work     = t_a02/60,
                          school   = t_a03/60,
                          shopping = t_a04/60,
                          privBusiness = t_a05/60,
                          petrol   = t_a06/60, 
                          leisure  = t_a07/60,
                          vacation = t_a08/60,
                          exercise = t_a09/60,
                          home     = t_a10/60,
                          travel   = t_a11/60,
                          other = t_a12/60)
  
  ### Define utilities for individual alternatives
  V = list()
  V[["dropOff" ]] = delta_dropOff 
  V[["work"    ]] = delta_work    
  V[["school"  ]] = delta_school  
  V[["shopping"]] = delta_shopping
  V[["privBusiness"]] = delta_privBusiness
  V[["petrol"  ]] = delta_petrol  
  V[["leisure" ]] = delta_leisure 
  V[["vacation"]] = delta_vacation
  V[["exercise"]] = delta_exercise
  V[["home"    ]] = delta_home    
  V[["travel"  ]] = delta_travel  
  V[["other"]] = delta_other
  
  ### Define alpha parameters
  alpha = list(dropOff  = 1 /(1 + exp(-alpha_base)), 
               work     = 1 /(1 + exp(-alpha_base)), 
               school   = 1 /(1 + exp(-alpha_base)), 
               shopping = 1 /(1 + exp(-alpha_base)), 
               privBusiness = 1 /(1 + exp(-alpha_base)),
               petrol   = 1 /(1 + exp(-alpha_base)),
               leisure  = 1 /(1 + exp(-alpha_base)), 
               vacation = 1 /(1 + exp(-alpha_base)), 
               exercise = 1 /(1 + exp(-alpha_base)), 
               home     = 1 /(1 + exp(-alpha_base)), 
               travel   = 1 /(1 + exp(-alpha_base)),
               other = 1 /(1 + exp(-alpha_base)))

  ### Define gamma parameters
  gamma = list(dropOff  = gamma_dropOff, 
               work     = gamma_work,
               school   = gamma_school,
               shopping = gamma_shopping,
               privBusiness = gamma_privBusiness,
               petrol   = gamma_petrol, 
               leisure  = gamma_leisure,
               vacation = gamma_vacation,
               exercise = gamma_exercise,
               home     = gamma_home,
               travel   = gamma_travel,
               other = gamma_other)
  
  ### Define costs for individual alternatives
  cost = list(dropOff  = 1, 
              work     = 1,
              school   = 1,
              shopping = 1,
              privBusiness = 1,
              petrol   = 1, 
              leisure  = 1,
              vacation = 1,
              exercise = 1,
              home     = 1,
              travel   = 1,
              other = 1)

  ### Define budget
  budget = budget/60
  
  ### Define settings for MDCEV model
  mdcev_settings <- list(alternatives      = alternatives,
                         avail             = avail,
                         continuousChoice  = continuousChoice,
                         V                 = V,
                         alpha             = alpha,
                         gamma             = gamma, 
                         sigma             = sigma, 
                         cost              = cost,
                         budget            = budget)
  
  ### Compute probabilities using MDCEV model
  P[["model"]] = apollo_mdcev(mdcev_settings, functionality)
  
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
