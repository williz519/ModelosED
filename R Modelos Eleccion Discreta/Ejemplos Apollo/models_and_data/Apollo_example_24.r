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
  modelName  = "Apollo_example_24",
  modelDescr = "ICLV model on drug choice data, using ordered measurement model for indicators",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_drugChoiceData.csv",header=TRUE)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_brand_Artemis    = 0, 
                b_brand_Novum    = 0, 
                b_brand_BestValue    = 0, 
                b_brand_Supermarket    = 0, 
                b_brand_PainAway    = 0, 
                b_country_CH  = 0, 
                b_country_DK  = 0, 
                b_country_USA  = 0, 
                b_country_IND  = 0, 
                b_country_RUS  = 0, 
                b_country_BRA  = 0, 
                b_char_standard     = 0, 
                b_char_fast     = 0, 
                b_char_double     = 0, 
                b_risk            = 0, 
                b_price           = 0,  
                lambda            = 1, 
                gamma_reg_user    = 0, 
                gamma_university  = 0, 
                gamma_age_50      = 0, 
                zeta_quality         = 1, 
                zeta_ingredient         = 1, 
                zeta_patent         = 1, 
                zeta_dominance         = 1, 
                tau_quality_1     =-2, 
                tau_quality_2     =-1, 
                tau_quality_3     = 1, 
                tau_quality_4     = 2, 
                tau_ingredients_1 =-2, 
                tau_ingredients_2 =-1, 
                tau_ingredients_3 = 1, 
                tau_ingredients_4 = 2, 
                tau_patent_1      =-2, 
                tau_patent_2      =-1, 
                tau_patent_3      = 1, 
                tau_patent_4      = 2, 
                tau_dominance_1   =-2, 
                tau_dominance_2   =-1, 
                tau_dominance_3   = 1, 
                tau_dominance_4   = 2)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b_brand_Artemis", "b_country_USA", "b_char_standard")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=100,          
  interUnifDraws=c(),      
  interNormDraws=c("eta"), 
  
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_reg_user*regular_user + gamma_university*university_educated + gamma_age_50*over_50 + eta
  
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
  
  ### Likelihood of indicators
  ol_settings1 = list(outcomeOrdered=attitude_quality, 
                      V=zeta_quality*LV, 
                      tau=c(tau_quality_1, tau_quality_2, tau_quality_3, tau_quality_4),
                      rows=(task==1))
  ol_settings2 = list(outcomeOrdered=attitude_ingredients, 
                      V=zeta_ingredient*LV, 
                      tau=c(tau_ingredients_1, tau_ingredients_2, tau_ingredients_3, tau_ingredients_4), 
                      rows=(task==1))
  ol_settings3 = list(outcomeOrdered=attitude_patent, 
                      V=zeta_patent*LV, 
                      tau=c(tau_patent_1, tau_patent_2, tau_patent_3, tau_patent_4), 
                      rows=(task==1))
  ol_settings4 = list(outcomeOrdered=attitude_dominance, 
                      V=zeta_dominance*LV, 
                      tau=c(tau_dominance_1, tau_dominance_2, tau_dominance_3, tau_dominance_4), 
                      rows=(task==1))
  P[["indic_quality"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_ingredients"]] = apollo_ol(ol_settings2, functionality)
  P[["indic_patent"]]      = apollo_ol(ol_settings3, functionality)
  P[["indic_dominance"]]   = apollo_ol(ol_settings4, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']] = ( b_brand_Artemis*(brand_1=="Artemis") + b_brand_Novum*(brand_1=="Novum") 
  		+ b_country_CH*(country_1=="Switzerland") + b_country_DK*(country_1=="Denmark") + b_country_USA*(country_1=="USA") 
  		+ b_char_standard*(char_1=="standard") + b_char_fast*(char_1=="fast acting") + b_char_double*(char_1=="double strength") 
  		+ b_risk*side_effects_1
  		+ b_price*price_1 
  		+ lambda*LV )
  V[['alt2']] = ( b_brand_Artemis*(brand_2=="Artemis") + b_brand_Novum*(brand_2=="Novum") 
  		+ b_country_CH*(country_2=="Switzerland") + b_country_DK*(country_2=="Denmark") + b_country_USA*(country_2=="USA") 
  		+ b_char_standard*(char_2=="standard") + b_char_fast*(char_2=="fast acting") + b_char_double*(char_2=="double strength") 
  		+ b_risk*side_effects_2
  		+ b_price*price_2 
  		+ lambda*LV )
  V[['alt3']] = ( b_brand_BestValue*(brand_3=="BestValue") + b_brand_Supermarket*(brand_3=="Supermarket") + b_brand_PainAway*(brand_3=="PainAway") 
  		+ b_country_USA*(country_3=="USA") + b_country_IND*(country_3=="India") + b_country_RUS*(country_3=="Russia") + b_country_BRA*(country_3=="Brazil") 
  		+ b_char_standard*(char_3=="standard") + b_char_fast*(char_3=="fast acting") 
  		+ b_risk*side_effects_3
  		+ b_price*price_3 )
  V[['alt4']] = ( b_brand_BestValue*(brand_4=="BestValue") + b_brand_Supermarket*(brand_4=="Supermarket") + b_brand_PainAway*(brand_4=="PainAway") 
  		+ b_country_USA*(country_4=="USA") + b_country_IND*(country_4=="India") + b_country_RUS*(country_4=="Russia") + b_country_BRA*(country_4=="Brazil") 
  		+ b_char_standard*(char_4=="standard") + b_char_fast*(char_4=="fast acting") 
  		+ b_risk*side_effects_4
  		+ b_price*price_4 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4),
    avail        = list(alt1=1, alt2=1, alt3=1, alt4=1),
    choiceVar    = best,
    V            = V
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)

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

### Optional: calculate LL before model estimation
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
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
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="indic_quality")

# ----------------------------------------------------------------- #
#---- CONDITIONALS AND UNCONDITIONALS                            ----
# ----------------------------------------------------------------- #

conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)

unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)
                                        
# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()