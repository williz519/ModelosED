#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 18 15:52:01 2019

@author: williz
"""

# Cargar Paquetes

import pandas as pd
import numpy as np
import biogeme.database as db
import biogeme.biogeme as bio
from biogeme.models import piecewise
import biogeme.loglikelihood as ll


#Cargar DataBase

pandas = pd.read_csv('/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL1.csv', sep='\t')
database = db.Database("DBModLogitVL1",pandas)

from headers import *

exclude = (CHOICE == -1.0)
database.remove(exclude)


#Variables
JOVEN = DefineVariable('JOVEN', JOVEN, database)
ADULTO = DefineVariable('ADULTO', ADULTO, database)
MAYOR_60 = DefineVariable('MAYOR_60', MAYOR_60, database)
EDUC_BASICA = DefineVariable('EDUC_BASICA', EDUBASICA, database)
EDUC_SUPERIOR = DefineVariable('EDUC_SUPERIOR', EDUSUP, database)
TPROF = DefineVariable('TPROF', TIEMPO_PROFESION, database)
HORASTRAB = DefineVariable('HORASTRAB', HORASTRABAJO, database)
HPICO = DefineVariable('HPICO', HPICOHVALLE, database)
HVALLE = DefineVariable('HVALLE', HVALLE, database)
CSECO = DefineVariable('CSECO', CSECO, database)
CLLUVIA = DefineVariable('CLLUVIA', CLLUVIA, database)
CONG_AB = DefineVariable('CONG_AB', CONG_AB, database)
CONG_CD = DefineVariable('CONG_CD', CONG_CD, database)
CONG_EF = DefineVariable('CONG_EF', CONG_EF, database)
SININFOTR = DefineVariable('SININFOTR', SININFOTR, database)
CONINFOTR = DefineVariable('CONINFOTR', CONINFOTR, database)



# Coeficientes
COEF_INTER = Beta('COEF_INTER',0,None,None,0)
COEF_JOVEN = Beta('COEF_JOVEN',0,None,None,0)
COEF_ADULTO = Beta('COEF_ADULTO',0,None,None,0)
COEF_MAYOR_60 = Beta('COEF_MAYOR_60',0,None,None,0)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',0,None,None,0)
COEF_EDUC_SUPERIOR = Beta('COEF_EDUC_SUPERIOR',0,None,None,0)
COEF_TPROF = Beta('COEF_TPROF',0,None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',0,None,None,0)
COEF_HPICO = Beta('COEF_HPICO',0,None,None,0)
COEF_HVALLE = Beta('COEF_HVALLE',0,None,None,0)
COEF_CSECO = Beta('COEF_CSECO',0,None,None,0)
COEF_CLLUVIA = Beta('COEF_CLLUVIA',0,None,None,0)
COEF_CONG_AB = Beta('COEF_CONG_AB',0,None,None,0)
COEF_CONG_CD = Beta('COEF_CONG_CD',0,None,None,0)
COEF_CONG_EF = Beta('COEF_CONG_EF',0,None,None,0)
COEF_SININFOTR = Beta('COEF_SININFOTR',0,None,None,0)
COEF_CONINFOTR = Beta('COEF_CONINFOTR',0,None,None,0)


# Ecuacion Estructural Variable Latente

omega = RandomVariable('omega')
density = dist.normalpdf(omega) 
sigma_s = Beta('sigma_s',1,None,None,0)

ACTAGR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA + COEF_HORASTRAB*HORASTRAB+\
    COEF_CONG_CD*CONG_CD + COEF_CONG_EF*CONG_EF+COEF_SININFOTR*SININFOTR

STRESS = COEF_INTER + COEF_JOVEN*JOVEN + COEF_HORASTRAB*HORASTRAB + COEF_HPICO*HPICO +\
    COEF_SININFOTR*SININFOTR

AMBLABOR = COEF_INTER + COEF_ADULTO*ADULTO + COEF_CONG_CD*CONG_CD + COEF_ACTAGR*ACTAGR+\
    COEF_STRESS*STRESS


COEF_EDAD2*EDAD2+\
COEF_EDAD3*EDAD3+\
COEF_EDUCAC1*EDUCAC1+\
COEF_EDUCAC2*EDUCAC2+\
COEF_EDUCAC3*EDUCAC3+\
COEF_EDAD4*EDAD4+COEF_TPROF*TPROF+\



### ECUACIONES DE MEDIDA
INTER_CV = Beta('INTER_CV',0,None,None,0)
INTER_AN = Beta('INTER_AN',0,None,None,0)
INTER_CA = Beta('INTER_CA',0,None,None,1)

BETA_CV_F2 = Beta('BETA_CV_F2',-1,None,None,0)
BETA_AN_F2 = Beta('BETA_AN_F2',-1,None,None,0)
BETA_CA_F2 = Beta('BETA_CA_F2',0,None,None,1)

MODEL_CV = INTER_CV + BETA_CV_F2 * EXTROVERSION
MODEL_AN = INTER_AN + BETA_AN_F2 * EXTROVERSION
MODEL_CA = INTER_CA + BETA_CA_F2 * EXTROVERSION

SIGMA_STAR_CV = Beta('SIGMA_STAR_CV', 10,None,None,0)
SIGMA_STAR_AN = Beta('SIGMA_STAR_AN', 10,None,None,0)
SIGMA_STAR_CA = Beta('SIGMA_STAR_CA', 10,None,None,0)


delta_1 = Beta('delta_1',0.25196349820243613,0,None,0 )
delta_2 = Beta('delta_2',0.759172317380935,0,None,0 )
tau_1 = -delta_1 - delta_2
tau_2 = -delta_1 
tau_3 = delta_1
tau_4 = delta_1 + delta_2

Envir01_tau_1 = (tau_1-MODEL_Envir01) / SIGMA_STAR_Envir01
Envir01_tau_2 = (tau_2-MODEL_Envir01) / SIGMA_STAR_Envir01
Envir01_tau_3 = (tau_3-MODEL_Envir01) / SIGMA_STAR_Envir01
Envir01_tau_4 = (tau_4-MODEL_Envir01) / SIGMA_STAR_Envir01
IndEnvir01 = {
    1: bioNormalCdf(Envir01_tau_1),
    2: bioNormalCdf(Envir01_tau_2)-bioNormalCdf(Envir01_tau_1),
    3: bioNormalCdf(Envir01_tau_3)-bioNormalCdf(Envir01_tau_2),
    4: bioNormalCdf(Envir01_tau_4)-bioNormalCdf(Envir01_tau_3),
    5: 1-bioNormalCdf(Envir01_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_Envir01 = Elem(IndEnvir01, Envir01)


Envir02_tau_1 = (tau_1-MODEL_Envir02) / SIGMA_STAR_Envir02
Envir02_tau_2 = (tau_2-MODEL_Envir02) / SIGMA_STAR_Envir02
Envir02_tau_3 = (tau_3-MODEL_Envir02) / SIGMA_STAR_Envir02
Envir02_tau_4 = (tau_4-MODEL_Envir02) / SIGMA_STAR_Envir02
IndEnvir02 = {
    1: bioNormalCdf(Envir02_tau_1),
    2: bioNormalCdf(Envir02_tau_2)-bioNormalCdf(Envir02_tau_1),
    3: bioNormalCdf(Envir02_tau_3)-bioNormalCdf(Envir02_tau_2),
    4: bioNormalCdf(Envir02_tau_4)-bioNormalCdf(Envir02_tau_3),
    5: 1-bioNormalCdf(Envir02_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_Envir02 = Elem(IndEnvir02, Envir02)

Envir03_tau_1 = (tau_1-MODEL_Envir03) / SIGMA_STAR_Envir03
Envir03_tau_2 = (tau_2-MODEL_Envir03) / SIGMA_STAR_Envir03
Envir03_tau_3 = (tau_3-MODEL_Envir03) / SIGMA_STAR_Envir03
Envir03_tau_4 = (tau_4-MODEL_Envir03) / SIGMA_STAR_Envir03
IndEnvir03 = {
    1: bioNormalCdf(Envir03_tau_1),
    2: bioNormalCdf(Envir03_tau_2)-bioNormalCdf(Envir03_tau_1),
    3: bioNormalCdf(Envir03_tau_3)-bioNormalCdf(Envir03_tau_2),
    4: bioNormalCdf(Envir03_tau_4)-bioNormalCdf(Envir03_tau_3),
    5: 1-bioNormalCdf(Envir03_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_Envir03 = Elem(IndEnvir03, Envir03)

Mobil11_tau_1 = (tau_1-MODEL_Mobil11) / SIGMA_STAR_Mobil11
Mobil11_tau_2 = (tau_2-MODEL_Mobil11) / SIGMA_STAR_Mobil11
Mobil11_tau_3 = (tau_3-MODEL_Mobil11) / SIGMA_STAR_Mobil11
Mobil11_tau_4 = (tau_4-MODEL_Mobil11) / SIGMA_STAR_Mobil11
IndMobil11 = {
    1: bioNormalCdf(Mobil11_tau_1),
    2: bioNormalCdf(Mobil11_tau_2)-bioNormalCdf(Mobil11_tau_1),
    3: bioNormalCdf(Mobil11_tau_3)-bioNormalCdf(Mobil11_tau_2),
    4: bioNormalCdf(Mobil11_tau_4)-bioNormalCdf(Mobil11_tau_3),
    5: 1-bioNormalCdf(Mobil11_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_Mobil11 = Elem(IndMobil11, Mobil11)

Mobil14_tau_1 = (tau_1-MODEL_Mobil14) / SIGMA_STAR_Mobil14
Mobil14_tau_2 = (tau_2-MODEL_Mobil14) / SIGMA_STAR_Mobil14
Mobil14_tau_3 = (tau_3-MODEL_Mobil14) / SIGMA_STAR_Mobil14
Mobil14_tau_4 = (tau_4-MODEL_Mobil14) / SIGMA_STAR_Mobil14
IndMobil14 = {
    1: bioNormalCdf(Mobil14_tau_1),
    2: bioNormalCdf(Mobil14_tau_2)-bioNormalCdf(Mobil14_tau_1),
    3: bioNormalCdf(Mobil14_tau_3)-bioNormalCdf(Mobil14_tau_2),
    4: bioNormalCdf(Mobil14_tau_4)-bioNormalCdf(Mobil14_tau_3),
    5: 1-bioNormalCdf(Mobil14_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_Mobil14 = Elem(IndMobil14, Mobil14)

Mobil16_tau_1 = (tau_1-MODEL_Mobil16) / SIGMA_STAR_Mobil16
Mobil16_tau_2 = (tau_2-MODEL_Mobil16) / SIGMA_STAR_Mobil16
Mobil16_tau_3 = (tau_3-MODEL_Mobil16) / SIGMA_STAR_Mobil16
Mobil16_tau_4 = (tau_4-MODEL_Mobil16) / SIGMA_STAR_Mobil16
IndMobil16 = {
    1: bioNormalCdf(Mobil16_tau_1),
    2: bioNormalCdf(Mobil16_tau_2)-bioNormalCdf(Mobil16_tau_1),
    3: bioNormalCdf(Mobil16_tau_3)-bioNormalCdf(Mobil16_tau_2),
    4: bioNormalCdf(Mobil16_tau_4)-bioNormalCdf(Mobil16_tau_3),
    5: 1-bioNormalCdf(Mobil16_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_Mobil16 = Elem(IndMobil16, Mobil16)

Mobil17_tau_1 = (tau_1-MODEL_Mobil17) / SIGMA_STAR_Mobil17
Mobil17_tau_2 = (tau_2-MODEL_Mobil17) / SIGMA_STAR_Mobil17
Mobil17_tau_3 = (tau_3-MODEL_Mobil17) / SIGMA_STAR_Mobil17
Mobil17_tau_4 = (tau_4-MODEL_Mobil17) / SIGMA_STAR_Mobil17
IndMobil17 = {
    1: bioNormalCdf(Mobil17_tau_1),
    2: bioNormalCdf(Mobil17_tau_2)-bioNormalCdf(Mobil17_tau_1),
    3: bioNormalCdf(Mobil17_tau_3)-bioNormalCdf(Mobil17_tau_2),
    4: bioNormalCdf(Mobil17_tau_4)-bioNormalCdf(Mobil17_tau_3),
    5: 1-bioNormalCdf(Mobil17_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_Mobil17 = Elem(IndMobil17, Mobil17)

# Choice model
# Read the estimates from the sequential estimation, and use
# them as starting values

ASC_CAR = Beta('ASC_CAR',0.7725067037758291,None,None,0 )
ASC_SM = Beta('ASC_SM',1.8865188103480808,None,None,0 )
ASC_PT	 = Beta('ASC_PT',0,None,None,1)

BETA_COST_HWH = Beta('BETA_COST_HWH',-1.7800532700436242,None,None,0 )
BETA_COST_OTHER = Beta('BETA_COST_OTHER',-0.8176256998217855,None,None,0 )
BETA_DIST = Beta('BETA_DIST',-5.809646562001414,None,None,0 )
BETA_TIME_CAR_CL = Beta('BETA_TIME_CAR_CL',-1.6818275468466484,None,None,0 )
BETA_TIME_CAR_REF = Beta('BETA_TIME_CAR_REF',-17.694645513468497,None,None,0 )
BETA_TIME_PT_CL = Beta('BETA_TIME_PT_CL',-1.2424575875582378,None,None,0 )
BETA_TIME_PT_REF = Beta('BETA_TIME_PT_REF',-6.279351989351876,None,None,0 )
BETA_WAITING_TIME = Beta('BETA_WAITING_TIME',-0.02949883465222827,None,None,0 )

TimePT_scaled  = DefineVariable('TimePT_scaled', TimePT   /  200 ,database)
TimeCar_scaled  = DefineVariable('TimeCar_scaled', TimeCar   /  200 ,database)
MarginalCostPT_scaled  = \
 DefineVariable('MarginalCostPT_scaled', MarginalCostPT   /  10 ,database)
CostCarCHF_scaled  = \
 DefineVariable('CostCarCHF_scaled', CostCarCHF   /  10 ,database)
distance_km_scaled  = \
 DefineVariable('distance_km_scaled', distance_km   /  5 ,database)
PurpHWH = DefineVariable('PurpHWH', TripPurpose == 1,database)
PurpOther = DefineVariable('PurpOther', TripPurpose != 1,database)


### DEFINITION OF UTILITY FUNCTIONS:

BETA_TIME_PT = BETA_TIME_PT_REF * exp(BETA_TIME_PT_CL * CARLOVERS)

V0 = ASC_PT + \
     BETA_TIME_PT * TimePT_scaled + \
     BETA_WAITING_TIME * WaitingTimePT + \
     BETA_COST_HWH * MarginalCostPT_scaled * PurpHWH  +\
     BETA_COST_OTHER * MarginalCostPT_scaled * PurpOther

BETA_TIME_CAR = BETA_TIME_CAR_REF * exp(BETA_TIME_CAR_CL * CARLOVERS)

V1 = ASC_CAR + \
      BETA_TIME_CAR * TimeCar_scaled + \
      BETA_COST_HWH * CostCarCHF_scaled * PurpHWH  + \
      BETA_COST_OTHER * CostCarCHF_scaled * PurpOther 

V2 = ASC_SM + BETA_DIST * distance_km_scaled

# Associate utility functions with the numbering of alternatives
V = {0: V0,
     1: V1,
     2: V2}

# Associate the availability conditions with the alternatives.
# In this example all alternatives are available for each individual.
av = {0: 1,
      1: 1,
      2: 1}

# The choice model is a logit, conditional to the value of the latent variable
condprob = models.logit(V,av,Choice)
condlike = P_Envir01 * \
          P_Envir02 * \
          P_Envir03 * \
          P_Mobil11 * \
          P_Mobil14 * \
          P_Mobil16 * \
          P_Mobil17 * \
          condprob

loglike = log(Integrate(condlike * density,'omega'))

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "LV_Optima"
results = biogeme.estimate()
# Get the results in a pandas table
pandasResults = results.getEstimatedParameters()
print(pandasResults)
print(f"Nbr of observations: {database.getNumberOfObservations()}")
print(f"LL(0) =    {results.data.initLogLike:.3f}")
print(f"LL(beta) = {results.data.logLike:.3f}")
print(f"rho bar square = {results.data.rhoBarSquare:.3g}")
print(f"Output file: {results.data.htmlFileName}")




