import pandas as pd
import numpy as np
import biogeme.database as db
import biogeme.biogeme as bio
from biogeme.models import piecewise
import biogeme.loglikelihood as ll


pandas = pd.read_table("optimaDataset.dat")
database = db.Database("optima",pandas)

from headers import *

exclude = (Choice == -1.0)
database.remove(exclude)


# Piecewise linear definition of income

ScaledIncome = DefineVariable('ScaledIncome',\
                              CalculatedIncome / 1000,database)

thresholds = [4,6,8,10]
ContIncome = piecewise(ScaledIncome,thresholds)
ContIncome_0_4000 = ContIncome[0]
ContIncome_4000_6000 = ContIncome[1]
ContIncome_6000_8000 = ContIncome[2]
ContIncome_8000_10000 = ContIncome[3]
ContIncome_10000_more = ContIncome[4]

age_65_more = DefineVariable('age_65_more',age >= Numeric(65),database)
moreThanOneCar = DefineVariable('moreThanOneCar',NbCar > 1,database)
moreThanOneBike = DefineVariable('moreThanOneBike',NbBicy > 1,database)
individualHouse = DefineVariable('individualHouse',\
                                 HouseType == 1,database)
male = DefineVariable('male',Gender == 1,database)
haveChildren = DefineVariable('haveChildren',\
                              ((FamilSitu == 3)+(FamilSitu == 4)) > 0,database)
haveGA = DefineVariable('haveGA',GenAbST == 1,database)
highEducation = DefineVariable('highEducation', Education >= 6,database)

### Coefficients
coef_intercept = Beta('coef_intercept',0.0,None,None,0)
coef_age_65_more = Beta('coef_age_65_more',0.0,None,None,0)
coef_age_unknown = Beta('coef_age_unknown',0.0,None,None,0)
coef_haveGA = Beta('coef_haveGA',0.0,None,None,0)
coef_ContIncome_0_4000 = \
 Beta('coef_ContIncome_0_4000',0.0,None,None,0)
coef_ContIncome_4000_6000 = \
 Beta('coef_ContIncome_4000_6000',0.0,None,None,0)
coef_ContIncome_6000_8000 = \
 Beta('coef_ContIncome_6000_8000',0.0,None,None,0)
coef_ContIncome_8000_10000 = \
 Beta('coef_ContIncome_8000_10000',0.0,None,None,0)
coef_ContIncome_10000_more = \
 Beta('coef_ContIncome_10000_more',0.0,None,None,0)
coef_moreThanOneCar = \
 Beta('coef_moreThanOneCar',0.0,None,None,0)
coef_moreThanOneBike = \
 Beta('coef_moreThanOneBike',0.0,None,None,0)
coef_individualHouse = \
 Beta('coef_individualHouse',0.0,None,None,0)
coef_male = Beta('coef_male',0.0,None,None,0)
coef_haveChildren = Beta('coef_haveChildren',0.0,None,None,0)
coef_highEducation = Beta('coef_highEducation',0.0,None,None,0)

### Latent variable: structural equation

# Note that the expression must be on a single line. In order to 
# write it across several lines, each line must terminate with 
# the \ symbol

CARLOVERS = \
coef_intercept +\
coef_age_65_more * age_65_more +\
coef_ContIncome_0_4000 * ContIncome_0_4000 +\
coef_ContIncome_4000_6000 * ContIncome_4000_6000 +\
coef_ContIncome_6000_8000 * ContIncome_6000_8000 +\
coef_ContIncome_8000_10000 * ContIncome_8000_10000 +\
coef_ContIncome_10000_more * ContIncome_10000_more +\
coef_moreThanOneCar * moreThanOneCar +\
coef_moreThanOneBike * moreThanOneBike +\
coef_individualHouse * individualHouse +\
coef_male * male +\
coef_haveChildren * haveChildren +\
coef_haveGA * haveGA +\
coef_highEducation * highEducation

sigma_s1 = Beta('sigma_s1',1,0.001,None,1)

FACTOR2 = \
coef_intercept +\
coef_age_65_more * age_65_more +\
coef_ContIncome_0_4000 * ContIncome_0_4000 +\
coef_ContIncome_4000_6000 * ContIncome_4000_6000 +\
coef_ContIncome_6000_8000 * ContIncome_6000_8000 +\
coef_ContIncome_8000_10000 * ContIncome_8000_10000 +\
coef_ContIncome_10000_more * ContIncome_10000_more +\
coef_moreThanOneCar * moreThanOneCar +\
coef_moreThanOneBike * moreThanOneBike +\
coef_individualHouse * individualHouse +\
coef_male * male +\
coef_haveChildren * haveChildren +\
coef_haveGA * haveGA +\
coef_highEducation * highEducation 

sigma_s2 = Beta('sigma_s2',1,0.001,None,1)


### Measurement equations Factor 1

INTER_Envir01 = Beta('INTER_Envir01',0,None,None,1)
INTER_Envir02 = Beta('INTER_Envir02',0,None,None,0)
INTER_Envir03 = Beta('INTER_Envir03',0,None,None,0)
INTER_Mobil11 = Beta('INTER_Mobil11',0,None,None,0)
INTER_Mobil14 = Beta('INTER_Mobil14',0,None,None,0)
INTER_Mobil16 = Beta('INTER_Mobil16',0,None,None,0)
INTER_Mobil17 = Beta('INTER_Mobil17',0,None,None,0)

B_Envir01_F1 = Beta('B_Envir01_F1',-1,None,None,1)
B_Envir02_F1 = Beta('B_Envir02_F1',-1,None,None,0)
B_Envir03_F1 = Beta('B_Envir03_F1',1,None,None,0)
B_Mobil11_F1 = Beta('B_Mobil11_F1',1,None,None,0)
B_Mobil14_F1 = Beta('B_Mobil14_F1',1,None,None,0)
B_Mobil16_F1 = Beta('B_Mobil16_F1',1,None,None,0)
B_Mobil17_F1 = Beta('B_Mobil17_F1',1,None,None,0)

MODEL_Envir01 = INTER_Envir01 + B_Envir01_F1 * CARLOVERS
MODEL_Envir02 = INTER_Envir02 + B_Envir02_F1 * CARLOVERS
MODEL_Envir03 = INTER_Envir03 + B_Envir03_F1 * CARLOVERS
MODEL_Mobil11 = INTER_Mobil11 + B_Mobil11_F1 * CARLOVERS
MODEL_Mobil14 = INTER_Mobil14 + B_Mobil14_F1 * CARLOVERS
MODEL_Mobil16 = INTER_Mobil16 + B_Mobil16_F1 * CARLOVERS
MODEL_Mobil17 = INTER_Mobil17 + B_Mobil17_F1 * CARLOVERS

SIGMA_STAR_Envir01 = Beta('SIGMA_STAR_Envir01',10,0.001,None,0)
SIGMA_STAR_Envir02 = Beta('SIGMA_STAR_Envir02',10,0.001,None,0)
SIGMA_STAR_Envir03 = Beta('SIGMA_STAR_Envir03',10,0.001,None,0)
SIGMA_STAR_Mobil11 = Beta('SIGMA_STAR_Mobil11',10,0.001,None,0)
SIGMA_STAR_Mobil14 = Beta('SIGMA_STAR_Mobil14',10,0.001,None,0)
SIGMA_STAR_Mobil16 = Beta('SIGMA_STAR_Mobil16',10,0.001,None,0)
SIGMA_STAR_Mobil17 = Beta('SIGMA_STAR_Mobil17',10,0.001,None,0)


### Measurement equations Factor 2

INTER_ResidCh01 = Beta('INTER_ResidCh01',0,None,None,0)
INTER_ResidCh04 = Beta('INTER_ResidCh04',0,None,None,0 )
INTER_ResidCh05 = Beta('INTER_ResidCh05',0,None,None,0 )
INTER_ResidCh06 = Beta('INTER_ResidCh06',0,None,None,0 )
INTER_LifSty07  = Beta('INTER_LifSty07',0,None,None,0 )
INTER_LifSty10  = Beta('INTER_LifSty10',0,None,None,0 )


B_ResidCh01_F2 = Beta('B_ResidCh01_F2',-1,None,None,0)
B_ResidCh04_F2 = Beta('B_ResidCh04_F2',-1,None,None,0 )
B_ResidCh05_F2 = Beta('B_ResidCh05_F2',1,None,None,0 )
B_ResidCh06_F2 = Beta('B_ResidCh06_F2',1,None,None,0 )
B_LifSty07_F2  = Beta('B_LifSty07_F2',1,None,None,0 )
B_LifSty10_F2  = Beta('B_LifSty10_F2',1,None,None,0 )

MODEL_ResidCh01 = INTER_ResidCh01 + B_ResidCh01_F2 * FACTOR2
MODEL_ResidCh04 = INTER_ResidCh04 + B_ResidCh04_F2 * FACTOR2
MODEL_ResidCh05 = INTER_ResidCh05 + B_ResidCh05_F2 * FACTOR2
MODEL_ResidCh06 = INTER_ResidCh06 + B_ResidCh06_F2 * FACTOR2
MODEL_LifSty07  = INTER_LifSty07 + B_LifSty07_F2 * FACTOR2
MODEL_LifSty10  = INTER_LifSty10 + B_LifSty10_F2 * FACTOR2

SIGMA_STAR_ResidCh01 = Beta('SIGMA_STAR_ResidCh01',10,0.001,None,0)
SIGMA_STAR_ResidCh04 = Beta('SIGMA_STAR_ResidCh04',10,0.001,None,0)
SIGMA_STAR_ResidCh05 = Beta('SIGMA_STAR_ResidCh05',10,0.001,None,0)
SIGMA_STAR_ResidCh06 = Beta('SIGMA_STAR_ResidCh06',10,0.001,None,0)
SIGMA_STAR_LifSty07 = Beta('SIGMA_STAR_LifSty07',10,0.001,None,0)
SIGMA_STAR_LifSty10 = Beta('SIGMA_STAR_LifSty10',10,0.001,None,0)


F = {}
F['Envir01'] = Elem({0:0, \
 1:ll.loglikelihoodregression(Envir01,MODEL_Envir01,SIGMA_STAR_Envir01)},\
  (Envir01 > 0)*(Envir01 < 6))
F['Envir02'] = Elem({0:0, \
 1:ll.loglikelihoodregression(Envir02,MODEL_Envir02,SIGMA_STAR_Envir02)},\
  (Envir02 > 0)*(Envir02 < 6))
F['Envir03'] = Elem({0:0, \
 1:ll.loglikelihoodregression(Envir03,MODEL_Envir03,SIGMA_STAR_Envir03)},\
  (Envir03 > 0)*(Envir03 < 6))
F['Mobil11'] = Elem({0:0, \
 1:ll.loglikelihoodregression(Mobil11,MODEL_Mobil11,SIGMA_STAR_Mobil11)},\
  (Mobil11 > 0)*(Mobil11 < 6))
F['Mobil14'] = Elem({0:0, \
 1:ll.loglikelihoodregression(Mobil14,MODEL_Mobil14,SIGMA_STAR_Mobil14)},\
  (Mobil14 > 0)*(Mobil14 < 6))
F['Mobil16'] = Elem({0:0, \
 1:ll.loglikelihoodregression(Mobil16,MODEL_Mobil16,SIGMA_STAR_Mobil16)},\
  (Mobil16 > 0)*(Mobil16 < 6))
F['Mobil17'] = Elem({0:0, \
 1:ll.loglikelihoodregression(Mobil17,MODEL_Mobil17,SIGMA_STAR_Mobil17)},\
  (Mobil17 > 0)*(Mobil17 < 6))
F['ResidCh01'] = Elem({0:0, \
 1:ll.loglikelihoodregression(ResidCh01,MODEL_ResidCh01,SIGMA_STAR_ResidCh01)},\
  (ResidCh01 > 0)*(ResidCh01 < 6))
F['ResidCh04'] = Elem({0:0, \
 1:ll.loglikelihoodregression(ResidCh04,MODEL_ResidCh04,SIGMA_STAR_ResidCh04)},\
  (ResidCh04 > 0)*(ResidCh04 < 6))
F['ResidCh05'] = Elem({0:0, \
 1:ll.loglikelihoodregression(ResidCh05,MODEL_ResidCh05,SIGMA_STAR_ResidCh05)},\
  (ResidCh05 > 0)*(ResidCh05 < 6))
F['ResidCh06'] = Elem({0:0, \
 1:ll.loglikelihoodregression(ResidCh06,MODEL_ResidCh06,SIGMA_STAR_ResidCh06)},\
  (ResidCh06 > 0)*(ResidCh06 < 6))
F['LifSty07'] = Elem({0:0, \
 1:ll.loglikelihoodregression(LifSty07,MODEL_LifSty07,SIGMA_STAR_LifSty07)},\
  (LifSty07 > 0)*(LifSty07 < 6))
F['LifSty10'] = Elem({0:0, \
 1:ll.loglikelihoodregression(LifSty10,MODEL_LifSty10,SIGMA_STAR_LifSty10)},\
  (LifSty10 > 0)*(LifSty10 < 6))



loglike = bioMultSum(F)

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "01LV_OptimaRegression"
results = biogeme.estimate()
print(f"Estimated betas: {len(results.data.betaValues)}")
print(f"final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")