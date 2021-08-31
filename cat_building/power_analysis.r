# cat_building - Power Analyses for E2 and E3a

# NOTES:
# be aware that these power simulations can take a long time to run
# save a copy of E2 and E3a reminding data to same directory as script (or change directory prior to loading) 

# load libraries
library(simr)

# # read in data
# # # Experiment 3a
data.reminding <- read.csv('Experiment 3a reminding data.csv')
# # keep data from subs that completed at least half of reminding assessment, but drop missing values
data.reminding <- subset(data.reminding, include == 1)
data.sim <- subset(data.reminding, condition == "OnlyLabels" | condition == 'OnlyInstruct') 

# # # Experiment 2
e2.reminding <- read.csv('Experiment 2 reminding data.csv')

# E3a Power analyses
# # fit model
model1 <- glmer(reminding ~ condition + (1|pnum), 
	subset(data.sim, principle != "Distractor"), family = 'binomial')

# # power analysis
powerSim(model1)

# Power for predictor 'condition', (95% confidence interval):
#       43.20% (40.10, 46.34)

# Test: Likelihood ratio

# Based on 1000 simulations, (118 warnings, 0 errors)
# alpha = 0.05, nrow = 316

# Time elapsed: 0 h 3 m 53 s

# nb: result might be an observed power calculation
# Warning message:
# In observedPowerWarning(sim) :
#   This appears to be an "observed power" calculation

# # Extrapolate power simulation across a large sample, create power curve, and print plot to display
model2 <- extend(model1, along = "pnum", n = 1000)
pc <- powerCurve(model2, along = 'pnum')
plot(pc)


# E2 Power Analyses
# # fit models
CBDCmod <- glmer(reminding ~ condition + (1|pnum), family = 'binomial', data = subset(e2.reminding, principle != "Distractor" & condition != "Single-comparison"))
CBSCmod <- glmer(reminding ~ condition + (1|pnum), family = 'binomial', data = subset(e2.reminding, principle != "Distractor" & condition != "Double-comparison"))
DCSCmod <- glmer(reminding ~ condition + (1|pnum), family = 'binomial', data = subset(e2.reminding, principle != "Distractor" & condition != "Category-building"))


# # Double Category-building vs. Double-comparison post-hoc power analyses
DCsim <- powerSim(CBDCmod)

# Power for predictor 'condition', (95% confidence interval):
#       74.90% (72.09, 77.56)

# Test: Likelihood ratio

# Based on 1000 simulations, (78 warnings, 0 errors)
# alpha = 0.05, nrow = 396

# Time elapsed: 0 h 4 m 23 s

# nb: result might be an observed power calculation
# Warning message:
# In observedPowerWarning(sim) :
#   This appears to be an "observed power" calculation

# # Category-building vs. Single-comparison post-hoc power analyses
SCsim <- powerSim(CBSCmod)

# Power for predictor 'condition', (95% confidence interval):
#       99.10% (98.30, 99.59)

# Test: Likelihood ratio

# Based on 1000 simulations, (91 warnings, 0 errors)
# alpha = 0.05, nrow = 404

# Time elapsed: 0 h 5 m 30 s

# nb: result might be an observed power calculation

# # Double-cmparison vs. Single-comparison 
Compsim <- powerSim(DCSCmod)

# Power for predictor 'condition', (95% confidence interval):
#       44.20% (41.09, 47.34)

# Test: Likelihood ratio

# Based on 1000 simulations, (109 warnings, 0 errors)
# alpha = 0.05, nrow = 250

# Time elapsed: 0 h 4 m 50 s

# nb: result might be an observed power calculation

# # # extend Compsim along larger sample sizes, create power curve, and print plot to display
model3 <- extend(DCSCmod, along = "pnum", n = 200)

pc <- powerCurve(model3, along = 'pnum')
plot(pc)
