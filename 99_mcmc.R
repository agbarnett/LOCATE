## 99_mcmc.R
# MCMC options for nimble; called by 99_run_weibull.R
# April 2023

debug = FALSE
n.chains = 2
thin = 10
MCMC = 20000
#MCMC = 200 # temporary
seed = TeachingDemos::char2seed('bradford')
