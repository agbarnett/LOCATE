# 99_run_weibull.R
# run a Weibull survival model
# April 2023
library(nimble)

# code
code_survival <- nimbleCode({
  ## Likelihood
  for (i in 1:N){ # loop through participants
    censored[i] ~ dinterval(time[i], c[i])
    time[i] ~ dweib(shape = r, scale = mu[i])
    log(mu[i]) <- inprod(alpha[1:P], X[i,1:P])
  }
  r ~ dexp(1)
  for(k in 1:P){
    alpha[k] ~ dnorm(mean = 0, sd = 1000)
  }
})

## data
# censored[i] should be given as data with a value of 1 if t[i] is right-censored (t[i] > c[i]) and 0 if it is observed
indata = mutate(indata, 
                censored = 1 - as.numeric(event),
                c = ifelse(censored == 1, time, 0), # censoring times
                time = ifelse(censored == 1, NA, time)) # The data vector for t should have NA (indicating missing data) for any censored t[i] entries
#
bdata = list(time = indata$time,
             c = indata$c, 
             censored = indata$censored)
# regression
X = model.matrix.lm(censored ~ I(randomised == 'New model of care') + sex + I(age/10) + centrename, data = indata, na.action = "na.pass") # censored as the dependent is a dummy variable
X = as.matrix(X)
# remove variables with no variance
remove = as.numeric(which(colSums(X) == 0))
if(any(remove)){
  X = X[, -remove]
}
#
P = ncol(X)
N = nrow(indata)
constants = list(N = N,
                 P = P,
                 X = X)

# run MCMC
source('99_mcmc.R')

# initial values
inits = list(alpha = rep(0, P), r = 1)

# parameters
parms = c('alpha','r')

# set up
model_survival = nimbleModel(code_survival, 
                            data = bdata,
                            inits = inits,
                            constants = constants)

# run model
mcmc =  nimbleMCMC(model = model_survival,
                   inits = inits,
                monitors = parms,
                   niter = MCMC*2*thin, # times 2 for burn-in 
                    thin = thin,
                 nchains = n.chains, 
                 nburnin = MCMC,
                 summary = TRUE, 
                 setSeed = seed,
                   WAIC = FALSE) # 

# for saving
weibull = mcmc

# table
table = data.frame(mcmc$summary$all.chains) %>%
  tibble::rownames_to_column(var = "var") %>%
  clean_names() %>%
  select(-median, -st_dev) %>%
  rename('lower' = 'x95_ci_low',
         'upper' = 'x95_ci_upp'
  ) %>%
  mutate(mean = ifelse(var != 'r', exp(mean), mean), # covert to hazard ratios (not shape parameter)
         lower = ifelse(var != 'r', exp(lower), lower),
         upper = ifelse(var != 'r', exp(upper), upper))
# add names
names = data.frame(var = c(paste('alpha[', 1:P, ']', sep=''), 'r'), 
                   variable = c(colnames(X), 'shape'))
table = full_join(names, table, by='var')
                   