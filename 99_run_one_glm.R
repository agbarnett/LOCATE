# 99_run_one_glm.R
# run one Bayesian model, useful for running multiple imputations
# May 2023

# transform dependent variables to matrix
X = as.matrix(X)

# get the dependent variable
depvar = as.character(formula(equation))[2] # get dependent variable from formula
index = which(names(indata) == depvar)
dep = indata[,index]

# remove variables with no variance
remove = as.numeric(which(colSums(X) == 0))
if(any(remove)){
  X = X[, -remove]
}
#
P = ncol(X)
N = nrow(indata)
# sort out prior if not given
if(is.null(prior_sd)){
  prior_sd = rep(1000, P)  # vague prior
}
#
constants = list(N = N,
                 P = P,
                 X = X,
                 prior_sd = prior_sd) # added because of sparse results

## data
bdata = list(dep = dep)

# initial values
inits = list(alpha = rep(0, P))
if(family=='normal'){
  inits$sd = 1
}

# parameters, mu for predictions
parms = c('alpha','mu')
if(family=='normal'){
  parms = c(parms, 'sd')
}

# run MCMC
source('99_mcmc.R')

# set up
model_glm = nimbleModel(code_glm, 
                        data = bdata,
                        inits = inits,
                        constants = constants)

# run model
mcmc =  nimbleMCMC(model = model_glm,
                   inits = inits,
                   monitors = parms,
                   niter = MCMC*2*thin, # times 2 for burn-in 
                   thin = thin,
                   nchains = n.chains, 
                   nburnin = MCMC,
                   summary = TRUE, 
                   setSeed = seed,
                   WAIC = FALSE) # 



