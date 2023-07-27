# 99_glm_bayes.R
# functions to run a GLM in a Bayesian framework using nimble
# version for normal and binomial
# April 2023

glm_bayes = function(indata, 
                     family = 'normal', # normal or binomial
                     missing = NULL,
                     prior_sd = NULL,
                     equation){
  
  # impute missing predictors
  if(!is.null(missing)){
    # get distribution of complete
    index = which(names(indata) == missing)
    missvar = indata[,index]
    no_missing = missvar[!is.na(missvar)]
    n_missing = sum(is.na(missvar))
    #
    n_impute = 10
    imputed = list()
    for (k in 1:n_impute){
      imputed[[k]] = indata
      this_miss = missvar # start with complete data
      this_miss[is.na(this_miss)] = sample(no_missing, replace=TRUE, size = n_missing) # empirical distribution
      imputed[[k]][,index] = this_miss # replace variable in data
    }
  }
  
  ## code
  if(family=='normal'){
    code_glm <- nimbleCode({
      ## Likelihood
      for (i in 1:N){ # loop through participants
        dep[i] ~ dnorm(mean = mu[i], sd = sd)
        mu[i] <- inprod(alpha[1:P], X[i,1:P])
      }
      sd ~ dunif(0,100)
      for(k in 1:P){
        alpha[k] ~ dnorm(mean = 0, sd = prior_sd[k])
      }
    })
  }
  if(family=='binomial'){
    code_glm <- nimbleCode({
      ## Likelihood
      for (i in 1:N){ # loop through participants
        dep[i] ~ dbern(prob = mu[i])
        logit(mu[i]) <- inprod(alpha[1:P], X[i,1:P])
      }
      for(k in 1:P){
        alpha[k] ~ dnorm(mean = 0, sd = prior_sd[k])
      }
    })
  }
  if(family=='poisson'){
    code_glm <- nimbleCode({
      ## Likelihood
      for (i in 1:N){ # loop through participants
        dep[i] ~ dpois(mu[i])
        log(mu[i]) <- inprod(alpha[1:P], X[i,1:P])
      }
      for(k in 1:P){
        alpha[k] ~ dnorm(mean = 0, sd = prior_sd[k])
      }
    })
  }
  
  # run regression model, run multiple imputation or single model
  if(!is.null(missing)){ # using multiple imputation
    betas = vars = list()
    for (k in 1:n_impute){
      X = model.matrix.lm(as.formula(equation), data = imputed[[k]], na.action = "na.pass") 
      source('99_run_one_glm.R', local = environment()) # run the model with the imputed predictors
      # get estimates needed for multiple imputation
      samples = rbind(mcmc$samples$chain1, mcmc$samples$chain2) # combine both chains
      betas[[k]] = colMeans(samples)
      vars[[k]] = cov(samples)
    }
    # combine estimates
    mi_results = MIcombine(betas, vars) 
    stats = data.frame(var = attr(mi_results$coefficients, 'names'),
                       mean = mi_results$coefficients,
                       variance = diag(mi_results$variance)) %>%
      mutate(z = qnorm(0.975),
             se = sqrt(variance),
             lower = mean - (z*se),
             upper = mean + (z*se)) %>%
      select(-z, -se, -variance)
  }
  if(is.null(missing)){ # without multiple imputation
    X = model.matrix.lm(as.formula(equation), data = indata, na.action = "na.pass") 
    source('99_run_one_glm.R', local = environment())
    #
    for_stats = mcmc$summary$all.chains
    # formatting data frame of stats
    stats = data.frame(for_stats) %>%
      tibble::rownames_to_column(var = "var") %>%
      clean_names() %>%
      select(-median, -st_dev) %>%
      rename('lower' = 'x95_ci_low',
             'upper' = 'x95_ci_upp'
      ) 
    
  }
  
  # table
  table = filter(stats, !str_detect(var, pattern='^mu'))  # remove predictions
  # make into odds ratios for binomial; or rate ratios for Poisson
  if(family != 'normal'){
    table = mutate(table,
                   mean = exp(mean),
                   lower = exp(lower),
                   upper = exp(upper))
  }
  
  # add names to table
  names = data.frame(var = c(paste('alpha[', 1:P, ']', sep=''), 'sd'), 
                     variable = c(colnames(X), 'SD'))
  table = full_join(names, table, by='var')
  if(family != 'normal'){
    table = filter(table, var != 'sd')
  }
  table = select(table, -var) %>%
    mutate(variable = nice_variable(variable))
  
  # calculate residuals
  resid = filter(stats, str_detect(var, pattern='mu')) %>%
    bind_cols(bdata$dep)
  names(resid)[5] = 'fitted'
  resid = mutate(resid, resid = mean - fitted)
  
  # plot chain for intercept (just use latest imputation model)
  c1 = data.frame(res = mcmc$samples$chain1[,1], chain = 1) %>%
    mutate(sample = 1:n())
  c2 = data.frame(res = mcmc$samples$chain2[,1], chain = 2) %>%
    mutate(sample = 1:n())
  to_plot = bind_rows(c1, c2)
  cplot = ggplot(data = to_plot, aes(x = sample, y = res, col=factor(chain)))+
    geom_line()+
    scale_color_manual('Chain', values=c("khaki2", "limegreen"))+
    theme_bw()+
    theme(legend.position = 'none')+
    ylab('Intercept')
  
  # return
  to_return = list()
  to_return$glm = mcmc
  to_return$table = table
  to_return$N_participants = N
  to_return$resid = resid
  to_return$chain_plot = cplot
  return(to_return)
  
} # end of function
