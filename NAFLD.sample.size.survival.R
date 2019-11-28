# NAFLD.sample.size.survival.R
# sample size for NAFLD application (MRFF keeping people out of hospital)
# final version using survival times
# see https://cran.r-project.org/web/packages/coxed/vignettes/simulating_survival_data.html#simulating-a-single-dataset
# November 2019
library(coxed)
library(broom)
library(dplyr)
set.seed(879497)

## outcome =  time to diagnosis of high risk disease
# key numbers:
lower.per.group = 78 # smaller sample size per group 
higher.per.group = 117 # larger sample size per group
n.per.group = lower.per.group
reduction = 0.5 # multiplicative reduction with treatment group ...
beta = log(reduction) # ... on log scale
censor = 0.05 # proportion censored (assumed small, just deaths and other loss to follow-up)
mean.survival = 6*30 # this many months in usual care group (on scale of days)
sd.survival = 10 # standard deviation in survival
max.time = 365 # longest observed survival time (days)
n.sim = 1000 # number of simulations

# lognormal hazard function  
my.hazard <- function(t){ 
  dnorm((log(t) - log(mean.survival))/log(sd.survival)) /
    (log(sd.survival)*t*(1 - pnorm((log(t) - log(mean.survival))/log(sd.survival))))
}

# data with just one independent variable: treatment or usual care
my.data = data.frame(group = c(rep(0, n.per.group), rep(1, n.per.group)))
# simulate survival data (one example here)
simdata <- sim.survdata(T=max.time, X=my.data, beta=beta, censor=censor, num.data.frames=1, hazard.fun = my.hazard)
for.model = simdata$data %>%
  mutate(failed = ifelse(y == max.time, TRUE, failed)) # censor if hit max time

# baseline functions for the first simulation iteration:
survsim.plot(simdata, df=1, type="baseline")
# histograms of the simulated durations, linear predictors, and exponentiated linear predictors:
survsim.plot(simdata, df=1, type="hist")

# check data appearance
km = survfit(Surv(y, event=failed) ~ group, data = simdata$data)
plot(km, col=1:2)

## simulate ##
sim.res = NULL
for (k in 1:n.sim){
  # simulate data
  simdata <- sim.survdata(T=max.time, X=my.data, beta=beta, censor=censor, num.data.frames=1, hazard.fun = my.hazard)
  for.model = simdata$data %>%
    mutate(failed = ifelse(y == max.time, FALSE, failed)) # censor if hit max time (so no event)
  # model
  res = survreg(Surv(y, event=failed) ~ group, data = for.model)
  ests = tidy(res) %>%
    filter(term == 'group') %>%
    mutate(sim = k) %>%
    select(-term)
  sim.res = bind_rows(sim.res, ests)
}
# check results
hist(sim.res$estimate)
prop.table(table(sim.res$p.value < 0.05)) # power
