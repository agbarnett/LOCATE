# 99_functions.R
# November 2020

### Functions use by 0_read_data_redcap.R to format data ###

centre_name = function(x){
  x = ifelse(x == 122, 'Metro South', 'Sunshine Coast')
}


# function for audit score, make into numbers for scoring
audit_score = function(x){
  x = as.character(x)
  y = case_when(
    x %in% c('Never','None, 1 or 2','No') ~ 0,
    x %in% c('Monthly or less','3 or 4','Less than monthly') ~ 1,
    x %in% c('2 to 4 times a month','5 or 6','Monthly','Yes, but not in the past year') ~ 2,
    x %in% c('2 to 3 times a week','7 to 9','Weekly') ~ 3,
    x %in% c('4 or more times a week','Daily or almost daily','10 or more','Yes, during the past year') ~ 4
  )
  return(y)
}

# make factor from numbers with yes/no as default
yesno = function(x, labels=c('No','Yes')){
  y = as.numeric(x)
  y = ifelse(x == '', NA, y)
  y = factor(y, levels=0:1, labels=labels)
  return(y)
}

# make factor from numbers
yesnounclear = function(x){
  y = as.numeric(x)
  y = ifelse(x == '', NA, y)
  y = factor(y, levels=0:2, labels=c('No','Yes','Unclear'))
  return(y)
}

#
drinks_factor = function(x){
  y = as.numeric(x)
  y = ifelse(x == '', NA, y)
  y = factor(y, levels=0:4, labels= c('Never','Less than monthly','Monthly','Weekly','Daily or almost daily'))
  return(y)
}

## function to run survival models
run_survival = function(indata,
                        outcome_date, # first date
                        start_date, # date of randomisation
                        censor_date # date of notes review
){
  # rename
  index = which(names(indata) == outcome_date)
  names(indata)[index] = 'outcome_date'
  index = which(names(indata) == start_date)
  names(indata)[index] = 'start_date'
  index = which(names(indata) == censor_date)
  names(indata)[index] = 'censor_date'
  
  # make data set
  indata = filter(indata, !is.na(randomised)) %>%
    mutate(event = !is.na(outcome_date), # event if there's an outcome date
            last_date = ifelse(event==TRUE, outcome_date, censor_date), # use censor date if no outcome
            last_date = as.Date(last_date, origin = '1970-01-01'),
            time = last_date - start_date + 0.5, # difference in days
            time = as.numeric(time) / 365.25 # convert to years
        )
  
  # exclude any negative or missing times
  negative = sum(indata$time <= 0, na.rm = TRUE) + sum(is.na(indata$time))
  indata = filter(indata, time > 0)
  
  # total follow-up time
  total_fu = sum(indata$time)
  
  ## create Kaplan-Meier
  indata = mutate(indata, 
                  random = ifelse(str_detect('New ', randomised), 'New model','Usual care'),
                  random = factor(randomised)) # makes nicer labels
  km = survfit(Surv(time, event==TRUE) ~ random, data = indata)
  
  # run Weibull model in nimble
  source('99_run_weibull.R', local = environment())
  
  #
  to_return = list()
  to_return$events = events
  to_return$censored = censored
  to_return$survdata = indata
  to_return$negative = negative
  to_return$km = km
  to_return$weibull = weibull
  to_return$table = table
  to_return$total_fu = total_fu
  to_return$chain_plot = cplot
  return(to_return)
}


# nice rename variable 
nice_variable = function(invar){
  out = case_when(
    str_detect(invar, 'New model') ~ 'New model of care',
    str_detect(invar, 'Male') ~ 'Male',
    str_detect(invar, 'phone_time') ~ 'Time since randomisation (months)',
    str_detect(invar, '\\bage\\b') ~ 'Age (+10 years)', # scaled
    str_detect(invar, 'Sunshine') ~ 'Sunshine Coast',
    invar == 'SD' ~ 'Standard deviation',
    str_detect(invar, 'Intercept') ~ 'Intercept',
    str_detect(invar, 'eq5d_b') ~ 'EQ-5D at baseline',
    str_detect(invar, 'gp_baseline') ~ 'GP visits at baseline (log-transformed)',
    str_detect(invar, 'dietician_baseline') ~ 'Saw nutritionist/dietician at baseline',
    str_detect(invar, '\\bshape\\b') ~ 'Weibull shape',
    str_detect(invar, 'median1') ~ 'Median time (usual care)',
    str_detect(invar, 'median2') ~ 'Median time (new model of care)',
    str_detect(invar, 'mediandiff') ~ 'Median difference (usual care minus new model)',
    TRUE ~ as.character(invar)
  )
  return(out)
}
