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