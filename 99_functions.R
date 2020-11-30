# 99_functions.R
# November 2020

### Functions use by 0_read_data_redcap.R to format data ###

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