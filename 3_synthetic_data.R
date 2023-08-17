# 3_synthetic_data.R
# make synthetic data for public sharing on github
# use method described here: https://elifesciences.org/articles/53275
# August 2023
library(stringr)
library(dplyr)
library(openxlsx)

# Load required packages
packages <- c("synthpop", "tidyverse", "cowplot", "car",
              "simstudy", "mice", "StatMeasures")
lapply(packages, require, character.only = TRUE)

### get the analysis data
load('data/0_scans.RData')
load('data/1_redcap_data.RData')

### Create synthetic data ###
seed = TeachingDemos::char2seed('peterborough')
# scans
ids = select(scans, "record_id","centre","num") # remove IDs, to add back
scans = select(scans, -"record_id", -"centre", -"num")
scans_sim <- syn(scans, seed = seed) # 
scans_sim = bind_cols(ids, scans_sim$syn) %>% # add back IDs
  mutate(bmi = weight/(height^2))
# health data
ids = select(data, "record_id","centre","num") # remove IDs, to add back
data = select(data , -reason_non_participate, -other_referral, -alcohol_other, # too much free text to simulate; could be identifying data too
              -employment_other, -study_description, -medications_description, 
              -ed_reason_1, -specialist_assessment, -other_notes, 
              -month_follow_up_notes, -other_appointments, -withdrew_reason_other, -excluded_reason_other,
              -doctor_liver, -hospital_admission_number , -death, # all missing
              -ed_number, # not used
              -morning_drinking ) # all zero
# functions needed for dates below
back.date = function(x){
  as.Date(x, origin='1970-01-01')
}
min.date = function(x){
  pmin(x, 19347) # stop dates being well outside observed range (using numeric dates)
}
#
data = select(data, -"record_id", -"centre", -"num") %>%
  mutate_at(vars(contains('date'), contains('ed_pres_')), .funs = as.numeric) # all dates to numeric
data_sim <- syn(data, seed = seed) # 
data_sim = bind_cols(ids, data_sim$syn) %>% # add back IDs
  mutate(morning_drinking = 0,
         bmi = weight/((height/100)^2)) %>%
  mutate_at(vars(contains('date'), contains('ed_pres_')), .funs = min.date) %>%
  mutate_at(vars(contains('date'), contains('ed_pres_')), .funs = back.date)
# row number problem with review_date

## quick checks comparing real and synthetic data
# scans
summary(scans_sim[,5:8])
summary(scans[,(5:8)-3]) # -3 due to IDs
with(scans_sim, table(gender, fasting))
with(scans, table(gender, fasting))
# data
vars = c('age','ast','height','date_withdrew')
summary(data_sim[,vars])
summary(data[,vars]) 
with(data_sim, table(sex, randomised))
with(data, table(sex, randomised))

## export
# R
save(data_sim,
     scans_sim,
     dictionary,
    file = 'data/synthetic.RData')

# excel
#
wb <- createWorkbook("Barnett")
#
addWorksheet(wb, sheetName = "scans")
writeData(wb, sheet=1, x=scans_sim) # synthetic
#
addWorksheet(wb, sheetName = "data")
writeData(wb, sheet=2, x=data_sim)
#
addWorksheet(wb, sheetName = "dictionary")
writeData(wb, sheet=3, x=dictionary)
#
saveWorkbook(wb, file = "data/synthetic.xlsx", overwrite = TRUE)
