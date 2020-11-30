# 0_read_data_scans.R
# read the data from the Fibroscanner
# November 2020
library(readxl)
library(dplyr)
library(janitor)

data_location = 'U:\\Research\\Projects\\ihbi\\aushsi\\locatenafld\\TRIAL\\Data collection\\scans'
to_read = dir(data_location, pattern='xlsx')

raw_scans = NULL
for (f in to_read){
  this_scan = readxl::read_excel(path=paste(data_location,'\\', to_read, sep=''), sheet='Data')
  raw_scans = bind_rows(raw_scans, this_scan)
}

# tidy up
# not confirmed that these are all the variables we need
# to do: more to convert to number
scans = clean_names(raw_scans) %>%
  mutate(exam_date = as.Date(ISOdate(year=exam_date_year, month=exam_date_month, day=exam_date_day)),
         height = as.numeric(height),
         weight = as.numeric(weight),
         bmi = weight/(height*height),
         total_measurement_number = as.numeric(total_measurement_number),
         birth_date_year = as.numeric(birth_date_year)) %>%
  rename('record_id'='pharma_study_code') %>% # rename to match REDcap
  select('record_id','gender','height','weight','bmi','birth_date_year','fasting','exam_date', 'exam_time','exam_duration_min','exam_type',
         'total_measurement_number', starts_with('e_'), starts_with('cap_'))

# save
save(scans, 'data/scans.RData')
