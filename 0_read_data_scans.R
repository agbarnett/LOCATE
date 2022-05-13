# 0_read_data_scans.R
# read the data from the Fibroscanner
# May 2022
library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)
source('99_functions.R')

# find all the files and read them in
data_location = 'U:\\Research\\Projects\\ihbi\\aushsi\\locatenafld\\TRIAL\\Data collection\\scans'
to_read = dir(data_location, pattern='xlsx$')
raw_scans = NULL
for (f in to_read){ # used `text` for all to make binding easier
  this_scan = readxl::read_excel(path = paste(data_location, '\\', f, sep=''), col_types='text', sheet='Data') %>%
    mutate(file = str_remove(f, '\\.xlsx')) %>% # add file name
    clean_names() %>%
    mutate(height = as.numeric(height),
           weight = as.numeric(weight)
    )
  
  # name check
  name_check = function(){
    n = names(this_scan)
    index = grep(pattern='^e_', x=n)
    index = grep(pattern='^cap_', x=n)
    e = n[index]
    cat(f, '\n')
    cat(paste(e, collapse='\n'), '\n\n')
  }
  
  ## changes based on which type of scanner:
  
  # rename for consistency
  scanner1 = 'valid_measures_number' %in% names(this_scan)
  if(scanner1 == TRUE){
    this_scan = rename(this_scan, 
                       'total_measurement_number' = 'valid_measures_number')
  }
  # rename for consistency
  scanner1 = 'e_med_k_pa' %in% names(this_scan)
  if(scanner1 == TRUE){
    this_scan = rename(this_scan, 
                       'e_median_k_pa' = 'e_med_k_pa',
                       'e_iqr_median_percent' = 'e_iqr_e_med')
  }
  # rename for consistency
  scanner1 = 'cap_med_d_b_m' %in% names(this_scan)
  if(scanner1 == TRUE){
    this_scan = rename(this_scan, 
                       'cap_median_d_b_m' = 'cap_med_d_b_m',
                       'cap_iqr_d_b_m' = 'cap_iqr_d_b_m')
  }
  # make numbers
  this_scan = mutate(this_scan,
    e_median_k_pa = as.numeric(e_median_k_pa),
    e_iqr_k_pa = as.numeric(e_iqr_k_pa),
    e_iqr_median_percent = as.numeric(e_iqr_median_percent),
    cap_median_d_b_m = as.numeric(cap_median_d_b_m),
    cap_iqr_d_b_m = as.numeric(cap_iqr_d_b_m),
    total_measurement_number = as.numeric(total_measurement_number)) 
  # makes numbers for extra variables
  scanner1 = 'cap_enhanced_mean_d_b_m' %in% names(this_scan)
  if(scanner1 == TRUE){
    this_scan = mutate(this_scan,
           cap_enhanced_mean_d_b_m = as.numeric(cap_enhanced_mean_d_b_m),
           cap_enhanced_sd_d_b_m = as.numeric(cap_enhanced_sd_d_b_m),
           cap_level_percent = as.numeric(cap_level_percent))
  }

  # concatenate
  raw_scans = bind_rows(raw_scans, this_scan)
  
}

# tidy up scan data
# to do: not yet confirmed that these are all the variables we need
scans = separate(raw_scans, exam_time, into=c('exam_hour','exam_min','exam_sec'), sep=':') %>%
  mutate(
    record_id = coalesce(code, pharma_study_code, pharma_patient_code), # ID number could be in any of these
    # dates
    exam_date = as.Date(ISOdate(year=exam_date_year, month=exam_date_month, day=exam_date_day)),
    exam_datetime = ISOdate(year=exam_date_year, month=exam_date_month, day=exam_date_day, hour=exam_hour, min=exam_min, sec=exam_sec, tz='Australia/Brisbane'),
    exam_duration_min = as.numeric(str_replace_all(exam_duration_min, ',', '.')), # remove comma
    exam_duration_min = ifelse(!is.na(exam_duration_s), as.numeric(exam_duration_s)/60, exam_duration_min), # use exam duration in seconds if available
    #
    height = as.numeric(height),
    height = ifelse(height > 100, height/100, height), # convert cm to m
    weight = as.numeric(weight),
    bmi = weight/(height*height),
    total_measurement_number = as.numeric(total_measurement_number),
    birth_date_year = as.numeric(birth_date_year)) %>%
  separate(record_id, into=c('centre','num'), sep = '-', remove=FALSE) %>% # extract centre number; keep original record id variable
  select('record_id','file','centre','num','gender','height','weight','bmi','birth_date_year',
         'fasting','exam_date', 'exam_datetime','exam_duration_min','exam_type',
         'total_measurement_number', starts_with('e_'), starts_with('cap_')) %>%
  select(-contains('_number_')) %>% # do not need individual readings
  filter(!is.na(record_id)) %>% # remove missing rows
  rename('probe_size' = 'exam_type') %>%
  mutate(probe_size = ifelse(probe_size=='Medium', 'M', probe_size)) %>%
  unique() # remove duplicates

# missing scans - no longer needed
#source('0_missing_scans.R')

# check double scans
tab = table(scans$record_id)
double = names(which(tab>2))
View(filter(scans, record_id == double[4]))

# Some almost duplicate rows with minor differences
# If same patient has two scans, then just use latest and/or with longest exam duration
scans = group_by(scans, record_id) %>%
  select(-file) %>% # source file no longer needed
  arrange(record_id, desc(exam_datetime), desc(exam_duration_min)) %>% # desc for latest time and longest duration
  ungroup() %>%
  mutate(centre = as.numeric(centre),
         num = as.numeric(num)) 

# save
save(scans, file = 'data/0_scans.RData')


