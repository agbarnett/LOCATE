# 0_read_data_redcap.R
# read the LOCATE data from REDCap using the API; calculate additional variables
# April 2022
library(dplyr)
library(tidyr)
library(janitor)
library(redcapAPI) # to directly get data from REDCap
library(RCurl) # for making the API request
library(jsonlite) # for converting JSON data
library(stringr)
library(eq5d)
library(openxlsx) # for exporting data for checking
source('99_functions.R')
source('API/config.R') # for key

## Section 1: redcap data dictionary ##
dictionary = read.csv('data/LOCATENAFLD_DataDictionary_2020-11-28.csv', stringsAsFactors = FALSE) %>%
  clean_names() %>%
  filter(!str_detect(variable_field_name, pattern='_instr_'), # remove instructions that are not variables
         !variable_field_name == 'audit_score_result') # remove this derived variable
# get variables of particular types:
dates = filter(dictionary, text_validation_type_or_show_slider_number=='date_dmy') %>% pull(variable_field_name)
numbers1 = filter(dictionary, text_validation_type_or_show_slider_number=='number') %>% pull(variable_field_name)
numbers2 = filter(dictionary, str_detect(string=variable_field_name, pattern='eq5d_')) %>% pull(variable_field_name)
numbers = union(numbers1, numbers2)
yes_no1 = filter(dictionary, choices_calculations_or_slider_labels=='1, Yes | 0, No') %>% pull(variable_field_name)
yes_no2 = filter(dictionary, field_type=='yesno') %>% pull(variable_field_name)
yes_no = c(yes_no1, yes_no2)
yes_no_unclear = filter(dictionary, choices_calculations_or_slider_labels=='1, Yes | 0, No | 2, Unclear') %>% pull(variable_field_name)
drinks = filter(dictionary, choices_calculations_or_slider_labels=='0, Never | 1, Less than monthly | 2, Monthly | 3, Weekly | 4, Daily or almost daily') %>% pull(variable_field_name)
audit = c('how_often','standard_drinks_day','six_or_more','not_stop_once_started','failed_normal_expectations','morning_drinking','guilt_remorse','unable_to_remember','you_someone_else_injured','relative_hp_concerned')

## Section 2: get data from REDCap via API ##
rcon = redcapConnection(url = api_url, token = api_token_locate)
result = exportRecords(rcon, forms = NULL) 

## process and tidy-up the data
data = clean_names(result) %>%
  select(-name, -address, -phone_number, -participant_contacted, -interest_to_participate, -patient_gp, -ends_with('complete'), -sex_referral, -pregnant, -leave_area) %>% # variable not needed; sex_referral is same as sex; no patients were pregnant; none leave area
  separate(record_id, into=c('centre','num'), sep = '-', remove=FALSE) %>% # extract centre number; keep original record id variable
  mutate(centrename = centre_name(centre),
         year_referral = as.numeric(format(date_of_referral, '%Y')),
         year_today = as.numeric(format(date_today, '%Y'))) %>%
  mutate(referral_time = as.numeric(date_returned - date_of_referral), # time between randomised and GP referral
         bmi_calc = weight / ((height/100)^2), # calculated BMI
         bmi_calc = as.numeric(bmi_calc), # remove label
         body_mass_index = as.numeric(body_mass_index), # remove label
         bmi = coalesce(body_mass_index, bmi_calc), # is sometimes missing for both, so combine two sources
         year_now = coalesce(year_referral, year_today),
         age = year_now - year_born_baseline,
         age = as.numeric(age)) %>% # remove label
  select(-starts_with('year_'), -body_mass_index, -bmi_calc, -audit_score_result) # drop some variables not needed
## combine two tick box results into one variable from referral letter
data = mutate(data,
              bmi_6m = case_when(
                bmi_6m_0 == 'Checked' ~ 1,
                bmi_6m_1 == 'Checked' ~ 0,
                bmi_6m_0 == 'Unchecked' & bmi_6m_1 == 'Unchecked' ~ NA_real_
              ),
              fbc_6m = case_when(
                fbc_6m_0 == 'Checked' ~ 1,
                fbc_6m_1 == 'Checked' ~ 0,
                fbc_6m_0 == 'Unchecked' & fbc_6m_1 == 'Unchecked' ~ NA_real_
              ),
              liver_6m = case_when(
                liver_6m_0 == 'Checked' ~ 1,
                liver_6m_1 == 'Checked' ~ 0,
                liver_6m_0 == 'Unchecked' & liver_6m_1 == 'Unchecked' ~ NA_real_
              ),
              hep_6m = case_when(
                hep_6m_0 == 'Checked' ~ 1,
                hep_6m_1 == 'Checked' ~ 0,
                hep_6m_0 == 'Unchecked' & hep_6m_1 == 'Unchecked' ~ NA_real_
              ),
              iron_6m = case_when(
                iron_6m_0 == 'Checked' ~ 1,
                iron_6m_1 == 'Checked' ~ 0,
                iron_6m_0 == 'Unchecked' & iron_6m_1 == 'Unchecked' ~ NA_real_
              ),
              us_6m = case_when(
                us_6m_0 == 'Checked' ~ 1,
                us_6m_1 == 'Checked' ~ 0,
                us_6m_0 == 'Unchecked' & us_6m_1 == 'Unchecked' ~ NA_real_
              )) %>%
  select(-ends_with('_0'), -ends_with('_1'))
# convert variables
data = mutate(data,
         across(any_of(dates), as.Date), # convert dates
         across(any_of(numbers), as.numeric), # convert to numbers
         across(any_of(yes_no), yesno), # convert yes/no
         how_often = audit_score(how_often), # audit not working in `across`
         standard_drinks_day = audit_score(standard_drinks_day),
         six_or_more = audit_score(six_or_more),
         not_stop_once_started = audit_score(not_stop_once_started),
         failed_normal_expectations = audit_score(failed_normal_expectations),
         morning_drinking = audit_score(morning_drinking),
         guilt_remorse = audit_score(guilt_remorse),
         unable_to_remember = audit_score(unable_to_remember),
         you_someone_else_injured = audit_score(you_someone_else_injured),
         relative_hp_concerned = audit_score(relative_hp_concerned)
)

## export liver size for hand cleaning
export_liver = function(){
  export = select(data, record_id, liver_size) %>%
    filter(!is.na(liver_size),
           liver_size != 'not stated') %>%
    mutate(size = NA)
  wb <- createWorkbook()
  addWorksheet(wb, 1)
  writeData(wb, sheet = 1, export)
  saveWorkbook(wb, "data/liver_size.xlsx", overwrite = TRUE)
}
# to do: read in consistent liver data once it has been completed


## AUDIT alcohol score
audit_score = select(data, record_id, all_of(audit)) %>%
  mutate_if(is.factor, as.numeric) %>%
  pivot_longer(cols=all_of(audit)) %>%
  filter(!is.na(value)) %>% # ignore missing, create score from what is available
  group_by(record_id) %>%
  summarise(n= n(), audit = sum(value)) %>%
  ungroup() %>%
  filter(n >= 4) %>% # must have answered 4 or more questions (most were fully complete)
  select(-n)
data = left_join(data, audit_score, by='record_id')

## calculate EQ-5D-3L 
# a) at baseline
scores.df = select(data, record_id, contains('eq5d')) %>%
  rename('MO' = 'eq5d_mb_b', # mobility
         'SC' = 'eq5d_sc_b', # self-care
         'UA' = 'eq5d_ua_b', # usual activities
         'PD' = 'eq5d_pd_b', # pain/discomfort
         'AD' = 'eq5d_ad_b') %>% # anxiety/depression
  select(record_id, MO, SC, UA, PD, AD) %>%
  drop_na() # remove all missing
scores.df.baseline = mutate(scores.df,
  eq5d_b = eq5d(scores.df, country="Australia", version="3L", type="TTO"),
  eq5d_b = eq5d_b*100 #  make eq-5d on 0 to 100 scale
  ) %>%
  select(record_id, eq5d_b)
# b) at follow-up
scores.df = select(data, record_id, contains('eq5d')) %>%
  rename('MO' = 'eq5d_mb_12m', # mobility
         'SC' = 'eq5d_sc_12m', # self-care
         'UA' = 'eq5d_ua_12m', # usual activities
         'PD' = 'eq5d_pd_12m', # pain/discomfort
         'AD' = 'eq5d_ad_12m') %>% # anxiety/depression
  select(record_id, MO, SC, UA, PD, AD) %>%
  drop_na() # remove all missing
# 
scores.df.fu = mutate(scores.df,
      eq5d_12m = eq5d(scores.df, country="Australia", version="3L", type="TTO"),
      eq5d_12m = eq5d_12m*100 #  make eq-5d on 0 to 100 scale
    ) %>%
  select(record_id, eq5d_12m)
# add scores back to data
data = left_join(data, scores.df.baseline, by='record_id')
data = left_join(data, scores.df.fu, by='record_id') # 

## fixes to exclusions and a few other small edits
data = mutate(data,
  record_id = as.character(record_id), # remove label for merging
  # randomised but excluded - set randomised to blank
  randomised = as.character(randomised),
  randomised = ifelse(!is.na(randomised) & !is.na(exclude_reason), NA, randomised),
  # a few more who never returned questionnaire but not marked in REDCap
  exclude_reason = as.character(exclude_reason),
  exclude_reason = ifelse(is.na(randomised) == TRUE & withdrew_or_excluded ==TRUE, 'Could not be contacted' , exclude_reason),
  withdrew_reason = as.character(withdrew_reason),
  reason = coalesce(exclude_reason, withdrew_reason),
  reason = ifelse(withdrew_or_excluded == 'Neither', 'Could not be contacted', reason)) # add people who could not be contacted


## save ##
save(data, dictionary, file = 'data/0_redcap_data.RData')

#
# filter(data, centre=='122', randomised==1) %>% pull(record_id)