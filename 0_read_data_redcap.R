# 0_read_data_redcap.R
# read the LOCATE data from REDCap using the API
# November 2020
library(dplyr)
library(tidyr)
library(janitor)
library(RCurl) # for making the API request
library(jsonlite) # for converting JSON data
library(stringr)
library(eq5d)
source('99_functions.R')
source('API/config.R') # for key

## Section 1: data dictionary ##
dictionary = read.csv('data/LOCATENAFLD_DataDictionary_2020-11-28.csv', stringsAsFactors = FALSE) %>%
  clean_names()
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

## Section 2: get data from REDCap via API ##
result = postForm(
  api_url,
  token = api_token_locate,
  content = 'record',
  format = 'json',
  type = 'flat', # one row per participant
  .opts = list(ssl.verifypeer = TRUE) # see https://redcap.ihbi.qut.edu.au/api/help/?content=security
)

## process and tidy-up the data
data = fromJSON(result) %>%
  clean_names() %>%
  select(-name, -address, -phone_number, -participant_contacted, -interest_to_participate, -patient_gp, -ends_with('complete')) %>% # variable not needed
  mutate(across(any_of(dates), as.Date), # convert dates
         across(any_of(numbers), as.numeric), # convert to numbers
         across(any_of(yes_no), yesno), # convert yes/no
#         across(any_of('hbsag','hepc_igg'), yesno, labels=c('Negative','Positive')), # convert hepatitis variables
         across(any_of(yes_no_unclear), yesnounclear), # convert yes/no/unclear
         across(any_of(drinks), drinks_factor), # convert drinks questions
         referral_cat = factor(referral_cat, levels=1:2, labels=c('First referral','Re-referral')),
         sex = factor(sex, levels=1:2, labels=c('Male','Female')),
         liver_outline = factor(liver_outline, levels=1:3, labels=c('Nodular','Smooth','Not stated')),
         alcohol = factor(alcohol, levels=1:5, labels=c('Non-drinker','Moderate drinker','Heavy drinker','Not mentioned','Other')),
         withdrew_or_excluded = case_when(
           withdrew_or_excluded == '1' ~ 'Excluded',
           withdrew_or_excluded == '2' ~ 'Withdrawn',
           withdrew_or_excluded == '3' ~ 'Neither',
           withdrew_or_excluded == '' ~ 'Neither'),
         exclude_reason = factor(exclude_reason, levels=1:7, labels=c('AUDIT (risky alcohol) or liver problem','Pregnant','Plans to move','Terminal illness','Incomplete consent','Could not be contacted','Other')),
  )


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
  eq5d_b = eq5d(scores.df, country="Australia", version="3L", type="TTO")) %>%
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
# uncomment when there's some data
#scores.df.fu = mutate(scores.df,
#                            eq5d_12m = eq5d(scores.df, country="Australia", version="3L", type="TTO")) %>%
#  select(record_id, eq5d_12m)
# add scores back to data
data = left_join(data, scores.df.baseline, by='record_id')
#data = left_join(data, scores.df.fu, by='record_id') # uncomment when there is data

# save
save(data, file = 'data/redcap_data.RData')
