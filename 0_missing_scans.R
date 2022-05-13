# 0_missing_scans.R
# check that patients randomised have scans (checking for missing scans)
# used when the trial was running to flag missing scans - no longer needed
# called from 0_read_data_scans.R
# Oct 2021

### Section 1 missing scans ###

## Excluded
# exported from REDCap as csv (text) using specific form
to_read = dir('data', pattern='LOCATENAFLD-Excluded')
infile = paste('data/', to_read, sep='')
excluded = read.csv(infile, stringsAsFactors = FALSE) %>%
  filter(withdrew_or_excluded == 1)

## Randomised groups:
# exported from REDCap as csv (text) using specific form
to_read = dir('data', pattern='LOCATENAFLD-RandomisedGroup_DATA')
infile = paste('data/', to_read, sep='')
group = read.csv(infile, stringsAsFactors = FALSE) %>%
  mutate(date_today = as.Date(date_today)) %>%
  filter(randomised == 1) %>%
  anti_join(excluded, by='record_id') %>% # not excluded (no match in excluded)
  separate(record_id, into=c('centre','num'), sep = '-', remove=FALSE) %>% # extract centre number; keep original record id variable
  mutate(centre = as.numeric(centre),
         num = as.numeric(num))

# randomised to scan, but no scan
anti_join(group, scans, by=c('record_id','centre','num')) %>%
  mutate(centrename = centre_name(centre)) %>%
  select(centrename, num, date_today)

# not randomised, but scan
anti_join(scans, group, by=c('record_id','centre','num')) %>%
  mutate(centrename = centre_name(centre)) %>%
  select(centrename, centre, num, exam_date)

## info:
# Subject 123-42 did not attend her study visit as well as her clinic visit
# Subject 122-114 withdrew (this is in REDCap)

### Section 2 check with REDCap data ###
# check age and sex match (i.e., check it is the same patients)
v = inner_join(scans, group, by=c('record_id','centre','num')) %>%
  mutate(centrename = centre_name(centre)) %>%
  select(centrename, centre, num, exam_date, gender, sex_referral, birth_date_year, year_born_referral )
#View(v)
with(v, table(gender, sex_referral, useNA ='always'))
mutate(v, diff = birth_date_year - year_born_referral) %>%
  filter(diff !=0 | is.na(diff))


### Section 3 missing information ###
# exam date
filter(scans, is.na(exam_date)) %>%
  select(record_id, file)
# gender
filter(scans, is.na(gender)) %>%
  select(record_id, file)
# year born
filter(scans, is.na(birth_date_year)) %>%
  select(record_id, file)
# height
filter(scans, is.na(height)) %>%
  select(record_id, file)
