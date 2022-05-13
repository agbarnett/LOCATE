# 0_basic_numbers.R
# basic numbers about the trial
# April 2022

# weekly targets
target_sample_low = 156 # total sample size
target_sample_high = 234

# work out number of weeks recruitment in each centre
start_logan = as.Date('09-10-2020', format='%d-%m-%Y')
end_logan = as.Date('11-12-2020', format='%d-%m-%Y')
n_weeks_logan = ceiling(as.numeric(end_logan - start_logan) / 7)
start_sunshine = as.Date('04-01-2021', format='%d-%m-%Y')
end_sunshine = as.Date('23-04-2021', format='%d-%m-%Y')
n_weeks_sunshine = ceiling(as.numeric(end_logan - start_logan) / 7)
n_weeks_recruitment = n_weeks_logan + n_weeks_sunshine # total number of weeks of recruitment including Xmas break
