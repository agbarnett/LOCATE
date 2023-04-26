# 1_combine_data.R
# combine the REDCap and scan data
# April 2022

# not yet finished.

# is this needed? could keep separate ### 
# I don't think this is needed so delete eventually.

# get the data
load('data/0_scans.RData') # from 0_read_data_scans.R
load('data/0_redcap_data.RData') # from 0_read_data_redcap.R

# save the data
save(dictionary, , file='data/1_AnalysisReady.RData')
