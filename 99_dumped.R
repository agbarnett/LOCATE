# 99_dumped.R

```{r}
# exclude variables that cannot be missing
for_missing = select(data,
                     -record_id, -centre, -centrename, -num, -redcap_data_access_group, -date_today, -date_returned, -randomised, -reason_non_participate, -month_follow_up_notes, 
                     -contains('_12'), -contains('other'), -doctor_liver, -contains('withdrew'), -contains('exclude'), -referral_time)
vis_miss(for_missing) +
  ylab('Participant') +
  coord_flip()
```


# exclude variables that cannot be missing
for_missing = select(data,
                     -record_id, -centre, -centrename, -num, -redcap_data_access_group, -date_today, -date_returned, -randomised, -reason_non_participate, -month_follow_up_notes, 
                     -contains('_12'), -contains('other'), -doctor_liver, -contains('withdrew'), -contains('exclude'), -referral_time)
vis_miss(for_missing) +
  ylab('Participant') +
  coord_flip()
