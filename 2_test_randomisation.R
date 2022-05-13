# 2_test_randomisation.R
# graphical test of overall randomisation, using my code for fraud test
# April 2022
library(nimble) # for Bayesian model

# source the fraud functions
source('C:\\Users\\barnetta\\OneDrive - Queensland University of Technology\\fraud\\baseline_tables\\baseline\\99_functions.R')

## arrange summary data to match fraud functions
# continuous
stats1 = data.frame(N1 = tab1$ContTable$`New model of care`[,1],
                     miss1 = tab1$ContTable$`New model of care`[,2],
                     m1 = tab1$ContTable$`New model of care`[,4],
                     sd1 = tab1$ContTable$`New model of care`[,5]) %>%
  mutate(n1 = N1 - miss1) %>%
  tibble::rownames_to_column(var = "v1")
stats2 = data.frame(N2 = tab1$ContTable$`Usual care`[,1],
                    miss2 = tab1$ContTable$`Usual care`[,2],
                    m2 = tab1$ContTable$`Usual care`[,4],
                    sd2 = tab1$ContTable$`Usual care`[,5]) %>%
  mutate(n2 = N2 - miss2) %>%
  tibble::rownames_to_column(var = "v1")
continuous = full_join(stats1, stats2, by='v1')
## categorical
# need to loop through variables as they are in a list
variables = names(tab1$CatTable$`New model of care`)
cat1 = cat2 = NULL
for (v in variables){
  this_frame1 = tab1$CatTable$`New model of care`[[v]]
  this_frame1 = this_frame1[-1,] # remove reference row
  to_process1 = mutate(this_frame1, v1 = paste(v, level)) %>%
      select(v1, n, miss, freq) %>%
    mutate(freq = as.numeric(freq))
  cat1 = bind_rows(cat1, to_process1)
  #
  this_frame2 = tab1$CatTable$`Usual care`[[v]]
  this_frame2 = this_frame2[-1,] # remove reference row
  to_process2 = mutate(this_frame2, v1 = paste(v, level)) %>%
    select(v1, n, miss, freq)%>%
    mutate(freq = as.numeric(freq))
  cat2 = bind_rows(cat2, to_process2)
}
cat1 = mutate(cat1, N1 = n - miss) %>%
  rename('n1' = 'freq') %>%
  select(v1, n1, N1)
cat2 = mutate(cat2, N2 = n - miss) %>%
  rename('n2' = 'freq') %>%
  select(v1, n2, N2)
cat = full_join(cat1, cat2, by='v1')

# now get t-statistics
table_data = list()
table_data$continuous = continuous
table_data$percents = cat
tstats.p = t.stats.percents(indata = table_data$percents)
tstats.c = t.stats.continuous(indata = table_data$continuous)

# 2) get t-statistics for both statistics types
tstats.c = tstats.p = NULL
if(is.null(table_data$continuous) == FALSE){
  tstats.c = t.stats.continuous(indata = table_data$continuous)
}
if(is.null(table_data$percents) == FALSE){
  tstats.p = t.stats.percents(indata = table_data$percents)
}
# 
tstats = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
  mutate(study = 1) # dummy study number

# 3) make simulated data
n.sims = 100
for (k in 1:n.sims){
  tstats.sim = make_sim(table_data)
  
  # get t-statistics for both statistics types
  tstats.c = tstats.p = NULL
  if(is.null(table_data$continuous) == FALSE){tstats.c = t.stats.continuous(indata = tstats.sim$continuous)} 
  if(is.null(table_data$percents) == FALSE){tstats.p = t.stats.percents(indata = tstats.sim$percents)}
  tstats.sim = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
    mutate(study = k+1) # dummy study number
  tstats = bind_rows(tstats, tstats.sim) # add to overall data
}
# summary(tstats) # quick check

# 4) run Bayesian model
outfile = 'bayes_test_random.RData'
exists = length(dir('results', outfile)) > 0 # to save time, check if results exist already
if(exists == FALSE){
  results = run_bayes_test(in_data = tstats)
  save(results, file=paste('results/', outfile, sep='')) # to save time
}
if(exists == TRUE){
  load(paste('results/', outfile, sep=''))
}

# 5) draw the CDF
## first create a median
# create all CDFs
average = filter(tstats, study > 1) %>% # just simulations
  group_by(study) %>%
  mutate(cdf = ecdf(t)(t)) %>% # CDF per study
  ungroup() 
# now calculate median CDF
cdf_median = group_by(average, study) %>%
  arrange(study, t) %>%
  mutate(r = rank(t, ties.method = 'first')) %>%
  group_by(r) %>%
  summarise(mid = median(t)) %>%
  ungroup() %>%
  mutate(e = r/n(),
         study = 1) # had to provide study number
# add first point of the CDF
cdf_first = filter(cdf_median, r==1) %>%
  mutate(e = 0, r=0)
cdf_median = bind_rows(cdf_first, cdf_median)

# move trial line to last
tstats = mutate(tstats, study = ifelse(study==1, 999, study))
colours = grey(runif(n = n.sims + 2, min=0.5, max=0.9)) # grey colours for simulations
colours[1] = 'dodgerblue' # colour for median
colours[n.sims + 2] = 'indianred1' # colour for trial
sizes = rep(1, n.sims + 2)
sizes[c(1,n.sims + 2)] = 2 # median and trial are larger
# plot
tplot = ggplot(data=tstats, aes(x=t, size=factor(study), colour=factor(study))) +
  theme_bw()+
  scale_size_manual(values = sizes)+
  scale_color_manual(values = colours)+
  stat_ecdf()+
  geom_step(data=cdf_median, aes(x=mid, y=e))+ # simulation average CDF
  xlab('t-statistic')+
  ylab('Cumulative density')+
  theme(legend.position = 'none',
        panel.grid.minor = element_blank())+
  geom_segment(aes(x = 1.5, y = 0.5, xend = 0.9, yend = 0.6), colour='black', lwd=1, arrow = arrow(length = unit(0.03, "npc"))) # to highlight flat region

# export image
filename = 'results/cumulative_randomisation.jpg'
jpeg(filename, width=5, height=4, units='in', res=400, quality=100)
print(tplot)
dev.off()
