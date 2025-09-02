# 2_map_postcodes.R
# map of GP and patient postcodes for implementation paper
# shape file downloaded from https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
# November 2023
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggspatial) # for scalebar and north arrow on map

### data ###
## get the map data
fname = "data/POA_2021_AUST_GDA2020_SHP/POA_2021_AUST_GDA2020.shp"
postcodes = st_read(fname) # layer is POA_2021_AUST_GDA2020

## get the patient and GP data
# GPs
postcode_gp = read.csv('results/postcodes.csv') %>% # from 1_read_data_redcap.R
  mutate(postcode = as.character(postcode)) %>% # to suit other data
  filter(!is.na(postcode)) %>%
  rename('gp' = 'n')
# patients
postcode_patient = read.csv('results/postcodes_patients.csv') %>% # from 1_read_data_redcap.R
  mutate(postcode = as.character(postcode)) %>% # to suit other data
  rename('patient' = 'n')
postcode_numbers = full_join(postcode_gp, postcode_patient, by='postcode')

# re-arrange data for ggplot
for_ggplot <-  select(postcodes, POA_CODE21, geometry) %>% 
  gather(VAR, SID, -geometry) %>%
  rename('postcode' = 'SID')
# add numbers
for_ggplot = full_join(for_ggplot, postcode_numbers, by='postcode') %>%
  mutate(gp = ifelse(is.na(gp), 0, gp),
         patient = ifelse(is.na(patient), 0, patient)) %>%
  filter(!is.na(VAR)) # postcode 4072 does not exist in map data

## maps
long_limits = c(-25.3, -28.25) # long and lat limits
lat_limits = c(152.15, 153.6)
# GPs
gp_map = ggplot() + 
  geom_sf(data = for_ggplot, aes(fill = gp)) + 
  scale_fill_gradient('Number\nof GPs', low='white', high='darkred')+
  theme(legend.position = 'right',
        plot.margin = margin(1, 1, 1, 1, "mm"),
        legend.margin = margin(1, 1, 1, 1, "mm"))+
  theme_bw()+
  scale_x_continuous(breaks = seq(152,153.5,0.5))+ # fewer ticks
  scale_y_continuous(breaks = seq(-29,-24, 1))+
  annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"))+
  annotation_north_arrow(location = "tl", which_north = "true",
    pad_x = unit(0.01, "in"), pad_y = unit(0.1, "in"),
    style = north_arrow_minimal())+
  coord_sf(xlim = lat_limits, ylim = long_limits, clip='on') # focus on SE Queensland
#gp_map
# patients
patient_map = ggplot() + 
  geom_sf(data = for_ggplot, aes(fill = patient)) + 
  scale_fill_gradient('Number\nof\npatients', low='white', high='navyblue')+
  theme(legend.position = 'right',
        plot.margin = margin(1, 1, 1, 1, "mm"),
        legend.margin = margin(1, 1, 1, 1, "mm"))+
  theme_bw()+
  scale_x_continuous(breaks = seq(152,153.5,0.5))+ # fewer ticks
  scale_y_continuous(breaks = seq(-29,-24,1))+
  annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"))+
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.01, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_minimal())+
  coord_sf(xlim = lat_limits, ylim = long_limits, clip='on')  # focus on SE Queensland
#patient_map

# export to jpeg
jpeg('figures/maps.jpg', width=7, height=5, units='in', res=600, quality=100)
grid.arrange(patient_map, gp_map, ncol=2, nrow=1)
dev.off()

