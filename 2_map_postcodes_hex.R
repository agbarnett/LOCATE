# 2_map_postcodes_hex.R
# map of GP and patient postcodes for implementation paper
# version using hex pattern from sugarbag
# shape file downloaded from https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
# December 2023
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggspatial) # for scalebar and north arrow on map
library(sugarbag) # for hex
library(gganimate)
library(transformr)
library(stringr)
library(janitor)
library(readxl)

### data ###
## a) get the patient and GP data
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

## b) get the map data
fname = "data/POA_2021_AUST_GDA2020_SHP/POA_2021_AUST_GDA2020.shp"
postcodes = st_read(fname) %>% # layer is POA_2021_AUST_GDA2020
  filter(str_detect(POA_CODE21, pattern='^4')) %>% # starts with 4 for Qld
  rename('postcode' = 'POA_CODE21') %>%
  select(-LOCI_URI21)

## now use sugarbag for hex and animation
## see example https://cran.r-project.org/web/packages/sugarbag/vignettes/Tasmania.html

# Find the longitude and latitude centroid for each region or area
centroids <- create_centroids(postcodes, sf_id = "postcode")
# make hexagon grid, had to expand buffer from 1.2
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.8)
# Allocate the centroids to the hexagon grid
# We have the same amount of rows, as individual regions
hex_allocated <- allocate(centroids = centroids,
                          sf_id = "postcode",
                          hex_grid = grid,
                          hex_size = 0.2, # same size used in create_grid
                          hex_filter = 10,
                          focal_points = capital_cities,
                          # same column used in create_centroids
                          width = 30, 
                          verbose = TRUE) 

hexagons <- hex_allocated %>%
  fortify_hexagon(hex_size = 0.2, sf_id = "postcode") %>%
  left_join(., postcodes, by='postcode') %>% 
  mutate(poly_type = "Hexagonal")

polygons <- fortify_sfc(postcodes) %>% 
  mutate(poly_type = "Geographical")

## check that hexagons fit in the trimmed map area
# long and lat limits
lat_limits = c(-28.7, -25.3) # bottom to top
long_limits = c(151.9, 154.5) # left to right
ggplot(mapping = aes(fill = postcode)) +
  geom_polygon(data = polygons, aes(x=long, lat, group = interaction(postcode, polygon)), alpha = 0.4) +
  geom_polygon(data = hexagons, aes(x=long, lat, group = interaction(postcode))) + 
  scale_fill_viridis_d() +
  coord_sf(xlim = long_limits, ylim = lat_limits, clip='on')+  # focus on SE Queensland
  theme(legend.position = 'none') 

#
hexagon_points <- hexagons %>% 
  select(postcode, long, lat, poly_type) %>% # dropped SA4 here
  left_join(polygons %>% distinct(postcode, polygon), by = "postcode")
polygon_points <- polygons %>% 
  select(postcode, long, lat, polygon, poly_type)

## concatenate and add outcome data to plot
# a) GPs
animate_seq <- bind_rows(hexagon_points, polygon_points) %>%
   left_join(postcode_gp, by = "postcode") %>% # add key to plot data here
  rename('n' = 'gp') %>%
  mutate(n = ifelse(is.na(n), 0, n))
outjpeg = 'figures/side_by_side_gp.jpg'
outgif = "figures/seq_animation_gp.gif"
highcolour = 'navy'
legend.label = 'Number\nof GPs'
# b) patients
animate_seq <- bind_rows(hexagon_points, polygon_points) %>%
  left_join(postcode_patient, by = "postcode") %>% # add key to plot data here
  rename('n' = 'patient') %>%
  mutate(n = ifelse(is.na(n), 0, n))
outjpeg = 'figures/side_by_side_patient.jpg'
outgif = "figures/seq_animation_patient.gif"
highcolour = 'darkred'
legend.label = 'Number\nof\npatients'

# show two maps side by side
side_by_side = animate_seq %>%
  ggplot(aes(x=long, y=lat, group = interaction(polygon, postcode))) +
  geom_polygon(aes(fill = n)) +
  geom_polygon(data = polygon_points %>% select(-poly_type), fill = "grey40", alpha = 0.05) + 
  coord_equal(xlim = long_limits, ylim = lat_limits, clip='on') + # narrow location
  theme_void() + 
  theme(legend.position = "right",
        strip.text = element_text(size = 13, margin = margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")) # more room for facet titles
  )+
  facet_wrap(~poly_type) + 
  scale_fill_continuous(legend.label, low='white', high=highcolour) 
#
jpeg(outjpeg, width=7, height=5, units='in', res=600, quality=100)
print(side_by_side)
dev.off()

#
animation <- animate_seq %>% 
  ggplot(aes(x=long, y=lat, group = interaction(polygon, postcode))) +
  geom_polygon(aes(fill = n)) +
  geom_polygon(data = polygon_points %>% select(-poly_type), fill = "grey40", alpha = 0.05) + 
  coord_equal(xlim = long_limits, ylim = lat_limits, clip='on') + 
  theme_void() + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(legend.position = "none") +
  transition_states(states = poly_type) + 
  scale_fill_continuous(low='white', high=highcolour)
#
animated <- animate(animation, fps = 10, duration = 14, 
                    start_pause = 3, end_pause = 3, rewind = FALSE) # takes a while
#
anim_save(filename = outgif, animated)
