##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'stringr',
                      'sf','USAboundaries',
                      'ggplot2','ggpubr','scales','showtext', 'ggsn', 'cowplot')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
#################### parts
##########################

## colorblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
colors = c("#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000", "#56B4E9")

##########################
##################### data
##########################

## watershed boundary
watershed =
  st_read('store/Miss_RiverBasin/Miss_RiverBasin.shp') %>%
  mutate(aes = 'Mississippi River \nWatershed')

## test plot, looks good
watershed %>%
  ggplot() +
  geom_sf() +
  theme_void()

## get n2o data from Alisa.Keyser@colostate.edu and Stephen.Ogle@colostate.edu in an email on 3/3/2023
n2o.mrb =
  read_csv('store/n2o_in_mrb/MSRBgrid_NO3_wIPCC_2023-03-03.csv', show_col_types = F)[1:7] %>% 
  filter(!is.na(CENTROID_X), !is.na(CENTROID_Y)) %>% 
  st_as_sf(coords = c('CENTROID_X', 'CENTROID_Y'), 
           crs = st_crs(5070)) %>% 
  st_transform(st_crs(watershed))#%>%
# filter(IPCC_Zone_Name %in% c('Warm Temperate Dry', 'Cool Temperate Dry', 'Warm Temperate Moist', 'Cool Temperate Moist')) 

## test plot
n2o.mrb %>%
  ggplot() +
  geom_sf() +
  theme_void()

## trim lakes to watershed
n2o.mrb %<>%
  st_filter(watershed, .pred = st_intersect)

## test plot, looks good
ggplot() +
  geom_sf(data = watershed) +
  geom_sf(data = n2o.mrb %>% sample_n(100)) +
  theme_void()

## get climate zones from ipcc
zones = 
  st_read('store/climateMap/ipcc_zones_2017_names.shp') %>% 
  st_transform(st_crs(watershed)) %>% 
  st_filter(watershed, .pred = st_intersect)

## test plot, looks good 
ggplot() +
  geom_sf(data = zones) +
  geom_sf(data = watershed) +
  geom_sf(data = n2o.mrb %>% sample_n(100)) +
  theme_void()

## join waterbody points with climate zones to get climate zone info
data = 
  st_join(n2o.mrb, 
          zones %>% 
            st_make_valid, 
          join = st_within) %>% 
  dplyr::select(cellID_10km, fldd_zn) %>% 
  rename(climate.zone = fldd_zn)

## check
table(data$climate.zone)

# ## climate.zone
# boreal       cool temperate tropical dry/montane   tropical moist/wet   warm temperate dry warm temperate moist 
# 30                14200                   27                 1703                 3982                12912 

## rename to title case
data %<>% 
  mutate(climate.zone = str_to_title(climate.zone))

## there are 27 NA for climate zones, they are all off the coast of Louisiana
data %<>% filter(!is.na(climate.zone))

## export waterbody type
data %>% 
  st_drop_geometry %>% 
  write_csv('store/climate_zones_for_mrb_n2o_gridcells.csv')

## get state polygons
us_states = subset(USAboundaries::us_states(),
                   !name %in% c("United States Virgin Islands",
                                "Commonwealth of the Northern Mariana Islands",
                                "Guam",
                                "American Samoa",
                                "Puerto Rico",
                                "Alaska",
                                "Hawaii")) %>%
  st_transform(crs = st_crs(5070))

##########################
##################### plot
##########################

ggplot() +
  geom_sf(data  = us_states,
          color = 'black',
          alpha = 0.05) + 
  geom_sf(data  = watershed,
          color = "#56B4E9",
          fill  = "#56B4E9",
          alpha = 0.1) +
  geom_sf(data  = data,
          aes(color = climate.zone,
              fill  = climate.zone)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(color = '',
       fill  = '',
       label = '') +
  theme_void() +
  theme(legend.position = 'bottom') + 
  guides(color = guide_legend(nrow = 3),
         fill  = guide_legend(nrow = 3))

## end of script. have a great day! 