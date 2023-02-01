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
colors = c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")

##########################
##################### data
##########################

## watershed boundary
watershed =
  st_read('store/Miss_RiverBasin/Miss_RiverBasin.shp') %>%
  mutate(aes = 'Mississippi River \nWatershed')

# ## test plot
# watershed %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

## get lakes from nhd
lakes = 
  st_read(dsn = "store/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb",
          layer = "NHDWaterbody") %>%
  dplyr::select(COMID, AREASQKM) %>% 
  st_transform(st_crs(watershed))

## trim lakes to watershed
## start clock
start.filter.time <- proc.time()
lakes.mrb = 
  lakes %>% 
  st_filter(watershed, .pred = st_intersect)
## stop clock
filter.time = proc.time() - start.filter.time

## clean house
rm(lakes)
gc()

## get centroids for climate zone intersection
lake.points = 
  lakes.mrb %>% 
  st_centroid

## get climate zones from ipcc
zones = 
  st_read('store/climateMap/ipcc_zones_2017_names.shp') %>% 
  st_transform(lake.points)

# ## test plot
# zones %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

## join waterbody points with climate zones to get climate zone info
## start clock
start.zone.time <- proc.time()
data = 
  left_join(
    lakes.mrb,
    st_join(lake.points, 
            zones %>% 
              st_make_valid, 
            join = st_within) %>% 
      st_drop_geometry %>% 
      as_tibble %>% 
      select(COMID, clmt_zn) %>% 
      rename(climate.zone = clmt_zn),
    by = 'COMID'
  )
## stop clock
zone.time = proc.time() - start.zone.time

## clean house
rm(lake.points)
gc()

## classify as pond or reservoir
data %<>% 
  mutate(type = case_when(AREASQK <  0.08 ~ 'pond',
                          AREASQK >= 0.08 ~ 'reservoir'))
## check
data %>% st_drop_geometry %>% select(type) %>% table

## rename to title case
data %<>% 
  mutate(climate.zone = str_to_title(climate.zone))

## export waterbody type
data %>% 
  st_drop_geometry %>% 
  select(COMID, climate.zone, type) %>% 
  write_csv('store/climate_zones_for_mrb_waterbodies.csv')

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

study_area = 
  ggplot() +
  geom_sf(data  = us_states,
          color = 'black',
          alpha = 0.05) + 
  geom_sf(data  = watershed,
          aes(color = aes,
              fill  = aes),
          alpha = 0.1) + 
  geom_sf(data  = data,
          aes(color = climate.zone,
              fill  = climate.zone)) + 
  geom_sf_text(data = states, 
               aes(label = stusps)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  north(data     = states,
        location = "topright") + 
  scalebar(data      = states,
           location  = "bottomright",
           transform = T,
           dist      = 50,
           dist_unit = "mi") +
  labs(color = '',
       fill  = '',
       label = '') +
  theme_void() +
  theme(legend.position = c(0.89, 0.3),
        legend.key = element_rect(color=NA))

## draw study area inset 
ggdraw() +
  draw_plot(study_area) +
  draw_plot(region, x = 0.05, y = 0.55, width = 0.35, height = 0.35)

## export
ggsave("output/figures/climate_zones_mrb.png", width  = 6, height = 8)

## check times
print('time it took to filter to watershed: ')
filter.time

print('time it took to extract climate zones: ')
zone.time

## end of script. have a great day! 