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

# ## test plot
# watershed %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

# ## get lakes from nhd
# lakes = 
#   st_read(dsn = "store/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb",
#           layer = "NHDWaterbody") %>%
#   dplyr::select(COMID, AREASQKM) %>% 
#   st_transform(st_crs(watershed))
# 
# ## trim lakes to watershed
# ## start clock
# start.filter.time <- proc.time()
# lakes.mrb = 
#   lakes %>% 
#   st_filter(watershed, .pred = st_intersect)
# ## stop clock
# filter.time = proc.time() - start.filter.time
# 
# ## check computation time, took about 1.5 hours to filter lakes only in the mrb
# print('time it took to filter to watershed: ')
# filter.time
# 
# ## clean house
# rm(lakes)
# gc()
# 
# ## get centroids for climate zone intersection
# lake.points = 
#   lakes.mrb %>% 
#   st_centroid
# 
# ## export
# lakes.mrb %>% st_write('store/ndh_in_mrb/nhd_lakes_in_mrb.shp')
# lake.points %>% st_write('store/ndh_in_mrb/nhd_lakes_in_mrb_as_points.shp')

## read from above
lakes.mrb = st_read('store/ndh_in_mrb/nhd_lakes_in_mrb.shp')
lake.points = st_read('store/ndh_in_mrb/nhd_lakes_in_mrb_as_points.shp')

## get climate zones from ipcc
zones = 
  st_read('store/climateMap/ipcc_zones_2017_names.shp') %>% 
  st_transform(st_crs(lake.points))

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
      dplyr::select(COMID, fldd_zn) %>% 
      rename(climate.zone = fldd_zn),
    by = 'COMID'
  )
## stop clock
zone.time = proc.time() - start.zone.time

## check computation times, took about 3 minutes to extract climate zones
print('time it took to extract climate zones: ')
zone.time

## clean house
rm(lake.points, lakes.mrb)
gc()

## classify as pond or reservoir
data %<>% 
  mutate(type = case_when(AREASQKM <  0.08 ~ 'pond',
                          AREASQKM >= 0.08 ~ 'reservoir'))

## check
data %>% st_drop_geometry %>% dplyr::select(type, climate.zone) %>% table

# climate.zone
# type        boreal cool temperate tropical dry/montane tropical moist/wet warm temperate dry warm temperate moist
# pond         129          72293                  170              16452              11972                47209
# reservoir     23          29300                   22               7540               2773                10553

## rename to title case
data %<>% 
  mutate(climate.zone = str_to_title(climate.zone))

## drop lake superior
data %<>% filter(AREASQKM<50000)

## there are 145 NA for climate zones, they are all off the coast of Louisiana
data %<>% filter(!is.na(climate.zone))

## export waterbody type
data %>% 
  st_drop_geometry %>% 
  dplyr::select(COMID, climate.zone, type) %>% 
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

plot = 
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
  # north(data     = us_states,
  #       location = "topright") +
  # scalebar(data      = us_states,
  #          location  = "bottomright",
  #          transform = T,
  #          dist      = 1000,
  #          dist_unit = "mi") +
  labs(color = '',
       fill  = '',
       label = '') +
  theme_void() +
  theme(legend.position = 'bottom') + 
  guides(color = guide_legend(nrow = 3),
         fill  = guide_legend(nrow = 3))

## export
ggsave("output/figures/climate_zones_mrb.svg", plot, width  = 9, height = 6)
ggsave("output/figures/climate_zones_mrb.png", plot, width  = 9, height = 6)

## end of script. have a great day! 