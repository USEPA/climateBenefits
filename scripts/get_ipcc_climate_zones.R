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
colors = c("#56B4E9", "#F0E442", "#000000", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#999999")

##########################
##################### data
##########################

## watershed boundary
watershed =
  st_read('maps/chesapeakeBayWatershed/Chesapeake_Bay_Watershed_Boundary.shp') %>%
  mutate(aes = 'Chesapeake Bay \nWatershed')

# ## test plot
# watershed %>%
#   ggplot() +
#   geom_sf() + 
#   theme_void()

## get climate zones from ipcc
zones = 
  st_read('store/climateMap/ipcc_zones_2017_names.shp')

# ## test plot
# zones %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

## get lakes from study area
lakes = 
  st_read('store/study_lakes/study_lakes.shp') %>% 
  dplyr::select(WB_ID, AREASQK) %>% 
  st_transform(st_crs(zones))

# ## test plot
# lakes %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

## get centroids for climate zone intersection
lake.points = 
  lakes %>% 
  st_centroid

## join waterbody points with climate zones to get climate zone info
data = 
  left_join(
    lakes,
    st_join(lake.points, 
            zones %>% 
              st_make_valid, 
            join = st_within) %>% 
      st_drop_geometry %>% 
      as_tibble %>% 
      dplyr::select(WB_ID, fldd_zn) %>% 
      rename(climate.zone = fldd_zn),
    by = 'WB_ID'
  )

## clean house
rm(lake.points)
gc()

## classify as pond or reservoir
data %<>% 
  mutate(type = case_when(AREASQK <  0.08 ~ 'pond',
                          AREASQK >= 0.08 ~ 'reservoir'))
## check
data %>% st_drop_geometry %>% dplyr::select(climate.zone, type) %>% table
# type
# climate.zone           pond reservoir
# Cool Temperate        844       352
# Warm Temperate Moist 2440       586

## rename to title case
data %<>% 
  mutate(climate.zone = str_to_title(climate.zone))

## export waterbody type
data %>% 
  st_drop_geometry %>% 
  dplyr::select(WB_ID, climate.zone, type) %>% 
  write_csv('store/climate_zones_for_ches_waterbodies.csv')

## get shoreline
shoreline =
  st_read('maps/Chesapeake_Bay_Shoreline_Medium_Resolution.shp')

# ## test plot
# shoreline %>%
#   ggplot() +
#   geom_sf() + 
#   theme_void()

## get state polygons
states =
  st_as_sf(USAboundaries::us_states() %>%
             filter(name %in% c("Virginia", "Maryland", "Delaware",
                                "West Virginia", "Pennsylvania", "New York")) %>%
             st_transform(crs = st_crs(watershed)))

us_states = subset(USAboundaries::us_states(),
                   !name %in% c("United States Virgin Islands",
                                "Commonwealth of the Northern Mariana Islands",
                                "Guam",
                                "American Samoa",
                                "Puerto Rico",
                                "Alaska",
                                "Hawaii")) %>%
  st_transform(crs = st_crs(5070))

## region box
region_box =
  st_as_sfc(st_bbox(states)) %>% 
  st_transform(crs = st_crs(watershed))

## test plot
region = 
  ggplot() + 
  geom_sf(data = us_states, fill = "white", size = 0.2) + 
  geom_sf(data = region_box, fill=NA, color='#9DBF9E', size=1.2) +
  theme_void()

##########################
##################### plot
##########################

study_area = 
  ggplot() +
  geom_sf(data  = states,
          color = 'black',
          alpha = 0.05) + 
  geom_sf(data  = watershed,
          color = "#56B4E9",
          fill  = "#56B4E9",
          alpha = 0.1) +
  geom_sf(data  = shoreline,
          fill  = 'white') + 
  geom_sf(data  = data,
          aes(color  = climate.zone,
              fill  = climate.zone)) + 
  geom_sf_text(data = states, 
               aes(label = stusps)) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  scale_fill_manual(values = c("#009E73","#CC79A7")) +
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
  theme(legend.position = 'bottom')

## draw study area inset 
ggdraw() +
  draw_plot(study_area) +
  draw_plot(region, x = 0.05, y = 0.55, width = 0.35, height = 0.35)

## export
ggsave("output/figures/climate_zones.svg", width  = 6, height = 8)

## end of script. have a great day! 