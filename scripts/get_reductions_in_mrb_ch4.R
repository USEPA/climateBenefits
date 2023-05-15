##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'sf',
                      'arrow')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
#################### parts
##########################

## function to read all files in a directory
read_mrb_waterbodies = function(x) {
  read_parquet(x)
}

## watershed boundary
watershed =
  st_read('store/Miss_RiverBasin/Miss_RiverBasin.shp')

## get state polygons
us.states = subset(USAboundaries::us_states(),
                   !name %in% c("United States Virgin Islands",
                                "Commonwealth of the Northern Mariana Islands",
                                "Guam",
                                "American Samoa",
                                "Puerto Rico",
                                "Alaska",
                                "Hawaii")) %>%
  st_transform(crs = st_crs(watershed))

## get states in watershed
mrb.states = 
  us.states %>% 
  st_filter(watershed) %>% 
  st_drop_geometry %>% 
  .$state_abbr

##########################
##################### data
##########################

# ## read waterbodies from ghgrp within the mississippi river basin (from get_ghg_waterbodies_in_mrb.R)
# mrb.waterbodies = 
#   list.files('output/ghg_in_mrb/', pattern = "*.parquet", full.names = T) %>%
#   map_df(~read_mrb_waterbodies(.)) %>% 
#   .$globalid
# 
# ## read all waterbodies from ghgrp, filter to those in mississippi river basin
# ## TO-DO test if SQL query will work for this filer to speed up the read_sf, query is available as an option in read_sf: query = "SELECT * FROM national WHERE globalid in mrb.waterbodies;"
# data = 
#   read_sf('../climateBenefits.gpkg') %>%
#   dplyr::select(globalid, stusps, climate, subtype, area_ha_new, co2.tonnes.ha.y.2021, ch4.tonnes.ha.y.2021) %>% 
#   filter(globalid %in% mrb.waterbodies)
# 
# ## get emissions by climate zone and type from main markdown
# emissions.by.type = 
#   left_join(
#     left_join(
#       read_csv('output/emissions_by_lake_characteristics_ch4.csv', show_col_types = F),
#       read_csv('output/emissions_by_lake_characteristics_co2.csv', show_col_types = F),
#       by = c('type', 'climate.zone')
#     ),
#     read_csv('output/emissions_by_lake_characteristics_n2o.csv', show_col_types = F),
#     by = c('type', 'climate.zone')
#   ) %>% 
#   rename(subtype = type,
#          climate = climate.zone) %>% 
#   mutate(subtype = case_when(subtype == 'pond' ~ 'freshwater pond', T ~ subtype))
# 
# ## collect garbage
# gc()
# 
# ## get average emissions rates by subtype and climate to identify closest subtypes and climates
# data %>% 
#   st_drop_geometry %>% 
#   group_by(climate, subtype) %>% 
#   summarize(co2.tonnes.ha.y.2021 = mean(co2.tonnes.ha.y.2021, na.rm = T),
#             ch4.tonnes.ha.y.2021 = mean(ch4.tonnes.ha.y.2021, na.rm = T)) %>% 
#   arrange(subtype, ch4.tonnes.ha.y.2021)

# climate              subtype         co2.tonnes.ha.y.2021 ch4.tonnes.ha.y.2021
# <chr>                <chr>                          <dbl>                <dbl>
#   1 boreal               freshwater pond                 3.45               0.183
# 2 cool temperate       freshwater pond                 3.74               0.183
# 3 tropical dry/montane freshwater pond               NaN                  0.183
# 4 tropical moist/wet   freshwater pond                10.2                0.183
# 5 warm temperate dry   freshwater pond                 6.23               0.183
# 6 warm temperate moist freshwater pond                 5.35               0.183
# 7 boreal               reservoir                     NaN                  0.0136
# 8 cool temperate       reservoir                       3.74               0.0540
# 9 warm temperate moist reservoir                       5.35               0.0804
# 10 tropical moist/wet   reservoir                     NaN                  0.141
# 11 warm temperate dry   reservoir                       6.23               0.151
# 12 tropical dry/montane reservoir                     NaN                  0.284

## from the chesapeake bay we have cool temperate and warm temperate moist. 
## for freshwater ponds and ch4, there is no variation in emission rates across climate.
## for reservoirs and ch4, cool temperate gets mapped to boreal, and warm temperate moist for everything else.
## for freshwater ponds and co2, cool temperate gets mapped to boreal, and warm temperate moist for everything else.
## for reservoirs and co2, the same is true, although there are only emission rates for three zones. 

# ## get comparable climate zones for merging emissions reductions
# data %<>% 
#   mutate(climate.ches.bay = case_when(climate == 'Boreal' ~ 'Cool Temperate',
#                                       T ~ 'Warm Temperate Moist'))
# 
# ## merge with chesapeake bay emissions reductions
# data %<>% 
#   left_join(emissions.by.type, 
#             by = c('climate.ches.bay' = 'climate', 'subtype'))
# 
# ## estimate emissions for waterbodies in the mrb
# data %<>% 
#   mutate(co2.emissions = area_ha_new * co2.tonnes.ha.y.2021,
#          ch4.emissions = area_ha_new * (ch4.tonnes.ha.y.2021 + (ch4.tonnes.ha.y.2021 * 0.09)),
#          co2.reduction = co2.emissions * incr.co2.mean.pct,
#          ch4.reduction = ch4.emissions * incr.ch4.mean.pct)
# 
# ## get total emissions reductions
# data %>% 
#   st_drop_geometry %>% 
#   summarize(co2.reduction = sum(co2.reduction, na.rm = T),
#             ch4.reduction = sum(ch4.reduction, na.rm = T))
# 
# ## count number of waterbodies with co2 emissions estimates
# data %>% 
#   st_drop_geometry %>% 
#   filter(!is.na(co2.reduction)) %>% 
#   nrow
# 
# ## export results by state
# for(STATE in mrb.states){
#   data %>%
#     filter(stusps == STATE) %>%
#     st_write(paste0('output/mrb_emissions_reductions/mrb_emissions_reductions_', STATE, '.shp'))
# }
# 
# ## export results by state as text files
# for(STATE in mrb.states){
#   data %>%
#     st_drop_geometry %>% 
#     filter(stusps == STATE) %>%
#     write_parquet(paste0('output/mrb_emissions_reductions_text/mrb_emissions_reductions_text_', STATE, '.parquet'))
# }

## read results by state as text files (from above)
## function to read all files in a directory
read_all = 
  function(x) {
    read_parquet(x)
  }

data = 
  list.files('output/mrb_emissions_reductions_text', pattern = "*.parquet", full.names = T) %>%
  map_df(~read_all(.))

## get results 
data %>% 
  select(ch4.reduction, co2.reduction) %>% 
  summarise_all(sum, na.rm = T)

##########################
###################### n2o
##########################

## get n2o data from Alisa.Keyser@colostate.edu and Stephen.Ogle@colostate.edu in an email on 3/3/2023
n2o.mrb =
  right_join(
    read_csv('store/n2o_in_mrb/MSRBgrid_NO3_wIPCC_2023-03-03.csv')[1:7],
    read_csv('store/climate_zones_for_mrb_n2o_gridcells.csv'), 
    by = 'cellID_10km') 

## get average emissions rates by subtype and climate to identify closest subtypes and climates
n2o.mrb %>% 
  group_by(climate.zone) %>% 
  summarize(`Nitrogen_Leach_Rate_gN/m2/yr` = mean(`Nitrogen_Leach_Rate_gN/m2/yr`, na.rm = T)) %>% 
  arrange(climate.zone, `Nitrogen_Leach_Rate_gN/m2/yr`)

##
# climate.zone         `Nitrogen_Leach_Rate_gN/m2/yr`
# <chr>                                         <dbl>
#   1 Boreal                                       0.0768
# 2 Cool Temperate                               1.05  
# 3 Tropical Dry/Montane                         0.0103
# 4 Tropical Moist/Wet                           3.35  
# 5 Warm Temperate Dry                           0.185 
# 6 Warm Temperate Moist                         2.74  

## check climate zones
table(n2o.mrb$climate.zone)
table(emissions.by.type$climate)

## get comparable climate zones for merging emissions reductions
n2o.mrb %<>%
  mutate(climate.ches.bay = case_when(climate.zone == 'Boreal' ~ 'Cool Temperate',
                                      T ~ 'Warm Temperate Moist'))

## merge with chesapeake bay emissions reductions
n2o.mrb %<>% 
  left_join(emissions.by.type %>% 
              select(-subtype), 
            by = c('climate.ches.bay' = 'climate'))

## get results
n2o.mrb %>% 
  summarize(
    n2o.reductions = 
      sum(                                        ## summing across all grid cells in the mrb
        `Nitrogen_Leach_Rate_gN/m2/yr` *          ## leach rate in grams of N per meter squared 
          0.0075 *                                ## this converts leached N to N2O-N (IPCC rate), not entirely sure what this means but jake does because he's smart
          ModeledPct_total_CropGrass_area/100 *   ## gives percentage of each grid cell that is responsible for leaching
          1e4 * 1e4 *                             ## this is the area of each grid cell in meters
          1e-6 *                                  ## grams to tonnes
          44/14 *                                 ## this converts N to N2O (or, if you're paying attention, N2O-N to N2O)
          incr.n2o.mean.pct,                      ## this is the climate zone specific reduction from the policy (as modeled in the CB lakes model)
        na.rm = T
      )
  ) 

## get results 
n2o.mrb %>% 
  summarize(
    sum(                                        ## summing across all grid cells in the mrb
      `Nitrogen_Leach_Rate_gN/m2/yr` *          ## leach rate in grams of N per meter squared 
        0.0075 *                                ## this converts leached N to N2O-N (IPCC rate), not entirely sure what this means but jake does because he's smart
        ModeledPct_total_CropGrass_area/100 *   ## gives percentage of each grid cell that is responsible for leaching
        1e4 * 1e4 *                             ## this is the area of each grid cell in meters
        1e-6 *                                  ## grams to tonnes
        44/14,                                  ## this converts N to N2O (or, if you're paying attention, N2O-N to N2O)
      na.rm = T
    )
  ) 

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