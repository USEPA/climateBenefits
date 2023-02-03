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

## function to clean give data
read_mrb_waterbodies = function(x) {
  read_parquet(x, show_col_types = F)
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

## read waterbodies from ghgrp within the mississippi river basin (from get_ghg_waterbodies_in_mrb.R)
mrb.waterbodies = 
  list.files('output/ghg_in_mrb/', pattern = "*.parquet", full.names = T) %>%
  map_df(~read_mrb_waterbodies(.)) %>% 
  .$globalid

## read all waterbodies from ghgrp, filter to those in mississippi river basin
## TO-DO test if SQL query will work for this filer to speed up the read_sf, query is available as an option in read_sf: query = "SELECT * FROM national WHERE globalid in mrb.waterbodies;"
data = 
  read_sf('../climateBenefits.gpkg') %>%
  dplyr::select(globalid, stusps, climate, subtype, area_ha_new, co2.tonnes.ha.y.2021, ch4.tonnes.ha.y.2021) %>% 
  filter(globalid %in% mrb.waterbodies)

## get emissions by climate zone and type from main markdown
emissions.by.type = 
  left_join(read_csv('output/emissions_by_lake_characteristics_ch4.csv', show_col_types = F),
            read_csv('output/emissions_by_lake_characteristics_co2.csv', show_col_types = F),
            read_csv('output/emissions_by_lake_characteristics_n2o.csv', show_col_types = F),
            by = c('type', 'climate.zone')) %>% 
  rename(subtype = type,
         climate = climate.zone) %>% 
  mutate(subtype = case_when(subtype == 'pond' ~ 'freshwater pond', T ~ subtype))

## collect garbage
gc()

## get average emissions rates by subtype and climate to identify closest subtypes and climates
data %>% 
  st_drop_geometry %>% 
  group_by(climate, subtype) %>% 
  summarize(co2.tonnes.ha.y.2021 = mean(co2.tonnes.ha.y.2021, na.rm = T),
            ch4.tonnes.ha.y.2021 = mean(ch4.tonnes.ha.y.2021, na.rm = T)) %>% 
  arrange(subtype, ch4.tonnes.ha.y.2021)

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

## get comparable climate zones for merging emissions reducitons
data %<>% 
  mutate(climate.ches.bay = case_when(climate == 'Boreal' ~ 'Cool Temperate',
                                      T ~ 'Warm Temperate Moist'))

## merge with chesapeake bay emissions reductions
data %<>% 
  left_join(emissions.by.type, 
            by = c('climate.ches.bay' = 'climate', 'subtype'))

## estimate emissions for waterbodies in the mrb
data %<>% 
  mutate(co2.emissions = area_ha_new * co2.tonnes.ha.y.2021,
         ch4.emissions = area_ha_new * (ch4.tonnes.ha.y.2021 + (ch4.tonnes.ha.y.2021 * 0.09)),
         co2.reduction = co2.emissions * incr.co2.mean.pct,
         ch4.reduction = ch4.emissions * incr.ch4.mean.pct)

## get total emissions reductions
data %>% 
  st_drop_geometry %>% 
  summarize(co2.reduction = sum(co2.reduction, na.rm = T),
            ch4.reduction = sum(ch4.reduction, na.rm = T))

## count number of waterbodies with co2 emissions estimates
data %>% 
  st_drop_geometry %>% 
  filter(!is.na(co2.reduction)) %>% 
  nrow

## export results by state
for(STATE in mrb.states){
  data %>%
    filter(stusps == STATE) %>%
    st_write(paste0('output/mrb_emissions_reductions/mrb_emissions_reductions_', STATE, '.shp'))
}

## export results by state as text files
for(STATE in mrb.states){
  data %>%
    st_drop_geometry %>% 
    filter(stusps == STATE) %>%
    write_parquet(paste0('output/mrb_emissions_reductions_text/mrb_emissions_reductions_text_', STATE, '.parquet'))
}

## end of script. have a great day! 