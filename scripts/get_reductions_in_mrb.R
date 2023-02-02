##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'stringr',
                      'sf')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
##################### data
##########################

# ## get emissions by climate zone and type from main markdown
# emissions.by.type.ch4 = read_csv('output/emissions_by_lake_characteristics_ch4.csv')
# emissions.by.type.co2 = read_csv('output/emissions_by_lake_characteristics_co2.csv')
# emissions.by.type.n2o = read_csv('output/emissions_by_lake_characteristics_n2o.csv')

# ## get state polygons
# us_states = subset(USAboundaries::us_states(),
#                    !name %in% c("United States Virgin Islands",
#                                 "Commonwealth of the Northern Mariana Islands",
#                                 "Guam",
#                                 "American Samoa",
#                                 "Puerto Rico",
#                                 "Alaska",
#                                 "Hawaii")) %>%
#   st_transform(crs = st_crs(5070))

## get table that we want
## start clock
start.read.time <- proc.time()
national = 
  st_read('../climateBenefits.gpkg', 'national') %>%
  st_set_crs(4269)
## stop clock
read.time = proc.time() - start.read.time

## check read time, 700 seconds
print('time it took to read ghg database: ')
read.time

## check distinct types of waterbodies
national %>% st_drop_geometry %>% dplyr::select(subtype) %>% table
# subtype
# canals and ditches    freshwater pond     reservoir 
# 272058                6676699             126181 

## keep only ponds and reservoirs
## start clock
start.filter.time <- proc.time()
national %<>% filter(subtype %in% c('freshwater pond', 'reservoir'))
## stop clock
filter.time = proc.time() - start.filter.time

## check read time, 274 seconds
print('time it took to filter to ponds and reservoirs: ')
filter.time

## watershed boundary
watershed =
  st_read('store/Miss_RiverBasin/Miss_RiverBasin.shp')

## get centroids for climate zone intersection
national.points =
  national %>%
  st_centroid %>% 
  st_transform(st_crs(watershed))

## test projection on a subset
# t = 
#   national.points[1:1000,] %>%
#   st_set_crs(4269) %>% 
#   st_transform(st_crs(watershed))
# 
# ## plot to check projections
# ggplot() +
#   geom_sf(data  = us_states,
#           color = 'black',
#           alpha = 0.05) +
#   geom_sf(data  = watershed,
#           color = "#56B4E9",
#           fill  = "#56B4E9",
#           alpha = 0.1) +
#   geom_sf(data  = t,
#           aes(color = climate,
#               fill  = climate))

## clean house
rm(national)
gc()

## trim waterbodies to watershed
## start clock
start.trim.time <- proc.time()
mrb.points =
  national.points %>%
  st_filter(watershed)
## stop clock
trim.time = proc.time() - start.trim.time

## check computation time
print('time it took to trim waterbodies to mrb watershed: ')
trim.time

## export
mrb.points %>% st_write('output/ghg_waterbodies_in_mrb/ghg_waterbodies_in_mrb_points.shp')



## end of script. have a great day! 