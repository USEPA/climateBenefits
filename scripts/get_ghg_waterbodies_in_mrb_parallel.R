##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'sf', 'USAboundaries',
                      'doParallel',
                      'arrow')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
##################### data
##########################

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

###### set Up Cluster ######
## parallel filtering of waterbodies by state
## set cores
# n.cores = parallel::detectCores()-1
n.cores = 3

## make cluster
my.cluster = parallel::makeCluster(n.cores, type = "PSOCK")

## register cluster to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

## start clock
time1 = Sys.time()

## start parallel
foreach(
  STATE = mrb.states[1],
  .packages=c('magrittr','tidyverse',
              'sf',
              'arrow')
) %dopar% {

  data = 
    st_read(paste0('output/ghg_waterbodies_in_mrb/ghg_waterbodies_in_mrb_', STATE, '.shp')) %>%
    st_set_crs(4326) ## per jake beaulie on 2/2/2023 this is the correct projection. The CRS was dropped when creating the gpkg geopackage 
  
  ## collect garbage
  gc()

  ## keep only ponds and reservoirs
  data %<>% filter(is.na(comid), comid == 'NA')

  ## keep only columns needed for filtering to watershed
  national %<>% dplyr::select(comid, stusps)
  
  ## make polygons valid, reduce to centroids to speed up spatial filter
  national %>%
    filter(stusps == STATE) %>%
    st_make_valid %>% 
    st_centroid %>%
    st_transform(st_crs(watershed)) %>% 
    st_filter(watershed) %>% 
    st_drop_geometry %>% 
    write_parquet(paste0('output/ghg_in_mrb/ghg_in_mrb_', STATE, '.parquet'))
}

## stop cluster
parallel::stopCluster(cl = my.cluster)

## end of script. have a great day! 