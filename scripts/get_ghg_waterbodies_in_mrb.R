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

# ## function to write zip from r
# write_sf_shp_zip <- function(obj, zipfile, overwrite = FALSE) {
#   
#   stopifnot(tools::file_ext(zipfile) == "zip")
#   
#   if (file.exists(zipfile) && !overwrite) {
#     stop(paste0("File already exists: ", zipfile,
#                 "\nUse 'overwrite = TRUE' to overwrite the existing file."))
#   }
#   
#   tmp_zip <- basename(zipfile)
#   shp_name <- paste0(tools::file_path_sans_ext(tmp_zip), ".shp")
#   
#   ## Temporary directory to write .shp file
#   tmp <- tempfile()
#   dir.create(tmp)
#   on.exit(unlink(tmp, recursive = TRUE, force = TRUE))
#   
#   sf::write_sf(obj, file.path(tmp, shp_name), delete_layer = TRUE)
#   withr::with_dir(tmp, zip(tmp_zip, list.files()))
#   
#   file.copy(file.path(tmp, tmp_zip), zipfile, overwrite = overwrite)
# }

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

## get table that we want
## start clock
start.read.time <- proc.time()
national = 
  st_read('../climateBenefits.gpkg', 'national') %>%
  st_set_crs(4326) ## per jake beaulie on 2/2/2023 this is the correct projection. The CRS was dropped when creating the gpkg geopackage 
## stop clock
read.time = proc.time() - start.read.time

## check read time, 430 seconds, or 7 minutes
print('time it took to read ghg database: ')
read.time

## collect garbage
gc()

## trim to only states in the mrb
national %>% filter(stusps %in% mrb.states)

## check distinct types of waterbodies
national %>% st_drop_geometry %>% dplyr::select(subtype) %>% table
# subtype
# canals and ditches    freshwater pond    reservoir 
# 194850                5907664            88118 

## keep only ponds and reservoirs
## start clock
start.filter.time <- proc.time()
national %<>% filter(subtype %in% c('freshwater pond', 'reservoir'))
## stop clock
filter.time = proc.time() - start.filter.time

## check read time, 274 seconds
print('time it took to filter to ponds and reservoirs: ')
filter.time

## garbage collect
gc()

## keep only columns needed for filtering to watershed
national %<>% 
  dplyr::select(globalid, stusps) %>% 
  filter(!is.na(globalid))

# ## export one state at a time and zip
# for(i in mrb.states){
#   national %>%
#     filter(stusps == i) %>%
#     write_sf_shp_zip(paste0('output/ghg_waterbodies_in_mrb/ghg_waterbodies_in_mrb_', i, '.zip'))
# }

## garbage collect
gc()

# ## try one state at a time
# for(i in mrb.states){
#   ## make polygons valid, reduce to centroids to speed up spatial filter
#   national %>%
#     filter(stusps == j) %>%
#     st_make_valid %>%
#     st_centroid %>%
#     st_transform(st_crs(watershed)) %>%
#     st_filter(watershed) %>%
#     st_drop_geometry %>%
#     write_parquet(paste0('output/ghg_in_mrb/ghg_in_mrb_', i, '.parquet'))
# }

###### set Up Cluster ######
## parallel filtering of waterbodies by state
## set cores
# n.cores = parallel::detectCores()-1
n.cores = 4

## make cluster
my.cluster = parallel::makeCluster(n.cores, type = "PSOCK")

## register cluster to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

## start clock
time1 = Sys.time()

## start parallel
foreach(
  STATE = mrb.states,
  .packages=c('tidyverse',
              'sf',
              'arrow')
) %dopar% {
  
  ## make polygons valid, reduce to centroids to speed up spatial filter, keep only
  national %>%
    filter(stusps == STATE) %>%
    st_make_valid %>%
    st_centroid %>%
    st_transform(st_crs(watershed)) %>%
    st_filter(watershed) %>% 
    st_drop_geometry %>% 
    write_parquet(paste0('output/ghg_in_mrb/ghg_in_mrb_', STATE, '.parquet'))
}

## stop clock
Sys.time() - time1

## stop cluster
parallel::stopCluster(cl = my.cluster)

## end of script. have a great day!