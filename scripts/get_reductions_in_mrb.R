##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'stringr',
                      'sf',
                      'RSQLite')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
##################### data
##########################

## get emissions by climate zone and type from main markdown
emissions.by.type.ch4 = read_csv('output/emissions_by_lake_characteristics_ch4.csv')
emissions.by.type.co2 = read_csv('output/emissions_by_lake_characteristics_co2.csv')
emissions.by.type.n2o = read_csv('output/emissions_by_lake_characteristics_n2o.csv')

## waterbodies within the mrb from get_ipcc_climate_zones_mrb.R
mrb.points = st_read('store/ndh_in_mrb/nhd_lakes_in_mrb_as_points.shp')

# create connection to database
db <- DBI::dbConnect(RSQLite::SQLite(), 
                     dbname = '../climateBenefits.gpkg')

## explore tables in database
dbListTables(db)  

## get tables that we want
national = st_read(db, 'national')




## end of script. have a great day! 