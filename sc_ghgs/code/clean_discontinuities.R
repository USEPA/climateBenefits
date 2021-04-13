## Written by: US EPA National Center for Environmental Economics

##########################
#################  LIBRARY
##########################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c('tidyverse','magrittr','fs','stringr','reader','here','data.table') ## you can add more packages here
lapply(packages, pkgTest)

##########################
################  PREAMBLE
##########################

## Set working directory
setwd(paste0(here(),'\\sc_ghgs'))

## List of gases
gas_list <- c('co2','n2o','ch4')

##########################
##############  START LOOP
##########################

for (thisgas in gas_list) {

disc_dir <- list.dirs(path=paste0(getwd(),'\\data\\',thisgas,'\\page'),full.names = FALSE, recursive = TRUE) %>%
            str_subset(., 'discontinuity_mismatch')
page_dir    <- paste0(getwd(),"\\data\\",thisgas,'\\page\\',disc_dir)                                # location of file group
page_files  <- fs::dir_ls(page_dir, regexp = "\\.csv$")                         # create list of .csv files
page        <- page_files %>% 
                  map_dfr(read_csv, .id = "source") %>%                         # read in files (map), turn into data frame (df), and row bind (r)
                  as.data.table()

##########################
###################  CLEAN
##########################

page %<>% mutate(source = gsub(".*/discontinuity_mismatch/","",source)) %>%
          mutate(source = str_remove(source,".csv")) %>%
          separate(source, c("scenario","discount_rate"), " ") %>%
          mutate(scenario = case_when(scenario=="USG1" ~ "IMAGE",
                                      scenario=="USG2" ~ "MERGE Optimistic",
                                      scenario=="USG3" ~ "MESSAGE",
                                      scenario=="USG4" ~ "MiniCAM Base",
                                      scenario=="USG5" ~ "5th Scenario"),
                 discount_rate = paste0(as.numeric(discount_rate)*100,'%')) %>%
          group_by(scenario,discount_rate) %>%
          mutate(trial = seq(n()))

## WIDE TO LONG
years <-  paste(seq(2020,2060,5), sep=", ") # vector of years
page %<>% gather(year,discontinuity,all_of(years)) %>%
          mutate(model = 'PAGE 2009',
                 gas = thisgas)

##########################
####################  SAVE
##########################

write_csv(page, paste0(getwd(),"\\data\\",thisgas,"\\page\\",thisgas,"_discontinuity.csv"))

}

## END OF SCRIPT. Have a great day!
