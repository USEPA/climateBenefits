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

## Inflation from BEA deflator, annual values 113.626/92.486 from: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=2#reqid=19&step=2&isuri=1&1921=survey 
inflation <-  1.228575

## List of gases
gas_list <- c('co2')

##########################
##############  START LOOP
##########################

for (thisgas in gas_list) {

  dice_dir    <- list.dirs(path=paste0(getwd(),'/data/',thisgas,'/dice'),full.names = TRUE, recursive = TRUE) %>% str_subset(., paste0('/SC-',toupper(thisgas)))
  dice_files  <- fs::dir_ls(dice_dir, regexp = "\\.csv$")                       
  dice        <- dice_files %>% 
                     map_dfr(read_csv, .id = "source") %>%                      
                     as.data.table()

  fund_dir    <- list.dirs(path=paste0(getwd(),'/data/',thisgas,'/fund'),full.names = TRUE, recursive = TRUE) %>% str_subset(., paste0('/SC-',toupper(thisgas)))
  fund_files  <- fs::dir_ls(fund_dir, regexp = "\\.csv$") 
  fund        <- fund_files %>% 
                     map_dfr(read_csv, .id = "source") %>% 
                     as.data.table()
  
  page_dir    <- list.dirs(path=paste0(getwd(),'/data/',thisgas,'/page'),full.names = TRUE, recursive = TRUE) %>% str_subset(., paste0('/SC-',toupper(thisgas)))
  page_files  <- fs::dir_ls(page_dir, regexp = "\\.csv$") 
  page        <- page_files %>% 
                     map_dfr(read_csv, .id = "source") %>% 
                     as.data.table()
  
##########################
###################  CLEAN
##########################

clean_mods <- function(x) {
x %>% mutate(source = gsub(paste0('.*/SC-',toupper(thisgas),'/'),"",source)) %>%
      mutate(source = str_remove(source,".csv")) %>%
      separate(source, c("scenario","discount_rate","geography"), " ") %>%
      mutate(geography = case_when(is.na(geography) ~ 'Global',
                                    !is.na(geography) ~ 'Domestic'),
             scenario = case_when(scenario=="USG1" ~ "IMAGE",
                                  scenario=="USG2" ~ "MERGE Optimistic",
                                  scenario=="USG3" ~ "MESSAGE",
                                  scenario=="USG4" ~ "MiniCAM Base",
                                  scenario=="USG5" ~ "5th Scenario"),
             discount_rate = paste0(as.numeric(discount_rate)*100,'%')) %>%
      group_by(scenario,discount_rate,geography) %>%
      mutate(trial = seq(n()))}

## clean
dice %<>% clean_mods()
fund %<>% clean_mods()
page %<>% clean_mods()

## WIDE TO LONG
years <-  paste(seq(2020,2060,5), sep=", ") 
wide_to_long <- function(x) {
  x %>% gather(year,estimate,all_of(years)) %>%
        mutate(estimate = estimate * inflation)}

dice %<>% wide_to_long() %>% mutate(model = 'DICE 2010')
fund %<>% wide_to_long() %>% mutate(model = 'FUND 3.8')
page %<>% wide_to_long() %>% mutate(model = 'PAGE 2009') %>%
          as.data.table() %>% 
          setkey(scenario,discount_rate,trial,year,model)

# ## load page discontinuities and drop trues
# page_disc <- read_csv(paste0(getwd(),"\\data\\",thisgas,"\\page\\",thisgas,"_discontinuity.csv")) %>% 
#              as.data.table() %>% select(!gas) %>%
#              setkey(scenario,discount_rate,trial,year,model) %>%
#              mutate(year = as.character(year))
# 
# ## merge and drop trues
# page <- page_disc[page] %>%
#         mutate(estimate = case_when(discontinuity=='TRUE' ~ NA_real_,
#                                     discontinuity=='FALSE' ~ estimate)) %>%
#         select(!discontinuity)

## combine
data <- rbind(dice,fund,page) %>%
        mutate(gas=paste0(thisgas))

##########################
###################  MEANS
##########################

## functions
mod_means <- function(x) {
  x %>% group_by(geography,year,discount_rate) %>% 
    dplyr::summarize(mean = mean(estimate, na.rm=TRUE),se   = sd(estimate, na.rm=TRUE)/sqrt(10000))}

high_imp <- function(x) {
  x %>% filter(discount_rate=='3%') %>%
    group_by(geography,year) %>%
    dplyr::summarize(mean = quantile(estimate, 0.95, na.rm=TRUE)) %>%
    mutate(discount_rate = '95th Pct at 3.0%')}

## All models
means <- mod_means(data)
high_impact <- high_imp(data)
table <- rbind(means,high_impact)  


## DICE
means <- mod_means(dice)
high_impact <- high_imp(dice)
dice_table <- rbind(means,high_impact) 

## FUND
means <- mod_means(fund)
high_impact <- high_imp(fund)
fund_table <- rbind(means,high_impact) 

## PAGE
means <- mod_means(page)
high_impact <- high_imp(page)
page_table <- rbind(means,high_impact) 

##########################
####################  SAVE
##########################

# write_csv(data, paste0("data\\",thisgas,".csv"))
write_csv(table, paste0("data\\",thisgas,"_table_ignore_discontinuities_co2.csv"))
# write_csv(dice_table, paste0("data\\",thisgas,"_dice_table.csv"))
# write_csv(fund_table, paste0("data\\",thisgas,"_fund_table.csv"))
# write_csv(page_table, paste0("data\\",thisgas,"_page_table.csv"))

}

## END OF SCRIPT. Have a great day!
