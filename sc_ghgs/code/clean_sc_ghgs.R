## Written by: US EPA National Center for Environmental Economics

###############################################################################
###########################    Summary Statistics   ###########################
###############################################################################

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
setwd(here())

## Inflation from BEA deflator, annual values 113.626/92.486 from: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=2#reqid=19&step=2&isuri=1&1921=survey 
inflation <-  1.228575

## List of gases
hfc_list <- c('32','125','134a','143a','152a','227ea','236fa','245fa','4310mee')

##########################
##############  START LOOP
##########################

for (hfc in hfc_list) {
  
  dice_dir    <- paste0("data\\hfc",hfc,"\\dice") # location of file group
  dice_files  <- fs::dir_ls(dice_dir, regexp = "\\.csv$") # create list of .csv files
  dice        <- dice_files %>% 
                     map_dfr(read_csv, skip = 1, col_names = FALSE, .id = "source") %>% # read in files (map), turn into data frame (df), and row bind (r)
                     as.data.table() %>%
                     setnames(c('X1','X2','X3','X4','X5','X6','X7','X8','X9'),c('2020','2025','2030','2035','2040','2045','2050','2055','2060'))

  fund_dir    <- paste0("data\\hfc",hfc,"\\fund") # location of file group
  fund_files  <- fs::dir_ls(fund_dir, regexp = "\\.csv$") # create list of .csv files
  fund        <- fund_files %>% 
                     map_dfr(read_csv, .id = "source") %>% # read in files (map), turn into data frame (df), and row bind (r)
                     as.data.table()
  
  page_dir    <- paste0("data\\hfc",hfc,"\\page") # location of file group
  page_files  <- fs::dir_ls(page_dir, regexp = "\\.csv$") # create list of .csv files
  page        <- page_files %>% 
                     map_dfr(read_csv, .id = "source") %>% # read in files (map), turn into data frame (df), and row bind (r)
                     as.data.table()
  
##########################
###################  CLEAN
##########################

dice %<>% mutate(source = str_remove(source,paste0("data/hfc",hfc,"/dice/"))) %>%
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
          mutate(trial = seq(n()))

fund %<>% mutate(source = str_remove(source,paste0("data/hfc",hfc,"/fund/"))) %>%
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
          mutate(trial = seq(n()))

page %<>% mutate(source = str_remove(source,paste0("data/hfc",hfc,"/page/"))) %>%
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
          mutate(trial = seq(n()))

## WIDE TO LONG
years <-  paste(seq(2020,2060,5), sep=", ") 
dice %<>% gather(year,estimate,all_of(years)) %>%
          mutate(estimate = estimate * inflation, 
                 model = 'DICE 2010')
fund %<>% gather(year,estimate,years) %>%
          mutate(estimate = estimate * inflation, 
                 model = 'FUND 3.8')
page %<>% gather(year,estimate,years) %>%
          mutate(estimate = estimate * inflation, 
                 model = 'PAGE 2009') %>%
          as.data.table() %>% 
          setkey(scenario,discount_rate,trial,year,model)

## load page discontinuities and drop trues
page_disc <- read_csv(paste0("..\\..\\data\\hfc",hfc,"_page_discontinuity.csv")) %>% 
             as.data.table() %>% 
             setkey(scenario,discount_rate,trial,year,model) %>%
             mutate(year = as.character(year))

## merge and drop trues
page <- page_disc[page] %>%
        mutate(estimate = case_when(discontinuity=='TRUE' ~ NA_real_,
                                    discontinuity=='FALSE' ~ estimate)) %>%
        select(!discontinuity)

## COMBINE
data <- rbind(dice,fund,page) %>%
        mutate(gas=paste0('HFC',hfc))

##########################
###################  MEANS
##########################

## All models
means <- data %>% group_by(geography,year,discount_rate) %>% dplyr::summarize(mean = mean(estimate, na.rm=TRUE),
                                                                              se   = sd(estimate, na.rm=TRUE)/sqrt(10000))

## HIGH-IMPACT 95th percentile of each model
high_impact <- data %>% 
  filter(discount_rate=='3%') %>%
  group_by(geography,year) %>%
  dplyr::summarize(mean = quantile(estimate, 0.95, na.rm=TRUE)) %>%
  mutate(discount_rate = 'High Impact (95th Pct at 3.0%)')

table <- rbind(means,high_impact)  


## DICE
dice_means <- dice %>% group_by(geography,year,discount_rate) %>% dplyr::summarize(mean = mean(estimate, na.rm=TRUE),
                                                                                   se   = sd(estimate, na.rm=TRUE)/sqrt(10000))
dice_high_impact <- dice %>% 
  filter(discount_rate=='3%') %>%
  group_by(geography,year) %>%
  dplyr::summarize(mean = quantile(estimate, 0.95, na.rm=TRUE)) %>%
  mutate(discount_rate = 'High Impact (95th Pct at 3.0%)')

dice_table <- rbind(dice_means,dice_high_impact) 


## FUND
fund_means <- fund %>% group_by(geography,year,discount_rate) %>% dplyr::summarize(mean = mean(estimate, na.rm=TRUE),
                                                                                   se   = sd(estimate, na.rm=TRUE)/sqrt(10000))

fund_high_impact <- fund %>% 
  filter(discount_rate=='3%') %>%
  group_by(geography,year) %>%
  dplyr::summarize(mean = quantile(estimate, 0.95, na.rm=TRUE)) %>%
  mutate(discount_rate = 'High Impact (95th Pct at 3.0%)')

fund_table <- rbind(fund_means,fund_high_impact) 


## PAGE
page_means <- page %>% group_by(geography,year,discount_rate) %>% dplyr::summarize(mean = round(mean(estimate, na.rm=TRUE),0),
                                                                                   se   = round(sd(estimate, na.rm=TRUE),0)/sqrt(10000))

page_high_impact <- page %>% 
  filter(discount_rate=='3%') %>%
  group_by(geography,year) %>%
  dplyr::summarize(mean = quantile(estimate, 0.95, na.rm=TRUE)) %>%
  mutate(discount_rate = 'High Impact (95th Pct at 3.0%)')

page_table <- rbind(page_means,page_high_impact) 

##########################
####################  SAVE
##########################

write_csv(data, paste0("..\\..\\data\\hfc",hfc,".csv"))
write_csv(table, paste0("..\\..\\data\\hfc",hfc,"_table.csv"))
write_csv(dice_table, paste0("..\\..\\data\\hfc",hfc,"_dice_table.csv"))
write_csv(fund_table, paste0("..\\..\\data\\hfc",hfc,"_fund_table.csv"))
write_csv(page_table, paste0("..\\..\\data\\hfc",hfc,"_page_table.csv"))

}
## END OF SCRIPT. Have a great day!
