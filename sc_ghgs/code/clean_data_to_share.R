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
packages <- c('tidyverse','magrittr','fs','stringr','reader','here','data.table','zoo') ## you can add more packages here
lapply(packages, pkgTest)

##########################
################  PREAMBLE
##########################

## Set working directory
setwd(here())

## List of gases
gas_list <- c('co2','n2o','ch4')
discount_rates <- c('1.5%','2%','2.5%','3%','5%','95th Pct at 3.0%')

##########################
##################  tables
##########################

for (thisgas in gas_list) {

table <- read_csv(paste0(getwd(),"\\sc_ghgs\\data\\",thisgas,"_table.csv")) %>% 
                 filter(geography=="Global" & discount_rate %in% discount_rates) %>% 
                 select(!geography) %>%
                 mutate(gas = toupper(thisgas))

assign(paste0("table_", thisgas), table)
}

table_co2 <- read_csv(paste0(getwd(),"\\sc_ghgs\\data\\co2_table_ignore_discontinuities_co2.csv")) %>% 
              filter(geography=="Global" & discount_rate %in% discount_rates) %>% 
              select(!geography) %>%
              mutate(gas = toupper('co2'))

table <- rbind(table_co2,table_n2o,table_ch4) %>% 
          mutate(se=ifelse(discount_rate=='95th Pct at 3.0%',0,se))

# create sequence of dates
annual <- data.frame(year = rep(seq(2020,2060,1),18),
                     discount_rate = c(rep('1.5%',41),rep('2%',41),rep('2.5%',41),rep('3%',41),rep('5%',41),rep('95th Pct at 3.0%',41)),
                     gas  = c(rep('CO2',246),rep('N2O',246),rep('CH4',246))) 

# create sequence of dates
five_year <- data.frame(year = rep(seq(2020,2060,5),18),
                        discount_rate = c(rep('1.5%',9),rep('2%',9),rep('2.5%',9),rep('3%',9),rep('5%',9),rep('95th Pct at 3.0%',9)),
                        gas  = c(rep('CO2',54),rep('N2O',54),rep('CH4',54))) 

# merge
annual_unrounded <- merge(annual,table,all=TRUE) %>%
                    arrange(gas,discount_rate,year) %>%
                    group_by(gas,discount_rate) %>%
                    mutate(mean = na.approx(mean),
                           se = na.approx(se)) 

# merge
five_year_unrounded <- merge(five_year,table,all=TRUE) %>% 
                       arrange(gas,discount_rate,year) %>%
                       group_by(gas,discount_rate) %>%
                       mutate(mean = na.approx(mean),
                              se = na.approx(se))

## reshape and export to excel
annual_w <- annual_unrounded %>% select(!se) %>% 
            pivot_wider(names_from = c(discount_rate,gas), values_from = mean)

five_year_w <- five_year_unrounded %>% select(!se) %>% 
               pivot_wider(names_from = c(discount_rate,gas), values_from = mean) 

##########################
####################  SAVE
##########################

write_excel_csv(annual_w, "sc_ghgs\\data\\scghg_annual_unrounded_ignore_pageco2_discontinuities.csv")
write_excel_csv(five_year_w, "sc_ghgs\\data\\scghg_five_year_unrounded_ignore_pageco2_discontinuities.csv")


## END OF SCRIPT. Have a great day!
