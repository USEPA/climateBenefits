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
hfc_list <- c('32','125','134a','143a','152a','227ea','236fa','245fa','4310mee')
discount_rates <- c('2.5%','3%','5%')

##########################
##############  START LOOP
##########################

for (hfc in hfc_list) {

##########################
##############  READ DATA
##########################

data <- read_csv(paste0("..\\..\\data\\hfc",hfc,".csv")) %>% 
                 filter(geography=="Global" & discount_rate %in% discount_rates) %>% 
                 select(!geography)

####################################################
###################################  all_mc_runs.csv
####################################################


####################################################
##############################  annual_unrounded.csv
####################################################

means <- data %>%
         group_by(year,discount_rate,gas) %>% 
         summarize(mean = mean(estimate,na.rm=TRUE))

## HIGH-IMPACT 95th percentile of each model
high_impact <- data %>% 
  filter(discount_rate=='3%') %>%
  group_by(year,gas) %>%
  summarize(mean = quantile(estimate, 0.95, na.rm=TRUE)) %>%
  mutate(discount_rate = '3% 95th Pct.')

table <- rbind(means,high_impact)

assign(paste0("table_", hfc), table)
}

sc_hfc_table <- rbind(table_32,table_125,table_134a,table_143a,table_152a,table_227ea,table_236fa,table_245fa,table_4310mee)


# create sequence of dates
annual <- data.frame(year = rep(seq(2020,2060,1),12),
                     discount_rate = c(rep('2.5%',41),rep('3%',41),rep('5%',41),rep('3% 95th Pct.',41)),
                     gas  = c(rep('HFC32',164),rep('HFC125',164),rep('HFC134a',164),rep('HFC143a',164),rep('HFC152a',164),rep('HFC227ea',164),rep('HFC236fa',164),rep('HFC245fa',164),rep('HFC4310mee',164))) 

# create sequence of dates
five_year <- data.frame(year = rep(seq(2020,2060,5),12),
                        discount_rate = c(rep('2.5%',9),rep('3%',9),rep('5%',9),rep('3% 95th Pct.',9)),
                        gas  = c(rep('HFC32',36),rep('HFC125',36),rep('HFC134a',36),rep('HFC143a',36),rep('HFC152a',36),rep('HFC227ea',36),rep('HFC236fa',36),rep('HFC245fa',36),rep('HFC4310mee',36))) 

# merge
annual_unrounded <- merge(annual,sc_hfc_table,all=TRUE) %>% 
  arrange(gas,discount_rate,year) %>%
  group_by(gas,discount_rate) %>%
  mutate(mean = na.approx(mean)) 

# merge
five_year_unrounded <- merge(five_year,sc_hfc_table,all=TRUE) %>% 
  arrange(gas,discount_rate,year) %>%
  group_by(gas,discount_rate) %>%
  mutate(mean = na.approx(mean))


## reshape and export to excel
annual_w <- annual_unrounded %>% 
  pivot_wider(names_from = c(discount_rate,gas), values_from = mean)

five_year_w <- five_year_unrounded %>% 
  pivot_wider(names_from = c(discount_rate,gas), values_from = mean) 

##########################
####################  SAVE
##########################

write_excel_csv(annual_w, "data\\schfc_annual_unrounded.csv")
write_excel_csv(five_year_w, "data\\schfc_five_year_unrounded.csv")



## END OF SCRIPT. Have a great day!
