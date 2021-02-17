################################################################################
############     Climate Benefits of Nutrient Management    ####################
################################################################################


####################################################
##########################################  PREAMBLE
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = T))
  {
    install.packages(x, dep = T)
    if(!require(x, character.only = T)) stop("Package not found")
  }
}

packages <- c('data.table','tidyverse','sf','readxl','USAboundaries',
              'viridis','epiDisplay',
              'ggmap','ggplot2','ggpubr','cowplot',
              'rnaturalearth','rnaturalearthdata',
              'maps','mapview')
lapply(packages, pkgTest)

####################################################
#################################  WORKING DIRECTORY
####################################################

## SET WORKING DIRECTORY
local_path <- Sys.getenv("USERPROFILE")
setwd(paste0(local_path,"\\Environmental Protection Agency (EPA)\\Moore, Chris - Climate benefits of nutrient management\\climateBenefits_bryan\\climateBenefits"))
# setwd(paste0(here(),'/iwg_2021'))


####################################################
##############################################  DATA
####################################################

# source("scripts\\masterLibrary.R")
# source("climateBenefits_jb\\scripts\\ghgFunctions.R")
# source("climateBenefits_jb\\scripts\\setDirectory.R")
# source("climateBenefits_jb\\scripts\\readData.R")
# source("climateBenefits_jb\\scripts\\simulateGhg.R")

milstead_lakes <- st_read("store\\MRB_shapes\\MRB1_WBIDLakes.shp")
head(milstead_lakes)

chesDat <- read_excel("store/ChesLakeLoadsConc.xlsx",
                      sheet = "ChesLakeConc")
head(chesDat)

nhdSf <- st_read(dsn = "store/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb",
                 layer = "NHDWaterbody") %>%
  dplyr::select(COMID, FDATE, RESOLUTION, # note, no OBJECTID when read with sf
                GNIS_ID, GNIS_NAME, AREASQKM, 
                ELEVATION, REACHCODE, FTYPE, FCODE,
                ONOFFNET, PurpCode, PurpDesc,
                MeanDepth, LakeVolume, MaxDepth, MeanDUsed, MeanDCode) # lakeMorpho data
head(nhdSf)

####################################################
#####################################  MISSING LAKES
####################################################

chesDat_not_in_ndh <- chesDat %>% filter(!(WB_ID %in% nhdSf$COMID)) # 25 waterbodies not in NHD
head(chesDat_not_in_ndh)

milstead_not_in_ndh <- milstead_lakes %>% filter(!(WB_ID %in% nhdSf$COMID)) # 296 waterbodies not in NHD

milstead_chesDat_not_in_ndh <- milstead_not_in_ndh %>% filter((WB_ID %in% chesDat_not_in_ndh$WB_ID)) # THE 25 IN chesDat AND ALSO IN MILSTEAD BUT NOT IN NHD

## WHERE ARE THE MISSING LAKES FROM MILSTEAD
map_milstead_not_in_ndh <- mapview(milstead_not_in_ndh, zcol = "WB_ID", legend = T, layer.name='Milstead WB_ID', alpha.regions = 0.3, aplha = 1)
mapshot(map_milstead_not_in_ndh, url = "maps/milstead_not_in_ndh.html")

## WHERE ARE THE MISSING LAKES FROM MILSTEAD AND CHESAPEAKE 
map_milstead_chesDat_not_in_ndh <- mapview(milstead_chesDat_not_in_ndh, zcol = "WB_ID", legend = T, layer.name='Milstead/chesData WB_ID', alpha.regions = 0.3, aplha = 1)
mapshot(map_milstead_chesDat_not_in_ndh, url = "maps/milstead_chesDat_not_in_ndh.html")

## PLOT MISSING LAKES
states <- us_states() %>% 
  filter(name %in% c("Virginia", "Maryland", "Delaware",
                     "West Virginia", "Pennsylvania", "New York")) %>%
  st_transform(5070)

plot_milstead_not_in_ndh <- ggplot(states) +
                            geom_sf() +
                            geom_sf(data = milstead_not_in_ndh) +
                            ggtitle('In Milstead but not in NDH')
ggsave('output\\milstead_not_in_ndh.png',plot_milstead_not_in_ndh)

plot_milstead_chesData_not_in_ndh <-  ggplot(states) +
                                      geom_sf() +
                                      geom_sf(data = milstead_chesDat_not_in_ndh) +
                                      ggtitle('In Milstead AND in ChesData but not in NDH')
ggsave('output\\milstead_chesData_not_in_ndh.png',plot_milstead_chesData_not_in_ndh)

## NHD LAKES in chesData
plot_dat.sf <- ggplot(states) +
               geom_sf() +
               geom_sf(data = dat.sf) +
               ggtitle('In ChesData AND in NHD')
ggsave('output\\chesData_and_NHD.png',plot_dat.sf)

map_chesData_and_NHD <- mapview(dat.sf, zcol = "WB_ID", legend = T, layer.name='WB_ID', alpha.regions = 0.3, aplha = 1)
mapshot(map_chesData_and_NHD, url = "maps/chesData_and_NHD.html")


###################################################
###########################  IDENTIFY MISSING LAKES
###################################################
## WHERE ARE THE MISSING LAKES FROM MILSTEAD AND CHESAPEAKE 

states2 <- st_transform(states,st_crs(nhdSf)) 
nhd_in_region <- st_intersection(nhdSf,states2)
map_nhd <- mapview(nhdSf, zcol = "COMID", legend = F, alpha.regions = 0.3, aplha = 1)
mapshot(map_milstead_chesDat_not_in_ndh, url = "maps/milstead_chesDat_not_in_ndh.html")


# ###################################################
# #########################################  PLOT GHG
# ###################################################
# 
# ## GET STATES
# states <- st_as_sf(us_states() %>% # get states map
#                      filter(name %in% c("Virginia", "Maryland", "Delaware", 
#                                         "West Virginia", "Pennsylvania", "New York")) %>%
#                      st_transform(5070))
# 
# dat.sf2 <- st_union(dat.sf,states)
# 
# 
# # SUM BY GROUP
# dat.sf2 %<>% group_by(state_abbr) %>% 
#   summarize(totCh4diff = sum(ch4Diff.lk.d, na.rm=T)/(1000*1000*1000), do_union = TRUE) # mg->g, g->kg, kg->Mg
# 
# dt <- sf::st_as_sf(data.table::rbindlist(dat.sf,states))
# dt2 %<>% group_by(state_abbr) %>% 
#   summarize(totCh4diff = sum(ch4Diff.lk.d, na.rm=T)/(1000*1000*1000), do_union = TRUE) # mg->g, g->kg, kg->Mg
# 
# ## MAP
# ch4 <- mapview(dat.sf2, zcol = "totCh4diff", legend = T, layer.name='Difference in Ch4 (State Totals)', alpha.regions = 0.3, aplha = 0.4)
# mapshot(ch4, url = "maps\\ch4.html",overwrite=T)
# 
# ch4_dt <- mapview(dt2, zcol = "totCh4diff", legend = T, layer.name='Difference in Ch4 (State Totals)', alpha.regions = 0.3, aplha = 0.4)
# mapshot(ch4_dt, url = "maps\\ch4_dt.html",overwrite=T)

####################################################
####################################  MILSTEAD LAKES
####################################################

### DATA 

# milstead_lakes <- st_read("store\\MRB_shapes\\MRB1_WBIDLakes.shp")
# head(milstead_lakes)
# class(milstead_lakes)
# 
# ches_lakes <- as.data.table(read_excel('store\\ChesLakeLoadsConc.xlsx', sheet = "ChesLakeConc"))
# head(ches_lakes)

### PLOT 

# world <- ne_countries(scale = "medium", returnclass = "sf")
# states <- st_as_sf(map("state", plot = F, fill = T))
# 
# ggplot(data = world) +
#   geom_sf() +
#   geom_sf(data = milstead_lakes, aes(fill = AlbersArea)) +
#   geom_sf(data = states, fill = NA) + 
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
#   coord_sf(xlim = c(-82, -65), ylim = c(36,48), expand = F)
# 
# map_milstead <- mapview(millstead_lakes, zcol = "AlbersArea", legend = T, layer.name='Surface Area', alpha.regions = 0.3, aplha = 1)
# mapshot(map_milstead, url = "output/milstead_lakes.html")

## END OF SCRIPT. Have a great day! 