################################################################################
############     Climate Benefits of Nutrient Management    ####################
################################################################################

# devtools::install_github('oswaldosantos/ggsn')

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
              'maps','mapview','here','rcartocolor','ggsn')
lapply(packages, pkgTest)

####################################################
#################################  WORKING DIRECTORY
####################################################

## SET WORKING DIRECTORY
setwd(here())

## google
register_google(key = "AIzaSyAdJyWfjL0dJUK2L7YmbDLSBrxMea5qVpo")

####################################################
##############################################  DATA
####################################################

watershed <- st_read('maps\\chesapeakeBayWatershed\\Chesapeake_Bay_Watershed_Boundary.shp')
watershed %>% 
  ggplot() +
  geom_sf() + theme_void()

shoreline <- st_read('maps\\Chesapeake_Bay_Shoreline_Medium_Resolution.shp')
shoreline %>% 
  ggplot() +
  geom_sf() + theme_void()

states <- st_as_sf(us_states() %>%
                     filter(name %in% c("Virginia", "Maryland", "Delaware",
                                        "West Virginia", "Pennsylvania", "New York")) %>%
                     st_transform(crs = st_crs(watershed)))

us_states = subset(us_states(), 
                   !name %in% c("United States Virgin Islands",
                                "Commonwealth of the Northern Mariana Islands",
                                "Guam",
                                "American Samoa",
                                "Puerto Rico",
                                "Alaska",
                                "Hawaii")) %>%
              st_transform(crs = st_crs(2163))

####################################################
########################################  PLOT PARTS
####################################################

region_box <- st_as_sfc(st_bbox(states)) %>% st_transform(crs = st_crs(watershed))

region <- ggplot() + 
  geom_sf(data=us_states, fill = "white", size = 0.2) + 
  geom_sf(data=region_box, fill=NA, color='#9DBF9E', size=1.2) +
  theme_void()

####################################################
##############################################  PLOT
####################################################

study_area <- ggplot() +
  geom_sf(data=states,color='black',aes(fill=name),alpha=0.05) + 
  geom_sf(data=watershed,fill='#9DBF9E',alpha=0.4) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf_text(data=states, aes(label=stusps)) +
  scale_fill_carto_d(type ="diverging", palette="Earth") +
  theme_void() + 
  guides(fill=F) + 
  north(data=states,location="topright") + scalebar(data=states,location="bottomright",transform=T,dist=50,dist_unit="mi")
  
plot <- ggdraw() +
  draw_plot(study_area) +
  draw_plot(region, x = 0.05, y = 0.55, width = 0.35, height = 0.35)

ggsave(filename = "output\\figures\\study_area.png", 
       plot = plot,
       width = 6, 
       height = 8,
       dpi = 300)


## END OF SCRIPT. Have a great day! 