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

# libraries vis renv controlled library
library(tidyverse)
library(sf)
library(USAboundaries)
library(ggsn) # north and scale functions
library(cowplot) # ggdraw
library(tictoc)
library(arrow) # read parquet
####################################################
#################################  WORKING DIRECTORY
####################################################

## SET WORKING DIRECTORY
setwd(here())

## google
# register_google(key = "",write = TRUE)

####################################################
##############################################  DATA
####################################################

# Chesapeake Bay
watershed <- st_read('maps\\chesapeakeBayWatershed\\Chesapeake_Bay_Watershed_Boundary.shp') %>% mutate(aes = 'Chesapeake Bay\nWatershed')
# watershed %>% 
#   ggplot() +
#   geom_sf() + theme_void()

lakes <- st_read('store\\study_lakes\\study_lakes.gpkg') %>% mutate(aes = 'Waterbodies')
# lakes %>% 
#   ggplot() +
#   geom_sf() + theme_void()

shoreline <- st_read('maps\\Chesapeake_Bay_Shoreline_Medium_Resolution.shp')
# shoreline %>% 
#   ggplot() +
#   geom_sf() + theme_void()

states <- st_as_sf(us_states() %>%
                     filter(name %in% c("Virginia", "Maryland", "Delaware",
                                        "West Virginia", "Pennsylvania", "New York")) %>%
                     st_transform(crs = st_crs(watershed)))

##########################
############## Mississippi
##########################
ms_watershed <- st_read('store\\Miss_RiverBasin\\Miss_RiverBasin.shp') %>% 
  mutate(aes = 'Mississippi/Atchafalaya\nWatershed') %>%
  st_transform(crs = st_crs(watershed))

tic() # 96 seconds at home.  12sec in office.  read in list of lakes in watershed
ms_lakes_ <- list.files('output/lakes_in_mrb/', 
                        pattern = "*.parquet", 
                        full.names = T) %>%
     map_df(~read_parquet(.))
toc()
nrow(ms_lakes_) # 3,908,549
head(ms_lakes_)
unique(ms_lakes_$comid) # 289, including NA
table(ms_lakes_$comid) # several have many duplicates?  These are polygons broken up by intersection with state or NWI


# Read in all US flooded lands
us_flood_lds <- st_read('store/climateBenefits.gpkg', 'national') %>%
  mutate(aes = 'Waterbodies') %>%
  st_transform(crs = st_crs(watershed))
nrow(us_flood_lds) #7,074,938


us_flood_lds %>% st_drop_geometry %>% filter(comid %in% unique(na.omit(ms_lakes_$comid))) %>% nrow #11,618 
us_flood_lds %>% st_drop_geometry %>% filter(globalid %in% unique(na.omit(ms_lakes_$globalid))) %>% nrow #3,897,585



tic() # 19.5 seconds. filter out those included in MS watershed
ms_lakes <- us_flood_lds %>% filter((comid %in% unique(na.omit(ms_lakes_$comid)))| 
                                      (globalid %in% unique(na.omit(ms_lakes_$globalid))))
toc()
nrow(ms_lakes) #3,909,203

# make subset for code development 
#ms_lakes <- ms_lakes %>% slice_sample(prop = 0.10)
# ms_lakes %>%
#   ggplot() +
#   geom_sf(color = NA, fill = "deepskyblue2") + theme_void()


## Get Mississippi River basin states
ms_states <- us_states() %>% 
  st_filter(ms_watershed)
# ms_states %>%
#   ggplot() +
#   geom_sf() + theme_void()

## Continernous states for locater map
contim_states = subset(us_states(),  
                   !name %in% c("United States Virgin Islands",
                                "Commonwealth of the Northern Mariana Islands",
                                "Guam",
                                "American Samoa",
                                "Puerto Rico",
                                "Alaska",
                                "Hawaii")) %>%
              st_transform(crs = st_crs(5070))

####################################################
########################################  PLOT PARTS
####################################################

region <- ggplot() + 
  geom_sf(data=contim_states, fill = "white", size = 0.2) + 
  #geom_sf(data=region_box, fill=NA, color='#9DBF9E', size=1.2) +
  geom_sf(data=watershed, fill = "#9DBF9E", alpha = 0.4) +
  geom_sf(data = ms_watershed, fill = "#996633", alpha = 0.4) +
  #ggsn::north(data=contim_states,location="topleft", scale =0.25) + 
  theme_void()

####################################################
##############################################  PLOT
####################################################

cb_study_area <- ggplot() +
  geom_sf(data=states,color='black',alpha=0.05) + 
  geom_sf(data=watershed,fill="#9DBF9E",alpha=0.4) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf(data=lakes,color='deepskyblue2',fill='deepskyblue2') + 
  geom_sf_text(data=states, aes(label=stusps)) +
  #scale_fill_manual(values=c("#9DBF9E",'deepskyblue2')) +
  # scale_color_carto_d(type ="diverging", palette="Earth") +
  theme_void() +
  #ggsn::north(data=states,location="topleft", scale =0.25) + 
  scalebar(data=states,location="bottomright",
           transform=T,dist=50,dist_unit="mi", st.size = 2) +
  labs(fill='') +
  theme(legend.position = c(0.1, 0.9),
        legend.key = element_rect(color=NA),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Chesapeake Bay Watershed")


ms_study_area <- ggplot() +
  geom_sf(data=ms_states,color='black',alpha=0.05) + 
  geom_sf(data=ms_watershed, fill="#996633", alpha = 0.4) + 
  geom_sf(data=ms_lakes, color='deepskyblue2', size = 0.1, aes(fill=aes)) + 
  geom_sf_text(data=ms_states, aes(label=stusps)) +
  scale_fill_manual(values='deepskyblue2') +
  # scale_color_carto_d(type ="diverging", palette="Earth") +
  theme_void() +
  ggsn::north(data=ms_states,location = "bottomright",
              scale = 0.2) + 
  scalebar(data=ms_states,location="bottomleft",
           transform=T,dist=250,dist_unit="mi", st.size = 2) +
  labs(fill='') +
  ggtitle("Mississippi/Atchafalya Watershed") +
  theme(legend.position = c(0.15, 0.17),
        legend.key = element_rect(color=NA),
        plot.title = element_text(hjust = 0.5)) 
  

# plot <- cowplot::ggdraw() +
#   draw_plot(cb_study_area, x = 0.5, y = 0.5, width = 0.35, height = 0.35) +
#   draw_plot(ms_study_area, x=0.25, y=0.01, width = 0.75) +
#   draw_plot(region, x = 0.05, y = 0.75, width = 0.35, height = 0.35)

top_row <- plot_grid(region, cb_study_area)
plot <- plot_grid(top_row, ms_study_area, ncol=1)
ggsave(filename = "output\\figures\\combined_study_area.png", 
       plot = plot,
       width = 6, 
       height = 8,
       dpi = 300)


## END OF SCRIPT. Have a great day! 