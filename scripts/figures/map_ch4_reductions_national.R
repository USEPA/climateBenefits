##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'stringr',
                      'sf','USAboundaries',
                      'ggplot2','ggpubr','ggnewscale','scales','showtext', 'ggsn', 'cowplot','RColorBrewer')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
#################### parts
##########################

## colorblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
colors = c("#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000", "#56B4E9")

## add fonts
font_add_google("Quattrocento Sans", "sans-serif")
showtext_auto()

x = "output/mrb_emissions_reductions/mrb_emissions_reductions_AL.shp"
## function to read all files in a directory
read_all_sf = function(x) {
  read_sf(x) %>% 
    st_simplify(dTolerance = 2000)
}

##########################
##################### data
##########################

## watershed boundaries
watershed.mrb = 
  st_read('store/Miss_RiverBasin/Miss_RiverBasin.shp') %>% 
  mutate(aes = 'Mississippi River Basin')
watershed.cb  = 
  st_read('maps/chesapeakeBayWatershed/Chesapeake_Bay_Watershed_Boundary.shp') %>% 
  mutate(aes = 'Chesapeake Bay Watershed') %>%
  st_transform(crs = st_crs(watershed.mrb))

## get state polygons
us.states = subset(USAboundaries::us_states(),
                   !name %in% c("United States Virgin Islands",
                                "Commonwealth of the Northern Mariana Islands",
                                "Guam",
                                "American Samoa",
                                "Puerto Rico",
                                "Alaska",
                                "Hawaii")) %>%
  st_transform(crs = st_crs(watershed.mrb))

## trim watershed boundaries
watershed.mrb %<>% st_intersection(us.states)
watershed.cb  %<>% st_intersection(us.states)

## results from get_reductions_in_mrb
data.mrb = 
  list.files('output/mrb_emissions_reductions/', pattern = "*.shp", full.names = T) %>%
  map_df(~read_all_sf(.))

## drop outliers
data.mrb %<>% 
  filter(between(ch4_rdc, quantile(data.mrb$ch4_rdc, 0.25), quantile(data.mrb$ch4_rdc, 0.75)),
         ar_h_nw > quantile(data.mrb$ar_h_nw, 0.05))

## share crs
data.mrb %<>%
  st_transform(crs = st_crs(watershed.mrb))

## collect garbage
gc()

##########################
##################### plot
##########################

plot =
  ggplot() +
  geom_sf(data  = watershed.mrb,
          aes(color = aes,
              fill  = aes),
          alpha = 0.08) +
  # geom_sf(data  = watershed.cb,
  #         aes(color = aes,
  #             fill  = aes),
  #         alpha = 0.08) +
  geom_sf(data  = us.states,
          color = 'grey20',
          alpha = 0.05) +
  scale_color_manual(values = c('#56B4E9', '#9DBF9E'),
                     guide   = guide_legend(title = '',
                                            nrow = 2)) +
  scale_fill_manual(values = c('#56B4E9', '#9DBF9E'),
                    guide   = guide_legend(title = '',
                                           nrow = 2)) +
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data  = data.mrb,
          aes(color = ch4_rdc,
              fill  = ch4_rdc)) +
  scale_color_distiller(palette = 'Spectral',
                        guide   = guide_colorbar(title          = expression(paste('Annual Reductions in  ', CH[4], ' Emissions (tonnes)')),
                                                 title.position = 'top',
                                                 barwidth       = unit(7, 'cm'))) +
  scale_fill_distiller(palette = 'Spectral',
                       guide   = guide_colorbar(title          = expression(paste('Annual Reductions in  ', CH[4], ' Emissions (tonnes)')),
                                                title.position = 'top',
                                                barwidth       = unit(7, 'cm'))) +
  north(data     = us.states,
        location = "bottomright") +
  scalebar(data      = us.states,
           location  = "bottomleft",
           transform = F,
           dist      = 250,
           dist_unit = 'mi',
           family    = 'sans-serif',
           st.size   = 8) +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title     = element_text(size = 32, color='grey20'),
        legend.text      = element_text(size = 32, color='grey20'),
        plot.caption     = element_text(size = 32, hjust = 0.5),
        plot.title       = element_text(size = 32, hjust = 0.5),
        text             = element_text(family = 'sans-serif', color = 'grey20'))

## export
# ggsave("output/figures/results_mrb_ch4.svg", plot, width  = 9, height = 6)
ggsave("output/figures/results_mrb_ch4.png", plot, width  = 9, height = 6)

## end of script. have a great day! 