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
                      'ggplot2','ggpubr','ggnewscale','scales','showtext', 'ggsn', 'cowplot','RColorBrewer','ggrepel')
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

## function to read all files in a directory
read_all_sf = function(x) {
  read_sf(x) #%>% 
  # st_simplify(preserveTopology = T, dTolerance = 0.001) 
}

##########################
##################### data
##########################

## watershed boundaries
watershed.mrb = 
  st_read('store/Miss_RiverBasin/Miss_RiverBasin.shp') %>% 
  mutate(aes = 'Mississippi River Basin')
# watershed.cb  = 
#   st_read('maps/chesapeakeBayWatershed/Chesapeake_Bay_Watershed_Boundary.shp') %>% 
#   mutate(aes = 'Chesapeake Bay Watershed') %>%
#   st_transform(crs = st_crs(watershed.mrb))

## get state polygons
us.states = 
  subset(USAboundaries::us_states(),
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
# watershed.cb  %<>% st_intersection(us.states)

## results from get_reductions_in_mrb
start.read.time <- proc.time()
data.mrb = 
  list.files('output/mrb_emissions_reductions/', pattern = "*.shp", full.names = T) %>%
  map_df(~read_all_sf(.))
## stop clock
read.time = proc.time() - start.read.time

## check read time; 9 minutes
print('time it took to read and simplify data: ')
read.time

# ## drop outliers
# data.mrb %<>% 
#   filter(between(ch4_rdc, quantile(data.mrb$ch4_rdc, 0.25), quantile(data.mrb$ch4_rdc, 0.75))) ## drop outer quartiles to reduce size

## share crs
data.mrb %<>%
  st_transform(crs = st_crs(watershed.mrb))

## collect garbage
gc()

##########################
##################### plot
##########################

## total ch4 reductions
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

## collect garbage
gc()

# ## total ch4 reductions per hectare drop upper and lower 2.5%
# data.mrb.2  = 
#   data.mrb %>%
#   mutate(ch4.per.hec = ch4_rdc/ar_h_nw) %>% 
#   filter(between(ch4.per.hec, quantile(ch4.per.hec, 0.025), quantile(ch4.per.hec, 0.975)))
#         
# data.mrb.2 %>% st_drop_geometry %>% summary
# 
# plot =
#   ggplot() +
#   geom_sf(data  = watershed.mrb,
#           aes(color = aes,
#               fill  = aes),
#           alpha = 0.08) +
#   # geom_sf(data  = watershed.cb,
#   #         aes(color = aes,
#   #             fill  = aes),
#   #         alpha = 0.08) +
#   geom_sf(data  = us.states,
#           color = 'grey20',
#           alpha = 0.05) +
#   scale_color_manual(values = c('#56B4E9', '#9DBF9E'),
#                      guide   = guide_legend(title = '',
#                                             nrow = 2)) +
#   scale_fill_manual(values = c('#56B4E9', '#9DBF9E'),
#                     guide   = guide_legend(title = '',
#                                            nrow = 2)) +
#   new_scale_color() +
#   new_scale_fill() +
#   geom_sf(data  = data.mrb.2,
#           aes(color = ch4_rdc/ar_h_nw,
#               fill  = ch4_rdc/ar_h_nw)) +
#   scale_color_distiller(palette = 'Spectral',
#                         guide   = guide_colorbar(title          = expression(paste('Annual Reductions in  ', CH[4], ' Emissions (tonnes/hectare)')),
#                                                  title.position = 'top',
#                                                  barwidth       = unit(8.3, 'cm'))) +
#   scale_fill_distiller(palette = 'Spectral',
#                        guide   = guide_colorbar(title          = expression(paste('Annual Reductions in  ', CH[4], ' Emissions (tonnes/hectare)')),
#                                                 title.position = 'top',
#                                                 barwidth       = unit(8.3, 'cm'))) +
#   north(data     = us.states,
#         location = "bottomright") +
#   scalebar(data      = us.states,
#            location  = "bottomleft",
#            transform = F,
#            dist      = 250,
#            dist_unit = 'mi',
#            family    = 'sans-serif',
#            st.size   = 8) +
#   theme_void() +
#   theme(legend.position = 'bottom',
#         legend.title     = element_text(size = 32, color='grey20'),
#         legend.text      = element_text(size = 32, color='grey20'),
#         plot.caption     = element_text(size = 32, hjust = 0.5),
#         plot.title       = element_text(size = 32, hjust = 0.5),
#         text             = element_text(family = 'sans-serif', color = 'grey20'))
# 
# ## export
# # ggsave("output/figures/results_mrb_ch4.svg", plot, width  = 9, height = 6)
# ggsave("output/figures/results_mrb_ch4_per_hectare_omit_outliers.png", plot, width  = 9, height = 6)


## total ch4 reductions per hectare scatter
## get top 5
top5 =
  data.mrb %>%  
  st_drop_geometry %>% 
  arrange(desc(ch4_rdc)) %>% 
  slice(1:5) %>% 
  .$globald

plot = 
  data.mrb %>% 
  st_drop_geometry %>% 
  ggplot() +
  geom_point(aes(x     = ch4_rdc,
                 y     = ar_h_nw,
                 color = climate)) +
  geom_text_repel(data = subset(data.mrb, globald %in% top5), 
                  aes(x     = ch4_rdc,
                      y     = ar_h_nw,
                      label = globald)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = expression(paste('Annual Reductions in  ', CH[4], ' Emissions (tonnes)')),
       y = 'Waterbody Surface Area (hectares)',
       color = '') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        # legend.justification = c(1, 0),
        legend.title     = element_text(size = 14, color='grey20'),
        legend.text      = element_text(size = 16, color='grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 24),
        axis.text        = element_text(size = 24),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text.x     = element_text(size = 13), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        text             = element_text(family = "sans-serif", color = 'grey20')) +
  guides(color = guide_legend(nrow = 3))

## export
ggsave("output/figures/results_mrb_ch4_per_hectare_scatter.png", plot, width  = 5, height = 5)

## end of script. have a great day! 