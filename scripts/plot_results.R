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

packages <- c('data.table','tidyverse','magrittr','sf','readxl','USAboundaries','USAboundariesData',
              'viridis','epiDisplay',
              'ggmap','ggplot2','ggpubr','cowplot',
              'rnaturalearth','rnaturalearthdata',
              'maps','mapview','here','rcartocolor','ggsn','extrafont','dataRetrieval')
lapply(packages, pkgTest)

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

watershed  <- st_read('maps\\chesapeakeBayWatershed\\Chesapeake_Bay_Watershed_Boundary.shp') %>% mutate(aes = 'Chesapeake Bay\nWatershed')
# ches_lakes <- st_read('store\\study_lakes\\study_lakes.shp')
shoreline <- st_read('maps\\Chesapeake_Bay_Shoreline_Medium_Resolution.shp')
results    <- readRDS('output\\dat.pt.rds')

states <- st_as_sf(us_states() %>%
                     filter(name %in% c("Virginia", "Maryland", "Delaware",
                                        "West Virginia", "Pennsylvania", "New York")) %>%
                     st_transform(crs = st_crs(watershed)))

county <- st_as_sf(us_counties(resolution="high", states=c("Virginia", "Maryland", "Delaware",
                                                           "West Virginia", "Pennsylvania", "New York"))) %>%
                     st_transform(crs = st_crs(watershed))

us_states = subset(us_states(), 
                   !name %in% c("United States Virgin Islands",
                                "Commonwealth of the Northern Mariana Islands",
                                "Guam",
                                "American Samoa",
                                "Puerto Rico",
                                "Alaska",
                                "Hawaii")) %>%
                    st_transform(crs = st_crs(5070))

## helper function
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}
cut_co2 <- function(x,n) {
  cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 1/n),na.rm = T)), 
      include.lowest=TRUE,labels=qco2)
}
cut_ch4 <- function(x,n) {
  cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 1/n),na.rm = T)), 
      include.lowest=TRUE,labels=qch4)
}
cut_n2o <- function(x,n) {
  cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 1/n),na.rm = T)), 
      include.lowest=TRUE,labels=qn2o)
}
####################################################
#############################################  CLEAN
####################################################
sizes <- c("Small","Medium","Large")
summary(results$scco2)
qco2 <- c("Lower 25%: < $32k","25% to 50%: $32k-$65k","50% to 75%: $65k-$120k","75% to 100%: > $120k")

summary(results$scch4)
qch4 <- c("Lower 25%: < $7k","25% to 50%: $7k-$13k","50% to 75%: $13k-$22k","75% to 100%: > $22k")

summary(results$scn2o)
qn2o <- c("Lower 25%: < $1k","25% to 50%: $1k-$14k","50% to 75%: $14k-$20k","75% to 100%: > $20k")

results %<>% mutate(scco2 = co2diff * 140,
                    scch4 = ch4diff * 3400,
                    scn2o = n2odiff * 50000,
                    lake_size = cut(LakeVolume,breaks=3,labels=sizes),
                    scco2_bin = cut_co2(co2diff * 140,4),
                    scch4_bin = cut_ch4(ch4diff * 3400,4),
                    scn2o_bin = cut_n2o(n2odiff * 50000,4))


# ####################################################
# ########################################  PLOT PARTS
# ####################################################
# 
# region_box <- st_as_sfc(st_bbox(states)) %>% st_transform(crs = st_crs(watershed))
# 
# region <- ggplot() + 
#   geom_sf(data=us_states, fill = "white", size = 0.2) + 
#   geom_sf(data=region_box, fill=NA, color='#9DBF9E', size=1.2) +
#   theme_void()

####################################################
##############################################  PLOT
####################################################

scco2 <- ggplot() +
  geom_sf(data=states,color='black',fill='white') + 
  geom_sf(data=watershed,fill="#9DBF9E",alpha=0.01) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf(data=results,aes(alpha=lake_size, color=scco2_bin)) + 
  geom_sf_text(data=states, aes(label=stusps)) +
  # scale_alpha_continuous(range = c(0.1, 1)) +
  # scale_color_distiller(palette = "BrBG") +
  # scale_colour_brewer(palette = "Accent") +
  scale_color_carto_d(type ="diverging", palette="Earth") +
  # scale_color_viridis_d() +
  theme_void() +
  # north(data=states,location="topright") + 
  # scalebar(data=states,location="bottomright",transform=T,dist=50,dist_unit="mi") +
  labs(color=expression(paste('SC-CO'[2],' Quartile')),
       caption = "Note: Transparency is proportional to waterbody volume") +
  theme(legend.position = c(0.88, 0.3),
        legend.key = element_rect(color=NA)) +
  guides(alpha=FALSE)

ggsave(filename = "output\\figures\\scco2.png", 
       plot = scco2,
       width = 8, 
       height = 8,
       dpi = 300)



scch4 <- ggplot() +
  geom_sf(data=states,color='black',fill='white') + 
  geom_sf(data=watershed,fill="#9DBF9E",alpha=0.01) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf(data=results,aes(alpha=lake_size, color=scch4_bin)) + 
  geom_sf_text(data=states, aes(label=stusps)) +
  # scale_alpha_continuous(range = c(0.1, 1)) +
  # scale_color_distiller(palette = "BrBG") +
  # scale_colour_brewer(palette = "Accent") +
  scale_color_carto_d(type ="diverging", palette="Earth") +
  # scale_color_viridis_d() +
  theme_void() +
  # north(data=states,location="topright") + 
  # scalebar(data=states,location="bottomright",transform=T,dist=50,dist_unit="mi") +
  labs(color=expression(paste('SC-CH'[2],' Quartile')),
       caption = "Note: Transparency is proportional to waterbody volume") +
  theme(legend.position = c(0.88, 0.3),
        legend.key = element_rect(color=NA)) +
  guides(alpha=FALSE)

ggsave(filename = "output\\figures\\scch4.png", 
       plot = scch4,
       width = 8, 
       height = 8,
       dpi = 300)


scn2o <- ggplot() +
  geom_sf(data=states,color='black',fill='white') + 
  geom_sf(data=watershed,fill="#9DBF9E",alpha=0.01) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf(data=results,aes(alpha=lake_size, color=scn2o_bin)) + 
  geom_sf_text(data=states, aes(label=stusps)) +
  # scale_alpha_continuous(range = c(0.1, 1)) +
  # scale_color_distiller(palette = "BrBG") +
  # scale_colour_brewer(palette = "Accent") +
  scale_color_carto_d(type ="diverging", palette="Earth") +
  # scale_color_viridis_d() +
  theme_void() +
  # north(data=states,location="topright") + 
  # scalebar(data=states,location="bottomright",transform=T,dist=50,dist_unit="mi") +
  labs(color=expression(paste('SC-N'[2],'O',' Quartile')),
       caption = "Note: Transparency is proportional to waterbody volume") +
  theme(legend.position = c(0.88, 0.3),
        legend.key = element_rect(color=NA)) +
  guides(alpha=FALSE)

ggsave(filename = "output\\figures\\scn2o.png", 
       plot = scn2o,
       width = 8, 
       height = 8,
       dpi = 300)



####################################################
#############################################  CLEAN
####################################################
sizes <- c("Small","Medium","Large")
summary(results$co2diff)
qco2 <- c("Lower 25%: < 51 MT/yr","25% to 50%: 51-92 MT/yr","50% to 75%: 92-151 MT/yr","75% to 100%: > 151 MT/yr")

summary(results$ch4diff)
qch4 <- c("Lower 25%: < 9 MT/yr","25% to 50%: 9-19 MT/yr","50% to 75%: 19-35 MT/yr","75% to 100%: > 35 MT/yr")

summary(results$n2odiff)
qn2o <- c("Lower 25%: < 0.01 MT/yr","25% to 50%: 0.01-0.03 MT/yr","50% to 75%: 0.03-0.04 MT/yr","75% to 100%: > 0.04 MT/yr")

results %<>% mutate(scco2 = co2diff * 140,
                    scch4 = ch4diff * 3400,
                    scn2o = n2odiff * 50000,
                    lake_size = cut(LakeVolume,breaks=3,labels=sizes),
                    co2diff_bin = cut_co2(co2diff,4),
                    ch4diff_bin = cut_ch4(ch4diff,4),
                    n2odiff_bin = cut_n2o(n2odiff,4))


# ####################################################
# ########################################  PLOT PARTS
# ####################################################
# 
# region_box <- st_as_sfc(st_bbox(states)) %>% st_transform(crs = st_crs(watershed))
# 
# region <- ggplot() + 
#   geom_sf(data=us_states, fill = "white", size = 0.2) + 
#   geom_sf(data=region_box, fill=NA, color='#9DBF9E', size=1.2) +
#   theme_void()

####################################################
##############################################  PLOT
####################################################

scco2 <- ggplot() +
  geom_sf(data=states,color='black',fill='white') + 
  geom_sf(data=watershed,fill="#9DBF9E",alpha=0.01) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf(data=results,aes(alpha=lake_size, color=co2diff_bin)) + 
  geom_sf_text(data=states, aes(label=stusps)) +
  # scale_alpha_continuous(range = c(0.1, 1)) +
  # scale_color_distiller(palette = "BrBG") +
  # scale_colour_brewer(palette = "Accent") +
  scale_color_carto_d(type ="diverging", palette="Earth") +
  # scale_color_viridis_d() +
  theme_void() +
  # north(data=states,location="topright") + 
  # scalebar(data=states,location="bottomright",transform=T,dist=50,dist_unit="mi") +
  labs(color=expression(paste('Avoided CO'[2],'Quartiles')),
       caption = "Note: Transparency is proportional to waterbody volume") +
  theme(legend.position = c(0.88, 0.3),
        legend.key = element_rect(color=NA)) +
  guides(alpha=FALSE)

ggsave(filename = "output\\figures\\co2diff.png", 
       plot = scco2,
       width = 8, 
       height = 8,
       dpi = 300)



scch4 <- ggplot() +
  geom_sf(data=states,color='black',fill='white') + 
  geom_sf(data=watershed,fill="#9DBF9E",alpha=0.01) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf(data=results,aes(alpha=lake_size, color=ch4diff_bin)) + 
  geom_sf_text(data=states, aes(label=stusps)) +
  # scale_alpha_continuous(range = c(0.1, 1)) +
  # scale_color_distiller(palette = "BrBG") +
  # scale_colour_brewer(palette = "Accent") +
  scale_color_carto_d(type ="diverging", palette="Earth") +
  # scale_color_viridis_d() +
  theme_void() +
  # north(data=states,location="topright") + 
  # scalebar(data=states,location="bottomright",transform=T,dist=50,dist_unit="mi") +
  labs(color=expression(paste('Avoided CH'[4],'Quartiles')),
       caption = "Note: Transparency is proportional to waterbody volume") +
  theme(legend.position = c(0.88, 0.3),
        legend.key = element_rect(color=NA)) +
  guides(alpha=FALSE)

ggsave(filename = "output\\figures\\ch4diff.png", 
       plot = scch4,
       width = 8, 
       height = 8,
       dpi = 300)


scn2o <- ggplot() +
  geom_sf(data=states,color='black',fill='white') + 
  geom_sf(data=watershed,fill="#9DBF9E",alpha=0.01) + 
  geom_sf(data=shoreline,fill='white') + 
  geom_sf(data=results,aes(alpha=lake_size, color=n2odiff_bin)) + 
  geom_sf_text(data=states, aes(label=stusps)) +
  # scale_alpha_continuous(range = c(0.1, 1)) +
  # scale_color_distiller(palette = "BrBG") +
  # scale_colour_brewer(palette = "Accent") +
  scale_color_carto_d(type ="diverging", palette="Earth") +
  # scale_color_viridis_d() +
  theme_void() +
  # north(data=states,location="topright") + 
  # scalebar(data=states,location="bottomright",transform=T,dist=50,dist_unit="mi") +
  labs(color=expression(paste('Avoided N'[2],'O','Quartiles')),
       caption = "Note: Transparency is proportional to waterbody volume") +
  theme(legend.position = c(0.88, 0.3),
        legend.key = element_rect(color=NA)) +
  guides(alpha=FALSE)

ggsave(filename = "output\\figures\\n2odiff.png", 
       plot = scn2o,
       width = 8, 
       height = 8,
       dpi = 300)


####################################################
##############################################  HIST
####################################################

# ggplot(results) +
#   geom_histogram(aes(y=5*..density.., fill=discount_rate, color=discount_rate), alpha=0.1, position='identity', binwidth=5) +
#   geom_segment(data=means, aes(x=mean, y=0, xend=mean, yend=yupper, color=discount_rate), alpha=0.9, size=1, linetype="dotdash", show.legend = F) +
#   annotate("text", x = means$mean + 19, y = means$yupper + 0.005, label=means$text, fontface=means$fontface) +
#   annotate("text", x = means[means$label=='3.0%',]$mean + 19, y = means[means$label=='3.0%',]$yupper + 0.02, label='Central Estimate', fontface='bold') +
#   scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks=scales::extended_breaks(n = 7)) + 
#   scale_x_continuous(breaks=seq(0,220,20),limits=c(-5, 230),expand=c(0,0)) +
#   # scale_fill_carto_d(type = "diverging", palette = "Bold") +    
#   scale_color_carto_d(type = "diverging", palette = "Bold") +
#   labs(title=expression(paste('DICE 2010: Frequency Distribution of SC-',CO[2],' Estimates for 2020')),
#        y='Fraction of Simulations',
#        x=expression(paste('Social Cost of Carbon in 2020 [2007$ / metric ton ',CO[2],']')),
#        fill='Discount Rate',
#        color='Discount Rate',
#        group='Discount Rate') +
#   guides(color = guide_legend(reverse=T),
#          fill = guide_legend(reverse=T)) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 16,hjust=0.5),
#         panel.grid.minor.y=element_blank(),
#         panel.grid.major.y=element_line(color="gray90",size=0.25),
#         panel.grid.minor.x=element_blank(),
#         panel.grid.major.x=element_line(color="gray90",size=0.25),
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16),
#         legend.position = c(0.9, 0.5))


## END OF SCRIPT. Have a great day! 