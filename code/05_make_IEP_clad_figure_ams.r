# Make Figure:

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(lubridate)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(viridis)
library(ggspatial)

# LOAD DATA ---------------------------------------------------------------

## this data came from the CDFW IEP Zooplankton Data: ftp://ftp.dfg.ca.gov/IEP_Zooplankton/
## more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON

options(scipen=12)

level_key <- c(`1` = "Winter (Jan-Feb)", `2` = "Winter (Jan-Feb)", `3` = "Early Spring (Mar-Apr)", `4` = "Early Spring (Mar-Apr)", `5` = "Late Spring (May-Jun)", `6` = "Late Spring (May-Jun)")

load("data/zoop_combined_sf_2014-2018.rda")

# View all Sites
# zoop_comb_sf %>% group_by(station_code) %>% select(location, lat, lon) %>% distinct(.keep_all = TRUE) %>% View()

# recode
clad_filt <- zoop_comb_sf %>% select(-core) %>%
  mutate("season"=recode_factor(month, !!!level_key)) %>%
  rename("station"=station_code) %>%
  group_by(station, year, season, lat, lon) %>% complete(nesting(station), season, year=2014:2018) %>%
  summarize(allclad_mean = mean(cpue, na.rm = TRUE),
            allclad_sum = sum(cpue, na.rm = TRUE),
            allcladocera_log = log(allclad_mean + 1)) %>%
  mutate(allcladocera_log = replace_na(allcladocera_log, 0),
         location=if_else(station=="STTD", "Yolo Bypass - Screw Trap at Toe Drain", "Sacramento River at Sherwood Harbor")) %>%
  ungroup() %>%
  # make spatial
  st_as_sf(., coords = c("lon", "lat"), remove = F, crs=4326)

# the seasons need to be reordered in plot
clad_filt$season = fct_relevel(clad_filt$season, "Winter (Jan-Feb)", "Early Spring (Mar-Apr)", "Late Spring (May-Jun)")

# GET SPATIAL DATA --------------------------------------------------------

# Read in shapefile and transform polygon
delta_sf <- st_read("data/spatial/Bay_delta_selection.shp") %>% st_transform(crs=4326)

# crop to region of interest...warning is ok!
crop_box <- st_bbox(c(xmin = -122.25, xmax = -121.22, ymax = 38.65, ymin = 37.77), crs = st_crs(4326))

# crop delta
delta_crop <- st_crop(delta_sf, crop_box)
clad_crop <- st_crop(clad_filt, crop_box) # warning is ok!


# ADD CUT GROUPS ----------------------------------------------------------


# add a cut number to the table based on zoop numbers:
# clad_crop <- clad_crop %>%
#   mutate(#allcladocera_log = replace(allcladocera_log, !is.finite(allcladocera_log), 0), # replace -Inf w 0
#          # replace negative values
#          #allcladocera_log = replace(allcladocera_log, allcladocera_log<0, .1),
#          allclad_cut = as.integer(Hmisc::cut2(allcladocera_log, g=5))*.8) # make breaks

summary(clad_crop$allclad_mean)

# Define circle radius and color
clad_crop$clad_cut <- as.numeric(cut(clad_crop$allclad_mean, breaks=c(0,50,500,2000,10000,30000,52000), include.lowest=TRUE))
radii<-c(2,5,11,18,25,33)
clad_crop$radius<-radii[clad_crop$clad_cut]
labs <- c("0-50","50-500","500-2000","2000-10000","10000-30000","30000-52000")
clad_crop$N_cat<-labs[clad_crop$clad_cut]

clad_crop$N_cat = fct_relevel(clad_crop$N_cat, "0-50","50-500","500-2000","2000-10000","10000-30000","30000-52000")


save(clad_crop, file = "data_output/clad_filt_sf_2014_2018_ams.rda")

# Quick Facet Map -------------------------------------------------------------

# for fancy scale bar & North Arrow
#annotation_scale(location = "bl", height = unit(0.14, "cm"), pad_y = unit(0.2, "cm")) +
#annotation_north_arrow(location = "bl",height = unit(0.8, "cm"),width = unit(0.8, "cm"), pad_y = unit(0.6, "cm"),style = north_arrow_fancy_orienteering(text_size = 8), which_north = "true") +

library(ggtext)
library(ggspatial)
#library(basemapR) # https://github.com/Chrisjb/basemapR

### NOTES: Also, could you add 2018 to Fig 7 and add units to the caption please?  If you think that adding in 2018 makes the fig too busy/big I'm wondering if we could plot the mean abundance per station for 2014 and 2015 and call those two years "2014-15" to parallel the fish plots. Thoughts?

# MAP
ggplot() +
  # basemaps, see: #positron #hydda #voyager #mapnik
  #base_map(bbox = st_bbox(clad_crop), basemap = 'hydda', increase_zoom = 3) +
  theme_bw(base_family = "Roboto Condensed") +
  # plot delta
  geom_sf(data=delta_crop, fill="steelblue", color="steelblue", alpha=0.7, inherit.aes=F) +
  # plot sites
  geom_sf(data=clad_crop, pch=21, color="gray30", fill="gray80", alpha=0.8, size=1) +
  # plot clad data
  geom_sf(data=filter(clad_crop, !is.nan(allclad_mean)), aes(fill=N_cat, size=radius),
          pch=21, color="gray20", alpha=0.9) +
  # geom_sf(data=clad_crop, aes(fill=allcladocera_log, size=allclad_cut),
  #         pch=21, color="gray20", alpha=0.9) +
  # plot scale: expression(paste("line1 \n line2 a" [b]))
  scale_fill_viridis_d(name="Mean no. per m^2", option = "A") +
  # scale_fill_viridis_c(name="Abundance <br>(no. indiv m^sq^)", option = "A",
  #                      breaks=c(0,2,4,6,8,10,12,14),
  #                      labels=c("0","7","55","400","3,000","22,000","163,000","1,203,000"),
  #                      na.value = "transparent", limits=c(0,14),
  #                      guide=guide_colorbar(draw.ulim = F, draw.llim = F)) +

  guides(size=FALSE) +
  # labs(title = paste0("All Cladocerans: 2014-2018"), x="", y="",
  #      caption = "Data Source: CDFW IEP Zooplankton Clarke-Bumpus \n{https://www.wildlife.ca.gov/Conservation/Delta/Zooplankton-Study}") +
  ggplot2::coord_sf(label_axes = "E", crs=4326) + #xlim = c(-122.25, -121.35), ylim=c(38.6, 37.85),
  facet_grid(year~season) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_markdown(),
        panel.grid.major = element_line(colour = 'transparent'),
        strip.background=element_rect(fill="grey23"),
        strip.text=ggtext::element_textbox(size=11, color = "white", box.color = "grey23"))# "#5D729D"))

# save
ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_faceted_A_revised_ams.png", width = 8, height = 8, dpi=300, units = "in", type="cairo") # may need to add type="cairo"

ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_faceted_A_revised.pdf", width = 8.5, height = 11, dpi=300, units = "in", device=cairo_pdf)

#ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_facet_D.png", width = 11, height = 8.5, dpi=300, units = "in")
#ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_facet.pdf", width = 11, height = 8.5, dpi=300, units = "in")

# SETUP AND MAP ----------------------------------------------------------


# # Define the legend scale bar with the min/max abundance across all data you are going to show (otherwise the scale bar changes between time steps)
# YY <- 2018
# MM <- 5
# min_obs <- 0 # log transformed
# max_obs <- 13 # log transformed
#
#
# # filter to specific year or years
# cb_yr <- cb_clad_crop %>% filter(year==YY, mm==MM)
# #cb_yr1805 <- cb_clad_crop %>% filter(year==YY, mm==MM)
#
# # quick map
# mapview(cb_yr, zcol="allcladocera_log", cex="allcladocera_log", lwd=1.5,
#         layer.name=paste0(YY, "-", month.abb[MM],"\nAll Cladocera (log)")) +
# # add all stations
# mapview(stations_crop, col.regions="white", lwd=0.7, cex=2, alpha=0.7, legend=FALSE)

# BUILD MANUAL WEBMAP -----------------------------------------------------

# col_pal <- colorNumeric(viridis(64, option = "B"), c(min_obs, max_obs), na.color = NA)
#
# leaflet() %>%
#   setView(lng = -121.7961, lat=38.06166, zoom=11) %>%
#   addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
#   addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
#   addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OpenBW") %>%
#   addProviderTiles("Esri.WorldGrayCanvas", group="ESRI Canvas") %>%
#
#   # point layer
#   addCircleMarkers(data=cb_yr, ~lon, ~lat, group=paste0(YY, "-",MM),
#                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                    weight= 1, fillOpacity=0.9, stroke=TRUE,
#                    popup = paste0(
#                      "All Cladocerans = ", cb_yr$allcladocera,
#                      "<br> All Cladoceran (log) = ", cb_yr$allcladocera_log,
#                      "<br> Station: ", cb_yr$station,
#                      "<br> Lon: ", cb_yr$lon,
#                      "<br> Lat: ", cb_yr$lat,
#                      "<br> Region: ", cb_yr$region,
#                      "<br> Temperature: ", cb_yr$temperature)) %>%
#
#   # all sites
#   addCircleMarkers(data=stations_crop, ~lon, ~lat, group="Stations",
#                    weight= 0.8, fill = T, fillOpacity=0.7, stroke=TRUE,
#                    fillColor = "skyblue", color="black", radius=2.5,
#                    popup = paste0(
#                      "Station = ", stations_crop$station,
#                      "<br> Current = ", stations_crop$current,
#                      "<br> Location: ", stations_crop$location,
#                      "<br> Yr Start: ", stations_crop$year_start,
#                      "<br> Yr End: ", stations_crop$year_end)) %>%
#
#
# # addCircleMarkers(data=cb_yr1405, ~lon, ~lat, group="May2014",
# #                  radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
# #                  weight= 1, fillOpacity=0.7, stroke=TRUE,
# #                  popup = paste0(
# #                    "All Cladocerans = ", cb_yr1405$allcladocera,
# #                    "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
# #                    "<br> Station: ", cb_yr1405$station,
# #                    "<br> Lon: ", cb_yr1405$lon,
# #                    "<br> Lat: ", cb_yr1405$lat,
# #                    "<br> Region: ", cb_yr1405$region,
# #                    "<br> Temperature: ", cb_yr1405$temperature)) %>%
# #
# #   addCircleMarkers(data=cb_yr1505, ~lon, ~lat, group="May2015",
# #                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
# #                    weight= 1, fillOpacity=0.7, stroke=TRUE,
# #                    popup = paste0(
# #                      "All Cladocerans = ", cb_yr1405$allcladocera,
# #                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
# #                      "<br> Station: ", cb_yr1405$station,
# #                      "<br> Lon: ", cb_yr1405$lon,
# #                      "<br> Lat: ", cb_yr1405$lat,
# #                      "<br> Region: ", cb_yr1405$region,
# #                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
# #   addCircleMarkers(data=cb_yr1605, ~lon, ~lat, group="May2016",
# #                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
# #                    weight= 1, fillOpacity=0.7, stroke=TRUE,
# #                    popup = paste0(
# #                      "All Cladocerans = ", cb_yr1405$allcladocera,
# #                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
# #                      "<br> Station: ", cb_yr1405$station,
# #                      "<br> Lon: ", cb_yr1405$lon,
# #                      "<br> Lat: ", cb_yr1405$lat,
# #                      "<br> Region: ", cb_yr1405$region,
# #                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
# #   addCircleMarkers(data=cb_yr1705, ~lon, ~lat, group="May2017",
# #                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
# #                    weight= 1, fillOpacity=0.7, stroke=TRUE,
# #                    popup = paste0(
# #                      "All Cladocerans = ", cb_yr1405$allcladocera,
# #                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
# #                      "<br> Station: ", cb_yr1405$station,
# #                      "<br> Lon: ", cb_yr1405$lon,
# #                      "<br> Lat: ", cb_yr1405$lat,
# #                      "<br> Region: ", cb_yr1405$region,
# #                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
# #   addCircleMarkers(data=cb_yr1805, ~lon, ~lat, group="May2018",
# #                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
# #                    weight= 1, fillOpacity=0.7, stroke=TRUE,
# #                    popup = paste0(
# #                      "All Cladocerans = ", cb_yr1405$allcladocera,
# #                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
# #                      "<br> Station: ", cb_yr1405$station,
# #                      "<br> Lon: ", cb_yr1405$lon,
# #                      "<br> Lat: ", cb_yr1405$lat,
# #                      "<br> Region: ", cb_yr1405$region,
# #                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
# #
#
#   addLegend(pal = col_pal, values = c(min_obs, max_obs), title = "log(All Clad.)") %>%
#   addFullscreenControl() %>%
#   #addControl(paste0(month.abb[MM], "-", YY), position = "bottomleft") %>%
#   addLayersControl(position = "topleft",
#                    baseGroups = c("ESRI Canvas", "OpenBW",
#                                   "Topo","ESRI Aerial"),
#                    overlayGroups = c(paste0(YY, "-",MM), "Stations"),
#                                      #"May2014","May2015","May2016","May2017","May2018"),
#                    options = layersControlOptions(collapsed = T))
#
#

# MAKE STATIC MAP ---------------------------------------------------------

library(ggmap)
library(ggspatial)


ggbbox <- c(left = -122.25, right = -121.22, top = 38.65, bottom = 37.77)
#location=c(-121.82,38.05) # set the center of the map
# set the background map up
map1 <- get_map(location=ggbbox, crop = F,
                color="bw",
                maptype="terrain",
                source="google",
                zoom=9)

yr <- 2017
#mon <- c(2:5)
season <- "Winter (Jan-Feb)"
min_obs <- 0 # log transformed
max_obs <- 12 # log transformed

ggmap(map1) +
#ggplot() +
  annotation_scale(location = "bl", height = unit(0.14, "cm"), pad_y = unit(0.2, "cm")) +
  theme_bw(base_size = 9) +#base_family = "Roboto Condensed") +
  annotation_north_arrow(location = "bl",height = unit(0.8, "cm"),width = unit(0.8, "cm"),
                         pad_y = unit(0.6, "cm"),style = north_arrow_fancy_orienteering(text_size = 8),
                         which_north = "true") +
  # scale_fill_viridis_c("log(Clad)", option = "A",
  #                      na.value = "transparent", limits=c(0,14)) +
  #scale_size_continuous(guide = TRUE) +
  scale_fill_viridis_c(name="Abundance <br>(no. indiv m^2^)", option = "A",
                       breaks=c(0,2,4,6,8,10,12),
                       labels=c("0","7","55","400","3,000","22,000","163,000"),
                       na.value = "transparent", limits=c(0,12),
                       guide=guide_colorbar(draw.ulim = F, draw.llim = F)) +
  geom_sf(data=delta_crop, fill="steelblue", color="steelblue", lwd=0.2, alpha=0.5, inherit.aes=F) +
  guides(size=FALSE) +
  geom_sf(data=clad_crop[clad_crop$year==yr,],
          aes(fill=allcladocera_log, size=allclad_cut), pch=21, color="gray20", alpha=0.9) +
  labs(x="", y="",
       caption = "Data Source: CDFW IEP Zooplankton Clarke-Bumpus\n{https://www.wildlife.ca.gov/Conservation/Delta/Zooplankton-Study}") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_markdown(),
        panel.grid.major = element_line(colour = 'transparent'),
        strip.background=element_rect(fill="#5D729D"),
        strip.text=ggtext::element_textbox(size=11, color = "white", box.color = "#5D729D"))+
  #coord_sf(xlim = c(-122.25, -121.35), ylim=c(38.38, 37.85), crs=4326) +
  facet_grid(year~season)

# w background
ggsave(filename = paste0("figures/all_cladocera_points_", yr, "_ggmap.pdf"),width = 10, height = 8, units = "in")

# no background
ggsave(filename = paste0("figures/all_cladocera_points_", yr, ".pdf"),width = 10, height = 8, units = "in")
