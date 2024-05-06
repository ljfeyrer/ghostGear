#mpa map debris

# April 2024

pacman::p_load(sp, terra, dplyr, sf, viridis, ggplot2, ggrepel, stringr, 
               here, ggtext, readr,
               pals, tidyr, fuzzyjoin, 
               patchwork,mapsf,readxl,
               ggforce, readr, ggspatial, lubridate, stars, scales, 
               RColorBrewer, grafify)

shapefiles ="~/CODE/shapefiles/"
# 
# debris_2022 = all_data%>%filter(!is.na(Lat))%>%st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# write_sf(debris_2022, "output/debris_2022.shp")



#read shapefiles----

# ROV shapes

debris_2022 <- read_sf("output/debris_2022.shp")
ROV_NAV_sf <- read_sf("output/ROV_NAV_sf.shp")
gearNoted_sf <- read_sf("output/gearNotedlive_sf.shp")


#EEZ
EEZ <- read_sf(paste0(shapefiles, "EEZ/EEZ_can.shp"))

##MPAs & conservation zones compiled from layers available as shapefiles -------
#CH - No Zone 1
NBW_CH <- read_sf(paste0(shapefiles,"SAR_CH/NBW_CH/NorthernBottlenoseWhale_CH.shp"))

#read in Gully Zone 
Gully <- read_sf(paste0(shapefiles,"ProtectedAreas/DFO/Gully/Gully_MPA.shp"))%>%filter(NAME != "Zone 1")

#MAP FIGURE

# Get the vertical and horizontal limits

# make bound box around data-----
Bound_boxB <- st_bbox( c(xmin = -60.2,ymin = 42.5, xmax = -56, ymax =45 ), crs = st_crs(4326))
Bound_boxB <- Bound_boxB %>%
  st_as_sfc()%>% #turns the bounding box into a sfc object, that just describes a specific geometry
  st_sf()
Bound_boxv = vect(Bound_boxB)

lims = lims <- list(
  x = c(xmin = -60.2, xmax = -56),
  y = c(ymin = 42.5, ymax = 45)
)

# bathy data -------

#for more detailed contours use GEBCO

r <- terra::rast(paste(shapefiles,"Bathymetry/GEBCO_bathy/gebco_2020.tif", sep = ""))


#need to downsample bc too big
bathy = terra::aggregate(r, fact = 2)

#now crop to extent of study area
bathy_crop = crop(bathy, Bound_boxv)

bathy_crop <- as.data.frame(bathy_crop, xy = T)%>%dplyr::rename(Depth = gebco_2020)%>%
  mutate(Depth = ifelse(Depth >=-10, NA, Depth))

# plot(bathy_crop)

# north america for reference------

land <- read_sf(here::here("~/CODE/shapefiles/coastline/worldcountries/ne_50m_admin_0_countries.shp"))%>%
  dplyr::filter(CONTINENT == "North America")

#sable Island
sable <- read_sf(paste0(shapefiles,"coastline/Sable/Sable Island Low Water Mark 1998.shp"))


#map --------
m1 = ggplot() +
  theme_bw()+
  
  # add bathy--
  geom_raster(data = bathy_crop, aes(x= x, y=y, fill = Depth)) +
  
  scale_fill_gradient2(high = "#b3cde0", 
                       mid = "#a2c0d0", 
                       low = "#011f4b", 
                       midpoint = -100)+
  
  # add land region
  geom_sf(  data = land, color=NA, fill="grey50") +
  geom_sf(  data = sable, color=NA, fill="grey50") +
  
  #add conservation zones---
  geom_sf(data = Gully, col = "#a6f4dc", fill = NA, alpha = .5, linewidth = .5)+
  geom_sf(data = NBW_CH, col = "#a6f4dc", fill = NA, alpha = .5, linewidth = .5)+
  
  
  #add ROV annotated surveys
  geom_sf(data = st_jitter(ROV_NAV_sf, factor = .01), col = "blue", shape = 17, fill = "blue",
          alpha = .5,
          size = 4) + geom_sf(data = st_jitter(debris_2022, factor = .01), col = "orange", shape = 17, fill = "orange",
          alpha = .7,
          size = 3) +
  
  geom_sf(data = st_jitter(gearNoted_sf, factor = .01), col = "red", shape = 17, fill = "red",
          alpha = .7,
          size = 3) +

  
  # format axes
  ylab("") + 
  xlab("") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.85,text_col = "white", 
    bar_cols = c("grey40", "white"))+
  # set map limits
  coord_sf(lims_method = "orthogonal",
           xlim=lims$x, ylim=lims$y, expand = F)+ guides(fill = "none")
m1

ggsave("output/mapROV.png", m1, dpi = 300)

# ###ADD IN 2007 survey debris notes
