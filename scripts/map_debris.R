#mpa map debris

# April 2024

pacman::p_load(sp, terra, dplyr, sf, viridis, ggplot2, ggrepel, stringr, 
               here, ggtext, readr,
               pals, tidyr, fuzzyjoin, 
               patchwork,mapsf,readxl,
               ggforce, readr, ggspatial, lubridate, stars, patchwork, scales, 
               RColorBrewer, grafify)

shapefiles ="~/CODE/shapefiles/"

debris_2022 = all_data%>%filter(!is.na(Lat))%>%st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#read eez shapefile----
EEZ <- read_sf(paste0(shapefiles, "EEZ/EEZ_can.shp"))

##MPAs & conservation zones compiled from layers available as shapefiles -------
#CH - No Zone 1
NBW_CH <- read_sf(paste0(shapefiles,"SAR_CH/NBW_CH/NorthernBottlenoseWhale_CH.shp"))

#read in Gully Zone 
Gully <- read_sf(paste0(shapefiles,"ProtectedAreas/DFO/Gully/Gully_MPA.shp"))%>%filter(NAME != "Zone 1")

#MAP FIGURE

# Get the vertical and horizontal limits



# Get x and y limits
lims <- list(
  x = c(xmin = -59.137359-1, xmax = -57.88472222+1),
  y = c(ymin = 43.69995-1, ymax = 44.3027777800001+1)
)
xmin <- -59.137359 -1
xmax <- -57.88472222+1
ymin <- 43.69995-1
ymax <- 44.3027777800001+1

ext <- terra::vect(c(xmin, xmax, ymin, ymax), crs="+proj=longlat")  # Specify the CRS if known, otherwise WGS84 is assumed

# bathy data -------

#for more detailed contours use GEBCO

r <- terra::rast(paste(shapefiles,"Bathymetry/GEBCO_bathy/gebco_2020.tif", sep = ""))


#need to downsample bc too big
bathy = terra::aggregate(r, fact = 2)

#now crop to extent of study area
bathy_crop = crop(bathy, ext)

bathy_crop <- as.data.frame(bathy_crop, xy = T)%>%dplyr::rename(Depth = gebco_2020)%>%
  mutate(Depth = ifelse(Depth >=-10, NA, Depth))

plot(bathy_crop)

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
  
  
  #add plastic surveys
  geom_sf(data = st_jitter(debris_2022, factor = .01), col = "orange", shape = 17, fill = "orange",
          alpha = .7,
          size = 3) +
  
  # format axes
  ylab("") + 
  xlab("") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.85,
    bar_cols = c("grey40", "white"))+
  # set map limits
  coord_sf(lims_method = "orthogonal",
           xlim=lims$x, ylim=lims$y, expand = T)
m1

###ALL IN UTM-----
  
  
  #annotate GuLLY
  annotate(geom = "text", x = 320799.125, y = 4846790.387, label = "The Gully",fontface = "bold.italic",
           color = "black", size = 4, ) +
  # add text annotation for canyons
  annotate(geom = "text", x = 429482.244, y = 4875581.286, label = "Haldimand",fontface = "bold.italic",
           color = "black", size = 4, ) +
  annotate(geom = "text", x = 395633.360, y = 4863808.839, label = "Shortland",fontface = "bold.italic",
           color = "black", size = 4, )+

  )

#plot                    
m1 = m1+# unnecessary spacing in the final plot arrangement.
  theme(plot.margin = margin(0, 0, 0, .1), plot.title = element_text(hjust=0.5), 
        plot.subtitle  = element_text(hjust=0.5), axis.title = element_blank())
