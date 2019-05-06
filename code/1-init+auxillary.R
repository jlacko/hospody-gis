# globální init = načtení dat, příprava prázného gridu a pomocných mapových objektů

library(tidyverse) # mainly ggplot & dplyr
library(RCzechia)  # spatial objects of the Czech Republic
library(raster)    # raster files handling
library(scales)    # aligned with tidyverse, but not a part of the core
library(units)     # for handling area units (square kilometers)
library(sf)        # spatial vector data operations package


# načíst data ----
bars <- read_csv("./data/prague_bars.csv") # read the bar data in


# vytvořit pomocné kreslicí objekty - hranice Prahy & kus Vltavy ----

obrys <- RCzechia::kraje("high") %>% # all the Czech NUTS3 entities ...
   filter(KOD_CZNUTS3 == 'CZ010') %>% #  ... just Prague
   pull(geometry) %>% # only the boundaries (no data)
   st_transform(5514) # to a metric CRS (for the buffer to work)

bbox <- obrys %>%
   st_buffer(1000) # 1 kilometer buffer

obrys <- st_transform(obrys, 4326) # back to WGS84

vltava <- RCzechia::reky() %>% # all the Czech rivers ...
   filter(NAZEV == 'Vltava') %>% # ... the one relevant
   st_transform(5514) %>% # to a metric CRS
   st_intersection(bbox) %>% # clip to the box around Prague
   st_transform(4326) # back to WGS84

# mřížka ----

plocha <- 5e6 # tj. x kilometrů čterečných

# size of grid cells, in units of the CRS (i.e. meters for 5514)
# grid_spacing <- sqrt(plocha) # tj. plocha čtverce = cíl
grid_spacing <- sqrt(2*plocha/(3*sqrt(3))) # tj. plocha šestiúhelníku = cíl

obrys <- st_transform(obrys, 5514) # temporarily transform to a metric CRS

krabicka <- st_bbox(obrys)

xrange <- krabicka$xmax - krabicka$xmin 
yrange <- krabicka$ymax - krabicka$ymin

# count the polygons necessary & round up to the nearest integer
rozmery <- c(xrange/grid_spacing , yrange/grid_spacing) %>% 
   ceiling() 

grid <- st_make_grid(obrys, square = F, n = rozmery) %>% # make the grid
   st_intersection(obrys) %>% # clop the inside part, as a sfc object
   st_transform(4326) %>% # convert to WGS84
   st_sf() %>% # make the sfc a sf object, capable of holding data
   mutate(id = row_number()) # create id of the grid cell

obrys <- st_transform(obrys, 4326) # back to WGS84

plot_grid_empty <- ggplot() + 
  geom_sf(data = grid, fill = NA, color = 'gray66', alpha = 0.6) +
  geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
  geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank())

