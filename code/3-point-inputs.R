# načtení bodů a naplnění gridu o stanice metra

metro <- st_read("./data/metro_stations.geojson", quiet = T) # read the metro stations

# count metro stations per grid cell & attach
pruseciky <- st_join(metro, grid) %>%
   st_set_geometry(NULL) %>%
   group_by(id) %>%
   tally() %>%
   dplyr::select(id, stations = n)

grid <- left_join(grid, pruseciky, by = 'id') # NAčka zůstávají jako NAčka

plot_metro_grid <- ggplot() + # plot metro stations
   geom_sf(data = grid, aes(fill = stations), color = 'gray50', alpha = 0.6) +
   scale_fill_gradient2(low = 'green2',
                        mid = 'yellow',
                        high = 'red3',
                        na.value = 'white',
                        name = 'metro stations',
                        breaks = c(1, 2, 3)) +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1)

grid <- grid %>% # NAčka přerazit na nuly
   mutate(stations = ifelse(is.na(stations), 0, stations))