# načtení dat ze staťáků a naplnění gridu o data za městské části

czso <- read_csv("./data/CZSO_data.csv") # read the bar data in

prazske_casti <- RCzechia::casti() %>% # shapefile all borroughs of Czech cities
   filter(NAZ_OBEC == 'Praha') %>% # Prague ones
   mutate(cast = as.numeric(KOD)) %>% # keys in RCzechia are character
   inner_join(czso, by = 'cast')

# transform to a metric CRS (for area measurement)
prazske_casti <- st_transform(prazske_casti, 5514)
grid <- st_transform(grid, 5514)

prazske_casti$pr_area <- st_area(prazske_casti) # area of the borrough

# create a temp structure for calculating the population & beds per grid cell
asdf <- prazske_casti %>%
   # intersection of borrough and grid cell - there will be partial intersections
   # (there will be more rows of intersections than of grid cells)
   st_intersection(grid) %>% 
   dplyr::select(id, pr_area, pop, beds)

asdf$in_area = drop_units(st_area(asdf)) # plocha průsečíku polygonu s částí

asdf <- asdf %>%
   st_set_geometry(NULL) %>%
   mutate(pop = pop * in_area / pr_area,  # share of metric as proportion of area
          beds = replace_na(beds * in_area / pr_area, 0)) %>%
   group_by(id) %>% # group by cell id
   summarise(pop = drop_units(sum(pop)), # totals per grid cell
             beds = drop_units(sum(beds))) 

grid <- left_join(grid, 
                  asdf, by = 'id') # attach the temp structure

grid <- st_transform(grid, 4326) # back to WGS84


plot_beds_grid <- ggplot() + # plot hotel beds
   geom_sf(data = grid, aes(fill = beds), color = 'gray50', alpha = 0.6) +
   scale_fill_gradient2(low = 'green2',
                        mid = 'yellow',
                        high = 'red3',
                        na.value = 'white',
                        name = 'hotel beds',
                        labels = comma) +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1)

plot_pop_grid <- ggplot() + # plot population
   geom_sf(data = grid, aes(fill = pop), color = 'gray50', alpha = 0.6) +
   scale_fill_gradient2(low = 'green2',
                        mid = 'yellow',
                        high = 'red3',
                        na.value = 'white',
                        name = 'population',
                        labels = comma) +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1)