# přehled barů + naplnění gridu o bary


# bary jako body ----
plot_bary_body <- ggplot() +
   geom_point(data = bars, aes(x = lon, y = lat), pch = 4, alpha = 0.7, col = "red") +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank())

# bary jako density plot ----
plot_bary_dens <- ggplot() +
   stat_density_2d(data = bars, aes(x = lon, y = lat, fill = ..level..), 
                   geom = "polygon", alpha = .15, color = NA) +
   scale_fill_gradient2("Bars", low = "white", mid = "yellow", high = "red") +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1)


# bary jako počty přes mřížku ----
bars <- bars %>% # convert to a sf object
   st_as_sf(coords = c("lon", "lat"), 
            crs = 4326, agr = "constant")

pruseciky <- st_join(bars, grid) %>%
   st_set_geometry(NULL) %>%
   group_by(id) %>%
   tally() %>%
   dplyr::select(id, barcount = n)

grid <- left_join(grid, pruseciky, by = 'id') %>%
   mutate(barcount = ifelse(is.na(barcount), 0, barcount)) # replace NAs with zero

plot_bary_grid <- ggplot() +
   geom_sf(data = grid, aes(fill = barcount), color = 'gray66', alpha = 0.6) +
   scale_fill_gradient2(midpoint = 20,
                        low = 'green2',
                        mid = 'yellow',
                        high = 'red3',
                        na.value = 'white',
                        name = 'bar count') +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1)