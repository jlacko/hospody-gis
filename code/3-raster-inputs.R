# načtení rastru a doplnění gridu o modifikovaný index vegetace

red <- raster("./data/red_prg-2018-06-30.tif") # červený kanál
nir <- raster("./data/nir_prg-2018-06-30.tif") # near infrared kanál

ndvi <- (nir - red) / (nir + red) # standardní index

ndvim <- max(ndvi, 0) # modifikovaný index - bez záporných hodnot

mod_ndvim <- ndvim %>%  
   as("SpatialPixelsDataFrame") %>% 
   as_data_frame()

plot_vege_raster <- ggplot() +
   geom_raster(data = mod_ndvim, aes(x=x, y=y, fill=layer), alpha=1) +
   scale_fill_gradientn(colors = rev(terrain.colors(7)),
                        limits = c(0, 1), 
                        name = "NDVI (abs.)") +
   coord_equal() +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1) +
   coord_sf(datum = NULL)


grid$vegetation <- 0 # inicializace nulou


for (i in grid$id) { # iterace přes řádky gridu
   
   bunka <- grid$geometry[i] %>%  # i-tá buňka v gridu
      st_transform(crs = ndvim@crs@projargs) %>% # transformovaná do crs rasteru
      as("Spatial") # jako sp objekt = fuuuj! :)
   
   grid$vegetation[i] <- crop(ndvim, bunka) %>% # index uvnitř buňky gridu ...
      cellStats(stat = "mean") %>%  # ... zprůměrovaný,
      na.omit() # ... a sanitizované NaN
}

# podat zprávu - obrázek ve mřížce
plot_vege_grid <- ggplot() + # plot vegetation index
   geom_sf(data = grid, aes(fill = vegetation), color = 'gray66', alpha = 1) +
   scale_fill_gradientn(colors = rev(terrain.colors(7)),
                        limits = c(0, 1),
                        name = 'NDVI (avg.)') +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1)
