# načtení rastru a doplnění gridu o modifikovaný index vegetace

red <- raster("./data/red_prg-2018-06-30.tif") # červený kanál
nir <- raster("./data/nir_prg-2018-06-30.tif") # near infrared kanál

ndvi <- (nir - red) / (nir + red) # standardní index

ndvim <- abs(ndvi) # modifikovaný index - bez záporných hodnot

plot_ndvi_raster <- function() plot(ndvim, axes = F, box = F) 

grid$vegetation <- 0 # inicializace nulou


for (i in grid$id) {
   
   bunka <- grid$geometry[i] %>%  # i-tá buňka v gridu
      st_transform(crs = ndvim@crs@projargs) %>% # transformovaná do crs rasteru
      as("Spatial") # jako sp objekt = fuuuj! :)
   
   grid$vegetation[i] <- crop(ndvim, bunka) %>% # index v buňce ...
      cellStats(stat = "mean") # ... zprůměrovaný
}

grid$vegetation[is.nan(grid$vegetation)] <- 0 # sanitizovat nan

# podat zprávu - obrázek ve mřížce
plot_vege_grid <- ggplot() + # plot vegetation index
   geom_sf(data = grid, aes(fill = vegetation), color = 'gray50', alpha = 0.7) +
   scale_fill_gradient2(low = 'yellow',
                        high = 'green2',
                        midpoint = 0.4,
                        name = 'vegetation index') +
   geom_sf(data = vltava, color = 'slategray3', lwd = 1.25) +
   geom_sf(data = obrys, fill = NA, color = 'gray75', lwd = 1, alpha = 0.6) +
   theme_bw() +
   theme(axis.title.y = element_blank(), 
         axis.title.x = element_blank(),
         legend.text.align = 1)