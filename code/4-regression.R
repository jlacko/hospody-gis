# vytvoření modelu, výpočet reziduí a jejich zmapování do leaflet objektu

# simple does it - linear regression ----

# kombinace vegetace a dat za administrative areas
grid$pop_times_veg <- grid$pop * grid$vegetation # efekt vegetace na populaci
grid$bed_times_veg <- grid$beds * grid$vegetation # efekt vegetace na hotelová lůžka

# baseline - jediná nezávislá veličina (populace)
bl_model <- lm(data = grid, barcount ~ pop)

print(summary(bl_model))

# jednoduchý model - čtyři nezávislé veličiny
model <- lm(data = grid, barcount ~ pop + beds + stations + vegetation)

print(summary(model))

# složitější model - čtyři nezávislé veličiny + dvě interakce
sl_model <- lm(data = grid, barcount ~ pop + beds + stations + vegetation + 
                  pop_times_veg + bed_times_veg)

print(summary(sl_model))

# poissonův model

poi_model <- glm(data = grid, barcount ~ pop + beds + stations + vegetation, 
                 family = "poisson")

summary(poi_model)


# čtyři varianty reziduálů ----

four_residuals <- data.frame(id = grid$id,
                             baseline = bl_model$residuals,
                             simple = model$residuals,
                             interactive = sl_model$residuals,
                             poisson = poi_model$residuals)

plot_resids <- ggplot(data = four_residuals) +
   geom_freqpoly(aes(baseline, color = "gray75"), binwidth = 1) +
   geom_freqpoly(aes(simple, color = "red"), binwidth = 1) +
   geom_freqpoly(aes(interactive, color = "goldenrod2"), binwidth = 1) +
   geom_freqpoly(aes(poisson, color = "cornflowerblue"), binwidth = 1) +
   scale_color_identity(breaks = c("gray75", # barvy
                                   "red", 
                                   "goldenrod2",
                                   "cornflowerblue"), 
                        labels = c("baseline", # popisky legendy
                                   "jednoduchý", 
                                   "složitější",
                                   "Poissonův"), 
                        guide = "legend",
                        name = "Modely:") + # vynucení zobrazení
   theme_bw() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.text.align = 1)

   

# příprava dat pro graf -----

resids <- sl_model$residuals # extract residuals from model
predikce <- sl_model$fitted.values # předpovědi z modelu

grid <- grid %>% # ... attach them to grid
   cbind(resids) %>% 
   cbind(predikce)

# podat o všem zprávu (leafletem :) ----
library(leaflet)
library(htmltools)

# diverging palette - green is good, red is bad
pal <- colorBin(palette = "RdYlGn",  domain = grid$resids,  bins = 5,  reverse = T)

grid <- grid %>% # create a HTML formatted popup label of grid cell
   mutate(label = paste0('Predikce ', round(predikce),' hospod, <br>',
                         'skutečnost ', barcount, ', ', 'rozdíl <b>',
                         ifelse(resids > 0, '+', '-'), abs(round(resids)), '</b>.'))

lplot <- leaflet() %>%
   addProviderTiles(providers$CartoDB.Positron) %>%
   setView(lng = 14.46, lat = 50.07, zoom = 10) %>%
   addPolygons(data = grid, 
               fillColor = ~pal(resids),
               fillOpacity = 0.5,
               stroke = F, 
               popup = ~label) 



