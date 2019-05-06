# vytvoření modelu, výpočet reziduí a jejich zmapování do leaflet objektu

# simple does it - linear regression
model <- lm(data = grid, barcount ~ pop + beds + stations + vegetation)

print(summary(model))

resids <- model$residuals # extract residuals from model

grid <- grid %>% # ... attach them to grid
   cbind(resids)


library(leaflet)
library(htmltools)

# diverging palette - green is good, red is bad
pal <- colorBin(palette = "RdYlGn",  domain = grid$resids,  bins = 5,  reverse = T)

grid <- grid %>% # create a HTML formatted popup label of grid cell
   mutate(label = paste0('O ', round(abs(resids)),' hospod <u>', 
                         ifelse(resids > 0, 'více', 'méně'),'</u> nežli očekáváno'))

lplot <- leaflet() %>%
   addProviderTiles(providers$CartoDB.Positron) %>%
   setView(lng = 14.46, lat = 50.07, zoom = 11) %>%
   addPolygons(data = grid, 
               fillColor = ~pal(resids),
               fillOpacity = 0.5,
               stroke = F, 
               popup = ~label) 

