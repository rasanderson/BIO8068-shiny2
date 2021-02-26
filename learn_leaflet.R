library(leaflet)
library(mapview)

m <- leaflet() %>%
   addTiles() %>%  # Add default OpenStreetMap map tiles
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addMarkers(lng=-1.6178, lat=54.9783, popup="The world's most important city!")
m  # Print the map


# Multiple layers
leaflet() %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addProviderTiles(providers$Stamen.TonerLines,
                    options = providerTileOptions(opacity = 0.5)) %>%
   addProviderTiles(providers$Stamen.TonerLabels) %>% 
   addMarkers(lng=-1.6178, lat=54.9783, popup="The world's most important city!")

# Circle markers
leaflet() %>%
   addTiles() %>%  
   addCircleMarkers(lng=-1.6178, lat=54.9783,
                    label="The world's most important city!",
                    radius = 15, color = "red")

# Shapefile from Nafferton
library(sf)
library(mapview)
nafferton_fields <- st_read("www/naff_fields.shp")
nafferton_fields <- st_set_crs(nafferton_fields, 27700)
nafferton_fields_ll <- st_transform(nafferton_fields, 4326)

leaflet() %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addFeatures(nafferton_fields_ll)

# Separate colours for organic and conventional
nafferton_fields_ll[nafferton_fields_ll$Farm_Meth=="Organic",]
leaflet() %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addFeatures(nafferton_fields_ll[nafferton_fields_ll$Farm_Meth=="Organic",],
               fillColor="green",
               color="white",
               opacity =0.7,
               fillOpacity=1) %>% 
   addFeatures(nafferton_fields_ll[nafferton_fields_ll$Farm_Meth=="Conventional",],
               fillColor="red",
               color="yellow", 
               fillOpacity=1)

# by field area
bins <- c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000)
pal <- colorBin(palette = "Greens", domain = bins)
leaflet() %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addFeatures(nafferton_fields_ll,
               fillColor = ~pal(nafferton_fields_ll$Area_m),
               fillOpacity = 1)




pal <- colorQuantile(n = 6, palette = "Greens", domain = nafferton_fields_ll$Area_m)
leaflet() %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addFeatures(nafferton_fields_ll,
               fillColor = ~pal(nafferton_fields_ll$Area_m),
               fillOpacity = 1)




# Add legend
pal <- colorNumeric(palette = "Greens", domain = bins)
leaflet(nafferton_fields_ll) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addFeatures(nafferton_fields_ll,
              fillColor = ~pal(Area_m),
              fillOpacity = 1) %>% 
  addLegend("bottomright", pal = pal,
            values = ~Area_m,
            title = "Field area",
            labFormat = labelFormat(suffix = " m^2"),
            opacity = 1)

# Highlight
pal <- colorNumeric(palette = "Greens", domain = bins)
leaflet(nafferton_fields_ll) %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addFeatures(nafferton_fields_ll,
               fillColor = ~pal(Area_m),
               fillOpacity = 1,
               highlightOptions = highlightOptions(color = "yellow", weight = 5,
                                                   bringToFront = TRUE)) %>% 
   addLegend("bottomright", pal = pal,
             values = ~Area_m,
             title = "Field area",
             labFormat = labelFormat(suffix = " m^2"),
             opacity = 1)

# Highlight and popup
pal <- colorNumeric(palette = "Greens", domain = bins)
field_info <- paste("Method: ", nafferton_fields_ll$Farm_Meth, "<br>",
                    "Crop: ", nafferton_fields_ll$Crop_2010)
                    
leaflet(nafferton_fields_ll) %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>% 
   addFeatures(nafferton_fields_ll,
               fillColor = ~pal(Area_m),
               fillOpacity = 1,
               highlightOptions = highlightOptions(color = "yellow", weight = 5,
                                                   bringToFront = TRUE),
               popup = field_info) %>% 
   addLegend("bottomright", pal = pal,
             values = ~Area_m,
             title = "Field area",
             labFormat = labelFormat(suffix = " m^2"),
             opacity = 1)


# Using basegroups
leaflet() %>% 
   addTiles(group = "OSM (default)") %>% 
   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
   addLayersControl(
      baseGroups = c("OSM (default)", "Satellite")
   ) %>% 
   setView(lat = 54.9857, lng=-1.8990, zoom=10)

# basegroups and overlays
leaflet() %>% 
   addTiles(group = "OSM (default)") %>% 
   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
   addFeatures(nafferton_fields_ll, group = "Nafferton Farm") %>% 
   addLayersControl(
      baseGroups = c("OSM (default)", "Satellite"), 
      overlayGroups = "Nafferton Farm",
      options = layersControlOptions(collapsed = FALSE)
   )

# Organic or conventional
leaflet() %>%
   addTiles(group = "OSM (default)") %>% 
   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
   addFeatures(nafferton_fields_ll[nafferton_fields_ll$Farm_Meth=="Organic",],
               fillColor="green",
               color="white",
               opacity =0.7,
               fillOpacity=1,
               group = "Organic") %>% 
   addFeatures(nafferton_fields_ll[nafferton_fields_ll$Farm_Meth=="Conventional",],
               fillColor="red",
               color="yellow", 
               fillOpacity=1,
               group = "Conventional") %>% 
   addLayersControl(
      baseGroups = c("OSM (default)", "Satellite"), 
      overlayGroups = c("Organic", "Conventional"),
      options = layersControlOptions(collapsed = FALSE)
   )

