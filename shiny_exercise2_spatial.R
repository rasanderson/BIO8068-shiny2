library(leaflet)
library(leafem)
library(mapview)

# Create the map
my_map <- leaflet() %>%
   addTiles() %>%  # Add default OpenStreetMap map tiles
   addMarkers(lng=-1.6178, lat=54.9783, popup="World's most important city!")

my_map  # Display the map in Viewer window


leaflet() %>%
   addProviderTiles(providers$Esri.WorldImagery) %>%
   addProviderTiles(providers$Stamen.TonerLines,
                    options = providerTileOptions(opacity = 0.5)) %>%
   addProviderTiles(providers$Stamen.TonerLabels) %>%
   addMarkers(lng=-1.6178, lat=54.9783, popup="World's most important city!")


   # addProviderTiles(providers$Stamen.TonerLines,
   #                  options = providerTileOptions(opacity = 0.5)) %>%


leaflet() %>%
   addTiles() %>%
   addCircleMarkers(lng=-1.6178, lat=54.9783,
                    popup="The world's most important city!",
                    radius = 5, color = "red")

leaflet() %>%
   addTiles() %>%
   addCircleMarkers(lng=-1.6178, lat=54.9783,
                    label="Newcastle population 270k",
                    labelOptions = labelOptions(noHide = T, textsize = "150px"))


## -----------------------------------------------------------------------------------------
library(sf)
nafferton_fields <- st_read("www/naff_fields.shp")


## ---- eval=FALSE--------------------------------------------------------------------------
## st_crs(nafferton_fields)


## -----------------------------------------------------------------------------------------
# First reset nafferton fields to OS 27700. Depending on the source of your data
# you may not always need this first step
nafferton_fields <- nafferton_fields %>% 
   st_set_crs(27700) %>% 
   st_transform(27700)

# Transform to latitude longitude
nafferton_fields_ll <- st_transform(nafferton_fields, 4326) # Lat-Lon


## ---- echo=FALSE--------------------------------------------------------------------------
plot(nafferton_fields)


## ---- eval=FALSE--------------------------------------------------------------------------
## leaflet() %>%
##    addProviderTiles(providers$Esri.WorldImagery) %>%
##    addFeatures(nafferton_fields_ll)


## ---- eval=FALSE--------------------------------------------------------------------------
## # Set the bins to divide up your areas
## bins <- c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000)
## 
## # Decide on the colour palatte
## pal <- colorBin(palette = "Greens", domain = bins)
## 
## # Create the map
## leaflet() %>%
##    addProviderTiles(providers$Esri.WorldImagery) %>%
##    addFeatures(nafferton_fields_ll,
##                fillColor = ~pal(nafferton_fields_ll$Area_m),
##                fillOpacity = 1)


## ---- eval=FALSE--------------------------------------------------------------------------
## pal <- colorNumeric(palette = "Greens", domain = bins)
## 
## # Now leaflet is called with nafferton_fields_ll
## leaflet(nafferton_fields_ll) %>%
##    addProviderTiles(providers$Esri.WorldImagery) %>%
##    addFeatures(fillColor = ~pal(Area_m),
##                fillOpacity = 1) %>%
##    addLegend("bottomright",
##              pal = pal,
##              values = ~Area_m,
##              title = "Field area",
##              labFormat = labelFormat(suffix = " m^2"),
##              opacity = 1)


## ---- eval=FALSE--------------------------------------------------------------------------
## highlightOptions = highlightOptions(color = "yellow",
##                                     weight = 5,
##                                     bringToFront = TRUE)


## ---- eval=FALSE--------------------------------------------------------------------------
## field_info <- paste("Method: ", nafferton_fields_ll$Farm_Meth,
##                     "<br>",
##                     "Crop: ", nafferton_fields_ll$Crop_2010)


## ---- eval=FALSE--------------------------------------------------------------------------
## leaflet() %>%
##    addTiles(group = "OSM (default)") %>%
##    addProviderTiles(providers$Esri.WorldImagery,
##                     group = "Satellite") %>%
##    addLayersControl(
##       baseGroups = c("OSM (default)", "Satellite")
##    )


## ---- eval=FALSE--------------------------------------------------------------------------
## leaflet() %>%
##    addTiles(group = "OSM (default)") %>%
##    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
##    addFeatures(nafferton_fields_ll, group = "Nafferton Farm") %>%
##    addLayersControl(
##       baseGroups = c("OSM (default)", "Satellite"),
##       overlayGroups = "Nafferton Farm",
##       options = layersControlOptions(collapsed = FALSE)
##    )


## ---- eval=FALSE--------------------------------------------------------------------------
## leafletOutput(outputId = "nafferton_map")


## ---- eval=FALSE--------------------------------------------------------------------------
##    output$nafferton_map <- renderLeaflet({
## 
##     })


## ---- eval=FALSE--------------------------------------------------------------------------
## observeEvent(input$nafferton_map_click, {
##       click<-input$nafferton_map_click
##       text<-paste("Lattitude ", click$lat, "Longtitude ",
##                   click$lng)
##       print(text)
##    })

