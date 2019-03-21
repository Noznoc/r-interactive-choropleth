# SET WORK DIRECTORY
setwd("")

# RUN LIBRARIES
library(tidyverse)
library(rgdal)
library(classInt)
library(rgeos)
library(leaflet)
library(rjson)

# READ ATTRIBUTE DATA
geo_attr <- read_csv("[enter path]")

# READ GEOGRAPHIC BOUNDARY SPATIAL DATA (CMA/CA)
geo_boundary <- rgdal::readOGR("[enter path where data is located]", "[enter file name]", encoding = "UTF-8")

# JOIN ATTRIBUTE DATA TO THE GEOGRAPHIC BOUNDARY DATA
geo_attr$[enter column] <- as.character(geo_attr$[enter column]) # convert data type from factor to character to match data type in 
geo_boundary@data <- left_join(geo_boundary@data, geo_attr, by = c("[enter column]" = "[enter column]"))
geo_boundary_attr <- #specify column of interested from geo_boundary that you want visualized in choropleth

# CREATE MAP CLASSIFICATION (NATURAL JENKS) & COLOUR PALETTE THROUGH createClasses FUNCTION
# function creates how the values are binned. Parameters: data = data to be visualized, 
# palette = colour palette (refer to ColorBrewer for more options: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3),
# na_color = the colour for NA values,
# n = number of bins (9 is the most for the given ColorBrewer palettes)
# NOTE: binning is done with Natural Jenks (style = "jenks"), this can be changed by adding a new value to the style parameter
createClasses <- function(data, palette, na_color, n) {
  classes <- classIntervals(na.exclude(data), n = n, style = "jenks")
  bins <- classes[["brks"]]
  pal <- colorBin(palette, domain = data, bins = bins, na.color = na_color)
  return <- list("pal" = pal, "bins" = bins)
} # function that assigns the values to a bin and colour for the visualization
pal <- createClasses(geo_boundary_attr, "YlGnBu", "transparent", 9) # run the function and store it as an object

# CREATE MAP LABELS & POPUPS
geo_boundary_labels <- sprintf(
  "<strong>%s</strong>",
  geo_boundary_attr) %>% 
  lapply(htmltools::HTML) # add labels

geo_boundary_popups <- sprintf(
  "<strong>%s</strong>",
  geo_boundary_attr) %>% 
  lapply(htmltools::HTML) # add popups

# TRANSFORM TO LONGLAT UNPROJECTED CRS FOR VISUALIZATION
geo_boundary <- spTransform(geo_boundary, CRS("+proj=longlat +datum=WGS84"))

# CREATE CENTROID FROM CMA/CA (CENTER POINT OF EACH CMA/CA GEOGRAPHIC BOUNDARY)
geo_centroids <- SpatialPointsDataFrame(gCentroid(geo_boundary, byid = TRUE), geo_boundary@data, match.ID = FALSE)

# MAP
map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(minZoom = 3, maxZoom = 9)) %>% 
  setView(-92.8726184, 55.9988861, 4) %>%
  addEasyButton(easyButton(icon="fa-globe", title="Zoom to Canada View", onClick=JS("function(btn, map){ map.setZoom(4); }"))) %>% 
  addPolygons(data = geo_boundary, stroke = TRUE, color = "#333", weight = 1, smoothFactor = 0.5,
              opacity = 0.2, fillOpacity = 0.9, fillColor = ~pal$pal(geo_boundary_attr), group = "Polygons",
              highlight = highlightOptions(weight = 2, color = "#333", fillOpacity = 0.1, bringToFront = TRUE),
              label = geo_boundary_labels, 
              labelOptions = labelOptions(style = list(padding = "3px 8px"), textsize = "13px", direction = "auto"),
              popup = geo_boundary_popups) %>% 
  addCircleMarkers(data = geo_centroids, group = "Centroids", 
                   opacity = 1, weight = 0.5, fillOpacity = 1, radius = 4, 
                   fillColor = ~pal$pal(geo_boundary_attr), color = "#333",
                   popup = geo_boundary_popups, label = geo_boundary_labels) %>% 
  addLegend("topright", pal = pal$pal, values = geo_boundary_attr, opacity = 1, title = "Enter Title", na.label = "No Data") %>%
  addLayersControl(overlayGroups = c("Centroids", "Polygons"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
map