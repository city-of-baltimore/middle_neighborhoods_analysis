library(tidyverse)
library(RSocrata)
library(maptools)
library(leaflet)
library(htmltools)

server <- function(input, output, session) {
  
  sr.endpoint <- "https://data.baltimorecity.gov/resource/ni4d-8w7k.json"
  sr.query <- "?srtype=HCD-Vacant Building"
  sr <- read.socrata(paste0(sr.endpoint, sr.query)) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           createddate = as.Date(createddate)) 
  
  vbn.endpoint <- "https://data.baltimorecity.gov/resource/rw5h-nvv4.json"
  vbn <- read.socrata(vbn.endpoint) %>%
    mutate(longitude = map_dbl(.$location.coordinates, 1),
           latitude = map_dbl(.$location.coordinates, 2),
           noticedate = as.Date(noticedate))
  
  sr.geo <- sr %>% filter(!is.na(longitude),
                          !is.na(latitude))
  
  sr.geo <- SpatialPointsDataFrame(
    coords = sr.geo %>% 
      select(longitude, latitude) %>% 
      as.matrix(),
    data = sr.geo,
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  vbn.geo <- vbn %>% filter(!is.na(longitude),
                            !is.na(latitude))
  
  vbn.geo <- SpatialPointsDataFrame(
    coords = vbn.geo %>% 
      select(longitude, latitude) %>% 
      as.matrix(),
    data = vbn.geo,
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -76.6, lat = 39.3, zoom = 11) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addCircleMarkers(data = sr.geo, ~longitude, ~latitude, 
                       radius = 1, 
                       opacity = 0.5,
                       color = "red",
                       label = mapply(function(x, y) {
                         HTML(sprintf("Vacant Service Request Created: %s <p> %s",
                                      htmlEscape(x), htmlEscape(y)))},
                         sr.geo$createddate, sr.geo$address, SIMPLIFY = F)) %>% 
      addCircleMarkers(data = vbn.geo, ~longitude, ~latitude, 
                       radius = 1,
                       opacity = 0.5,
                       label = mapply(function(x, y) {
                         HTML(sprintf("VBN Issued: %s <p> %s",
                                      htmlEscape(x), htmlEscape(y)))},
                         vbn.geo$noticedate, 
                         vbn.geo$buildingaddress,
                         SIMPLIFY = F))
  })
  
}