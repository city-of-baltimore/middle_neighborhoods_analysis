library(leaflet)
library(shiny)

ui <- fluidPage(
  leafletOutput("vacant.map")
)
