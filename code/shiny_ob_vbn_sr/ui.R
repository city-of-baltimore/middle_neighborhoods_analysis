library(leaflet)
library(shiny)

ui <- fluidPage(
  leafletOutput("vacant.map",
                width = "100%",
                height = "100%")
)
