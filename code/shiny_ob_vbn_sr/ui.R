library(leaflet)
library(shiny)

ui <- fluidPage(
  
  leafletOutput("vacant.map", height = "1000px"),
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",

                h2("Baltimore Vacancy"),
                h4("Vacant Building Notices (VBNs) and 311 Service Requests (SRs)"),
                p("VBNs (blue) are issued to buildings that are unoccupied and uninhabitable based on code violations. The total number of currently-open VBNs in Baltimore is:"),
                verbatimTextOutput("vbn.total"),
                p("The earliest VBN on Open Baltimore is:"),
                verbatimTextOutput("vbn.first"),
                p("The most recent VBN on Open Baltimore is:"),
                verbatimTextOutput("vbn.last"),
                p("SRs (red) can be initiated by residents through 311 for buildings they believe to be vacant/unoccupied. The total number of 311 service requests for vacant buildings is:"),
                verbatimTextOutput("sr.total"),
                p("The earliest SR for vacant on Open Baltimore is:"),
                verbatimTextOutput("sr.first"),
                p("The most recent SR for a vacant on Open Baltimore is:"),
                verbatimTextOutput("sr.last"),
                a(href="http://data.baltimorecity.gov",'Data on Open Baltimore.'),
                a(href="https://github.com/city-of-baltimore/vacants_map","Code on Github.")
                
  )
)


  