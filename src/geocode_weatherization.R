
library(ggmap)
library(readxl)
library(xlsx)
source("src/00_initialize.R")

# google geocoding api key
register_google(VARS$GOOGLE_GEO_KEY)

# nhood boundaries from Open Baltimore
hoods <- get_neighborhood_boundaries()

# list of neighborhoods with dominant HMT type
hmt.by.hood <- read_excel("data/raw/hmt/HMT by Neighborhood 2017.xlsx") %>%
  select(Neighborhood, `Predominant Code Ignoring Non-Residential`)


dir <- "data/raw/weatherization"
file.list <- list.files(dir)
df <- data.frame()

for (file in file.list){
  
  filename <- paste0(dir, "/", file)
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "loading ", filename))
  
  temp <- read_excel(filename, skip = 15, col_types = "text")
  
  df <- df %>% bind_rows(temp)
  
}

df <- df %>% filter(!is.na(`Last Name`), Agency != "Agency")

 # geocode

df <- df %>%
  mutate(full.address = paste0(Street, ", ", City, ", MD ", Zip))  %>%
  top_n(2500)
  mutate_geocode(full.address)
