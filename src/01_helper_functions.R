
get_egis_table <- function(db, table){
  
  table.full <- paste0(db, ".", table)
  
  message(paste0(Sys.time(), ": ", "Retrieving table ", table.full)) 
  
  db <- paste0('MSSQL:server=', VARS$EGIS_SERVER, ';',
               'uid=', VARS$EGIS_SERVER_USER,';',
               'pwd=', VARS$EGIS_SERVER_PWD, ';',
               'database=', db, ';',
               'trusted_connection=No')
  
  raw <- readOGR(db, table.full, verbose = F)
  
  message(paste0(Sys.time(), "Table ", table.full, " retrieved")) 
  return(raw)
}

get_hmt_data <- function(){
  
  hmt.raw <- get_egis_table("housing", "HMT2017")
  
  
  
}


load_block_group_data <- function(load.cache = T){
  
  library(here)
  if(load.cache == T){
    
    dir <- paste0(here(), "/data/raw/hmt")
    filename <- paste0(dir, "/hmt_join_acs.rds")
    
    # load existing cache or create it if it doesn't exist
    hmt <- tryCatch({
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Attempting to load cached data."))
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "File at: ", filename)) 
        cached.data <- readRDS(filename)
      },
      
      error = function(cond){
        
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache does not exist. Creating cache."))
        server.data <- get_block_group_data_from_server()
        
        if(file.exists(dir) == F){
          message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Directory does not exist. Creating directory.")) 
          dir.create(dir)
        }
    
        saveRDS(server.data, file = filename)
        message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "Cache saved to ", filename)) 
        return(server.data)
      }
    )
  } else {
    # load data directly from server 
    message(paste0(Sys.time(), ": ", "Loading directly from server."))
    server.data <- get_block_group_data_from_server()
  }
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", "HMT data succesfully loaded."))
  return(hmt)
}


get_block_group_data_from_server <- function(){
  
  suppressPackageStartupMessages(library(rgdal))
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(sp))
  
  # Census block groups: Housing Market Typology plus Census block group data
  
  housing.db <-   paste0('MSSQL:server=', VARS$EGIS_SERVER, ';',
                         'uid=', VARS$EGIS_SERVER_USER,';',
                         'pwd=', VARS$EGIS_SERVER_PWD, ';',
                         'database=housing;',
                         'trusted_connection=No')
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Grabbing HMT geospatial table from EGIS server."))
  hmt <- readOGR(housing.db, "housing.HMT2017", verbose = F)
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Formatting HMT geospatial table."))
  hmt <- SpatialPolygonsDataFrame(hmt, hmt@data)
  hmt@proj4string <- CRS("+init=epsg:2248")
  hmt <- spTransform(hmt, CRS("+init=epsg:4326"))
  
  
  hmt@data <- hmt@data %>% mutate(
    
    # HMT again, but with middle broken in two
    hmt.tier = case_when(
      MVA17HrdCd %in% c("A", "B", "C") ~ "healthy",
      MVA17HrdCd %in% c("D", "E") ~ "upper middle",
      MVA17HrdCd %in% c("F", "G", "H") ~ "lower middle",
      MVA17HrdCd %in% c("I", "J") ~ "distressed",
      TRUE ~ "other")
    
  )
  
  # census block group data (spreadsheets provided by M. Galdi)

  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Loading ACS block group data from Excel sheets."))
  acs.2016 <- read_excel(paste0(VARS$RAW_DATA, "/acs/Shortened Blockgroup, More Info.xlsx"), 
                         sheet = 2, range = "A1:CP654")
  acs.2011 <- read_excel(paste0(VARS$RAW_DATA, "/acs/Shortened Blockgroup, More Info.xlsx"), 
                         sheet = 3, range = "A1:CP654")
  
  planning.db <- paste0('MSSQL:server=', VARS$EGIS_SERVER, ';',
                        'uid=', VARS$EGIS_SERVER_USER, ';',
                        'pwd=', VARS$EGIS_SERVER_PWD, ';',
                        'database=planning;',
                        'trusted_connection=No')
  
  # @Justin replace spreadsheets with table on EGIS server (planning.ACS2016_BLOCKGROUP)
  
  #message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Loading ACS block group data from EGIS server."))
  #acs <- readOGR(planning.db, "planning.ACS2016_BLOCKGROUP")
  
  # join census bg data to hmt layer
  colnames(acs.2016) <- paste0(colnames(acs.2016), ".2016")
  colnames(acs.2011) <- paste0(colnames(acs.2011), ".2011")
  
  acs.2016$AreaYear.2016 <- as.factor(acs.2016$AreaYear.2016)
  acs.2011$AreaYear.2011 <- as.factor(acs.2011$AreaYear.2011)
  hmt$bg <- as.factor(hmt$bg)
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Joining ACS data to HMT geospatial table."))
  hmt@data <- hmt@data %>% 
    left_join(acs.2016, by = c("bg"="AreaYear.2016")) %>%
    left_join(acs.2011, by = c("bg"="AreaYear.2011"))

  return(hmt)
}


get_neighborhood_boundaries <- function(){
  suppressPackageStartupMessages(library(geojsonsf))
  suppressPackageStartupMessages(library(sf))
  suppressPackageStartupMessages(library(rgdal))
  
  
  # load Baltimore neighborhood boundaries
  # neighborhood boundaries
  hoods.url <- "https://data.baltimorecity.gov/resource/h3fx-54q3.geojson"
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ", 
                 "Getting neighborhood boundaries from Open Baltimore @ \n", hoods.url)) 
  
  
  # surely there is a better way, and i tried, but couldn't get anything else to work.
  # from geojson to sf to spatial df. geojson_sp didn't work for me.
  hoods <- geojson_sf(hoods.url)
  hoods <- as_Spatial(hoods)
  hoods <- spTransform(hoods, CRS("+init=epsg:4326"))
  
  return(hoods)
}

get_911cfs_type <- function(cfs.type){
  
  
  
}


unlist_lat_long <- function(df, colname){
  
  df$long <- sapply(
    df[, colname], 
    FUN = function(x){toString(x[[1]][[1]])}) %>%
    unlist() %>% 
    as.numeric()
  
  df$lat <- sapply(
    df[, colname], 
    FUN = function(x){toString(x[[2]][[1]])}) %>%
    unlist() %>% 
    as.numeric()
  
  return(df)
}
