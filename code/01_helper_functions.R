


get_block_group_data <- function(){
  
  library(rgdal)
  library(readxl)
  library(tidyverse)
  
  # Census block groups: Housing Market Typology plus Census block group data
  
  housing.db <-   paste0('MSSQL:server=', VARS$EGIS_SERVER, ';',
                         'uid=', VARS$EGIS_SERVER_USER,';',
                         'pwd=', VARS$EGIS_SERVER_PWD, ';',
                         'database=housing;',
                         'trusted_connection=No')
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Grabbing HMT geospatial table from EGIS server."))
  hmt <- readOGR(housing.db, "housing.HMT2017")
  
  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": Formatting HMT geospatial table."))
  hmt <- SpatialPolygonsDataFrame(hmt, hmt@data)
  hmt@proj4string <- CRS("+init=epsg:2248")
  hmt <- spTransform(hmt, CRS("+init=epsg:4326"))
  
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
  # load Baltimore neighborhood boundaries
  
}

