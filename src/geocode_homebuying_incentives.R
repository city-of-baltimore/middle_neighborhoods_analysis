

library(ggmap)
library(readxl)
source("src/00_initialize.R")

# google geocoding api key
register_google(VARS$GOOGLE_GEO_KEY)

# nhood boundaries from Open Baltimore
hoods <- get_neighborhood_boundaries()

# list of neighborhoods with dominant HMT type
hmt.by.hood <- read_excel("data/raw/hmt/HMT by Neighborhood 2017.xlsx") %>%
  select(Neighborhood, `Predominant Code Ignoring Non-Residential`)

dir <- "data/raw/homebuying_incentives"
file.list <- list.files(dir)
df <- data.frame()

for (file in file.list){
  
  filename <- paste0(dir, "/", file)
  temp <- read_excel(filename, skip = 2)
  df <- df %>% bind_rows(temp)
  
}

# geocode
df <- df %>%
  mutate(full.address = paste0(`House Num`, " ", 
                               Street, " ", `Street Type`, 
                               ", Baltimore, MD ", Zip)) %>%
  mutate_geocode(full.address)

# convert to sp 
df.geo <- df
coordinates(df.geo) <- c("lon", "lat")

proj4string(df.geo) <- CRS( "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# use over to label with lat longs and right neighborhoods 
df.geo@data <- bind_cols(df.geo@data, over(df.geo, hoods))

# restore lat and long to data frame for saving
df.geo$longitude <- df.geo@coords[,1]
df.geo$latitude <- df.geo@coords[,2]

# rename n'hood original
df.geo@data <- df.geo@data %>%
  rename(Neighborhood.original = Neighborhood)

# join HMT
df.geo@data <- left_join(df.geo@data, 
                           hmt.by.hood, 
                           by = c("label" = "Neighborhood"))

df.geo@data <- rename(df.geo@data, predominant.code = `Predominant Code Ignoring Non-Residential`)

# apply HMT grouping
df.geo@data <- df.geo@data %>% mutate_hmt_group(predominant.code)

colnames(df.geo@data)

df.geo@data <- df.geo@data %>% rename_all(
  funs(
    str_to_lower(.) %>% 
      str_replace_all(., " ", ".") %>%
      str_replace_all(., "\n", ".") %>%
      str_replace_all(., "\r", ""))
) 

# df.geo@data %>% rename(
#   id.hos = `ID HOS`,
#   last.name = `Last Name`,
#   first.name = `First Name`,
#   gender = Gender,
#   race = Race,
#   ethnicity = Ethnicity,
#   hh.income = `Household Income`,
#   hh.size = `Household Size`,
#   ami = AMI,
#   amount = Amount,
#   employer.amount = `Employer Amount`,
#   current.zip = `Current Zip`,
#   house.number = `House Num`,
#   dir = Dir,
#   street = Street,
#   street.type = `Street Type`,
#   


# write out
write_csv(df.geo@data, "data/processed/homebuying_incentives/homebuying_incentive_programs.csv")

