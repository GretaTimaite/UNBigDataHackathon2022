library(tmap)
# read excel file
CO2emis = readxl::read_excel("data/CO2emissionsbycountry.xlsx",
                              skip = 2)

# check if read correctly
# NOTE: uses 3-letter country code
CO2emis[1:5,1:10]

# count NA values in each column
colSums(is.na(CO2emis))

# which years have 266 NAs(full column is NA)?
names(CO2emis[,colSums(is.na(CO2emis))==266])
# NOTE: only 1990-2019 is viable
# which ones have more than 30 NAs?
names(CO2emis[,colSums(is.na(CO2emis))!=266 & colSums(is.na(CO2emis)) > 30])
# suggests 1992-2019 is best choice

# load country shapefile
# NOTE: uses iso_a2 (2-letter country code)
world = spData::world

wws <- read.csv("data/wvs.csv")
# NOTE: world view sirvey uses 3-letter country codes

# load geojson file
# Let's read the jeoJson file that is stored on the web with the geojsonio library:
library(geojsonio)
library(sf)
# spdf <- geojson_read("https://raw.githubusercontent.com/GretaTimaite/UNBigDataHackathon2022/main/climate_action_data.geojson",  what = "sp")
# load locally for now
sfdf <- geojson_read("data/climate_action_data.geojson",what="sp") %>% st_as_sf()
sfdf %>% tm_shape() + tm_polygons(col="ren_2018")
