library(tmap)
library(tidyverse)
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

# save sfdf as R object to be fed as data into the dashboard
#saveRDS(sfdf,"data/sfdf.RData")

# analysis to make Shiny plots of EDA per country
#coln <- sfdf %>% st_drop_geometry() %>% filter(name_long=="Kenya") %>% select(starts_with("dis")) %>% 
 # colnames()
coln <- sfdf %>% filter(name_long=="Kenya") %>%  st_drop_geometry() %>% select(-c(dis_Country,dis_ISO3,co2_Country.Name,gdpPercap)) %>%
  select(starts_with(c("gdp"))) %>% names()
coln1 <- as.numeric(substr(coln,nchar(coln)-4+1,nchar(coln)))
values <- sfdf %>% st_drop_geometry() %>%filter(name_long=="Kenya") %>%  select(-c(dis_Country,dis_ISO3,co2_Country.Name,gdpPercap)) %>%
  select(starts_with(c("gdp"))) %>% unname() %>%  as_vector() 
plot(x=coln1,y=values)
# library(stringr)
df <- data.frame(coln1,values)
ggplot(df) + geom_line(aes(x=coln1,y=values))  + geom_point(aes(x=coln1,y=values),lwd=2) +
  theme_minimal() 

# str_sub(coln, end=-4)
#substr(A, nchar(A)-3+1, nchar(A))
