library(tmap)
library(tidyverse)
library(geojsonio)
library(sf)
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

# make tmap from OSM data
# read excel file for OSM data
OSM = readxl::read_excel("data/OSM.xlsx"
                             ) 
OSM$timestamp <- substr(OSM$timestamp,0,4)
    #%>% rename("count_ren"='0')
# pivot_wider()
OSM1 <- OSM %>% pivot_wider(names_from = timestamp)
# join with world data 
OSM_sf <- world %>% select(name_long) %>%  left_join(OSM1,by=c("name_long"="Country")) 
# make tmap
tm_shape(OSM_sf) +
  tm_polygons(col= "2022",palette=viridis(n=7),alpha = 0.5)

# incorporate data from Eric
wvs %>% names()
sfdf %>% st_drop_geometry() %>% select(name_long,dis_ISO3)
wv <- wvs %>% select(country_5) %>% left_join((sfdf %>% st_drop_geometry() %>% select(name_long,dis_ISO3)),by=(c("country_5"="name_long")))
theme_set(theme_light())
# Explore how responses have changed over time

env_count <- wvs %>% 
  # Select evn columns
  mutate("country_5"=as_vector( wv[,2])) %>% 
  filter(country_5=="ARG" |country_4=="ARG" |country_6=="ARG" |country_7=="ARG"  ) %>% 
  select(contains("env")) %>% 
  #filter(if_any(everything(), is.na)) %>% 
  # Reshape data for easy analysis
  pivot_longer(everything(), names_to = "env", values_to = "opinion") %>% 
  #mutate(across(everything()))
  # Drop missing values
  drop_na() %>% 
  mutate(opinion = factor(opinion)) %>% 
  # Count number of respondents in each category
  count(env, opinion) %>% 
  group_by(env) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct = n/total) %>% 
  # Rename rows
  mutate(env = case_when(
    env == "env_4_num" ~ "wave_4",
    env == "env_5_num" ~ "wave_5",
    env == "env_6_num" ~ "wave_6",
    env == "env_7_num" ~ "wave_7"
  ))

# Visualize this
env_count %>% 
  ggplot(mapping = aes(x = env, y = pct*100)) +
  geom_col(aes(fill = opinion), position = "dodge", alpha = 0.8) +
  paletteer::scale_fill_paletteer_d("ggthemes::Tableau_10",
                                    labels=c("protect environment", " Economic growth", "Other")) +
  ggtitle("Protecting environment vs Economic growth") +
  labs(x = "survey period",
       y = "% of respondents in survey") +
  theme(plot.title = element_text(hjust = 0.5))
