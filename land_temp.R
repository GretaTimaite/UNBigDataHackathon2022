# AIM: visualise CO2 emissions

# read excel file
landtemp = readxl::read_excel("data/CO2emissionsbycountry.xlsx",
                              skip = 2)

# check if read correctly
landtemp[1:5,1:10]

# plot CO2 emissions in 2021

install.packages("rnaturalearth")

world = spData::world

world |> plot()

# let's find out which countries in world are also in landtemp
landtemp_logical = landtemp$`Country Name` %in% world$name_long 
logical |> table()
landtemp_sub = landtemp[landtemp_logical,]

# let's find out which countries in landtemp are also in world
world_logical = world$name_long %in% landtemp$`Country Name`
world_logical |> table()
world_sub = world[world_logical,]

# join both subsetted datasets
world_land = cbind(world_sub, landtemp_sub,
                   sf_column_name = "geom")
world_land |> dplyr::select("X2017") |> plot()

# plot CO2 emissions in 2021
world_land |> dplyr::select("X2010") |> plot()

# EDA
library(tidyverse)
world %>% arrange(name_long) 
landtemp
library(tmap)
world_land %>% tm_shape() + tm_polygons(col="X2017",style="quantile")
world_land %>% tm_shape() + tm_polygons(col="X2010",breaks=c(0,170,4270,9932,33050,128706,10096010))
