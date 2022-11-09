# The aim of this script is to prepare data for further analysis
# Tasks: clean, subset, join


# ======== get data ========
# CO2 emissions data
co2emissions = readxl::read_excel("data/CO2emissionsbycountry.xlsx",
                                  skip = 2)

# GDP per capita data
gdp <- readxl::read_excel("data/gdp/gdp.xls", skip = 2)

# our world :)
world = spData::world

# Renewable energy consumption (% of total final energy consumption)
renewable_ec <- readxl::read_excel("data/Renewable energy consumption (% of total final energy consumption).xlsx", skip = 2)

# Frequencies of climate-related natural disasters
climate_disasters <- readxl::read_excel("data/Climate-related_Disasters_Frequency.xlsx")

# Land temperatures
land_temp <- readr::read_csv("data/GlobalLandTemperaturesByCountry.csv")

# ====== subset data =====
# first let's subset all datasets, so the period is from 2000 to 2019 as a great number of countries have data only from 1990 and/or lack 2020 onwards
# 20 years timescale should be enought to understand changes in temperature, emissions, attitudes, etc

co2emissions_clean = co2emissions |> 
  dplyr::select(c(1:2,45:64))
colnames(co2emissions_clean) = paste0("co2_", colnames(co2emissions_clean)) # rename column names so we can join by column

gdp_clean = gdp |> 
  dplyr::select(c(1:2,45:64))
colnames(gdp_clean) = paste0("gdp_", colnames(gdp_clean))

renewable_clean = renewable_ec |> 
  dplyr::select(c(1:2,45:64))
colnames(renewable_clean) = paste0("ren_", colnames(renewable_clean))

disasters_clean = climate_disasters |> 
  dplyr::filter(Indicator == "Climate related disasters frequency, Number of Disasters: TOTAL") |> # focus on total number of disasters
  dplyr::select(c(2:4,32:51))
colnames(disasters_clean) = paste0("dis_", colnames(disasters_clean))

# land temp data requires more attention...
# we'll aggregative, so we have an average yearly temp instead of monthly, which is a bit too granular 
land_temp_clean = land_temp |> 
  dplyr::mutate(year = lubridate::year(dt)) |> 
  dplyr::filter(year >= 2000 & year <=2019) |> 
  dplyr::group_by(Country, year) |> 
  dplyr::summarise(average_temp = mean(AverageTemperature)) |> 
  tidyr::pivot_wider(names_from = year, values_from = average_temp) # rehsape to wide format
colnames(land_temp_clean) = paste0("temp_", colnames(land_temp_clean))
# oh no we only have data up to 2013-09-01!
# let's stick to it...

# ===== join data =====

joined_df = cbind(co2emissions_clean, gdp_clean, renewable_clean)  # leave disasters aside for now

# OK, let's drop some columns
joined_df_clean = joined_df |> 
  dplyr::select( -c("ren_Country Name", "ren_Country Code", "gdp_Country Name", "gdp_Country Code"))

# join disasters data with world but first remove any NA values in ISO2
world_clean_iso = world |> dplyr::filter(!is.na(iso_a2))
disasters_clean_iso = disasters_clean |> dplyr::filter(!is.na(dis_ISO2))
# left join
disasters_world = dplyr::left_join(world_clean_iso, disasters_clean_iso,
                                   by = c("iso_a2" = "dis_ISO2" ))

# left join with land temperature
dis_temp = dplyr::left_join(disasters_world, land_temp_clean,
                            by = c("name_long" = "temp_Country"))

# let's plot disasters_world for sanity check
tmap::tm_shape(disasters_world) + 
  tmap::tm_polygons(col = "dis_F2019")

# now let's join all the datasets into one grand dataframe :)
climate_action_data = dplyr::left_join(dis_temp, joined_df_clean,
                                       by = c("dis_ISO3" = "co2_Country Code"))
# let's plot again
tmap::tm_shape(climate_action_data) + 
  tmap::tm_polygons(col = "temp_2012")

# let's save data geojson
# sf::st_write(climate_action_data,
# "climate_action_data.geojson")




