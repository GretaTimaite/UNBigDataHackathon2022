# The aim of this script is to prepare data for further analysis
# Tasks: clean, subset, join

# FINAL data is here: https://github.com/GretaTimaite/UNBigDataHackathon2022/releases/tag/v0.1
# this excludes World Values Survey as redistribution is not allowed 
# please go to their website to download required data. I recommend getting .sav (SPSS) extension


# ======== get data ========
# CO2 emissions data
co2emissions = readxl::read_excel("data/CO2emissionsbycountry.xlsx",
                                  skip = 2)

# GDP per capita data 
gdp = readxl::read_excel("data/gdp/gdp.xls", skip = 2)

# our world :)
world = spData::world

# Renewable energy consumption (% of total final energy consumption)
renewable_ec = readxl::read_excel("data/Renewable energy consumption (% of total final energy consumption).xlsx", skip = 2)

# Frequencies of climate-related natural disasters
climate_disasters = readxl::read_excel("data/Climate-related_Disasters_Frequency.xlsx")

# Land temperatures
land_temp = readr::read_csv("data/GlobalLandTemperaturesByCountry.csv")

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

# ====== World Values Survey =====
# NOTE world values survey 

# read waves 4-7
wvs4 = foreign::read.spss("/Users/gretatimaite/Desktop/UNBigDataHackathon2022/data/WV4_Data_spss_v20201117.sav", to.data.frame = TRUE)
wvs5 = foreign::read.spss("/Users/gretatimaite/Desktop/UNBigDataHackathon2022/data/WV5_Data_Spss_v20180912.sav", to.data.frame = TRUE)
wvs6 = foreign::read.spss("/Users/gretatimaite/Desktop/UNBigDataHackathon2022/data/WV6_Data_sav_v20201117.sav", to.data.frame = TRUE)
wvs7 = foreign::read.spss("/Users/gretatimaite/Desktop/UNBigDataHackathon2022/data/WVS_Cross-National_Wave_7_spss_v4_0.sav", to.data.frame = TRUE)

# let's extract data from each wave:
# country
# Protecting environment vs. Economic growth
# sex
# age
# education
# social class
# income level

wvs7_sub = wvs7 |> dplyr::select(B_COUNTRY_ALPHA, Q111, Q260, X003R2, Q275R, Q287, Q288R)
colnames(wvs7_sub) = c("country_7", "env_7", "sex_7", "age_7", "education_7", "class_7", "income_7")

wvs6_sub = wvs6 |> dplyr::select(B_COUNTRY_ALPHA, V81, V240, X003R2, V248, V238, V239)
colnames(wvs6_sub) = c("country_6", "env_6", "sex_6", "age_6", "education_6", "class_6", "income_6")

wvs5_sub = wvs5 |> dplyr::select(V2, V104, V235, V237, V238, V252, V253) # I cannot believe this dataset doesn't have ISO code...
colnames(wvs5_sub) = c("country_5", "env_5", "sex_5", "age_5", "education_5", "class_5", "income_5")

wvs4_sub = wvs4 |> dplyr::select(B_COUNTRY_ALPHA, V36, V223, V225R2, V226, V235, V236)
colnames(wvs4_sub) = c("country_4", "env_4", "sex_4", "age_4", "education_4", "class_4", "income_4")

# let's recode variables...
# also
# find out max length of each, we'll use this information to create NA cells, so length of all datasets is the same
# then we'll be able to join them easily

max_length = max(c(nrow(wvs4_sub), nrow(wvs5_sub), nrow(wvs6_sub), nrow(wvs7_sub)))
max_length # wave 6 has most obervations

wvs7_sub = wvs7_sub |> 
  dplyr::mutate (env_7_num = env_7 |> as.numeric(),
                # env_7_num = c(wvs7_sub$env_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                sex_7_num = sex_7 |> as.numeric(),
                # sex_7_num = c(wvs7_sub$sex_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                age_7_num = age_7 |> as.numeric(),
                # age_7_num = c(wvs7_sub$age_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                education_7_num = education_7 |> as.numeric(),
                # education_7_num = c(wvs7_sub$education_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                income_7_num = income_7 |> as.numeric(),
                # income_7_num = c(wvs7_sub$income_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                )

wvs7_new = data.frame(env_7_num = c(wvs7_sub$env_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                      sex_7_num = c(wvs7_sub$sex_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                      age_7_num = c(wvs7_sub$age_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                      education_7_num = c(wvs7_sub$education_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                      income_7_num = c(wvs7_sub$income_7_num, rep(NA, max_length - nrow(wvs7_sub))),
                      country_7 = c(wvs7_sub$country_7, rep(NA, max_length - nrow(wvs7_sub)))
                      )

wvs6_sub = wvs6_sub |> 
  dplyr::mutate(env_6_num = env_6 |> as.numeric() |> as.factor(),
                sex_6_num = sex_6 |> as.numeric(),
                age_6_num = age_6 |> as.numeric(),
                education_6_num = education_6 |> as.numeric(),
                income_6_num = income_6 |> as.numeric())

wvs6_new = data.frame(env_6_num = c(wvs6_sub$env_6_num, rep(NA, max_length - nrow(wvs6_sub))),
                      sex_6_num = c(wvs6_sub$sex_6_num, rep(NA, max_length - nrow(wvs6_sub))),
                      age_6_num = c(wvs6_sub$age_6_num, rep(NA, max_length - nrow(wvs6_sub))),
                      education_6_num = c(wvs6_sub$education_6_num, rep(NA, max_length - nrow(wvs6_sub))),
                      income_6_num = c(wvs6_sub$income_6_num, rep(NA, max_length - nrow(wvs6_sub))),
                      country_6 = c(wvs6_sub$country_6, rep(NA, max_length - nrow(wvs6_sub)))
)

wvs5_sub = wvs5_sub |> 
  dplyr::mutate(env_5_num = env_5 |> as.numeric() |> as.factor(),
                sex_5_num = sex_5 |> as.numeric(),
                age_5_num = age_5 |> as.numeric() |> cut(breaks = c(0, 29, 49, 120), labels = c(1,2,3)),
                education_5_num = education_5 |> as.numeric(),
                income_5_num = income_5 |> as.numeric())

# for some reason countries are returned as levels in integer form (e.g. 1) rather than character (i.e. "Andora"),
# so we'll need to do tricks here :)
unique_vals = wvs5_sub$country_5 |> unique() |> as.character()
wvs5_new = data.frame(env_5_num = c(wvs5_sub$env_5_num, rep(NA, max_length - nrow(wvs5_sub))),
                      sex_5_num = c(wvs5_sub$sex_5_num, rep(NA, max_length - nrow(wvs5_sub))),
                      age_5_num = c(wvs5_sub$age_5_num, rep(NA, max_length - nrow(wvs5_sub))),
                      education_5_num = c(wvs5_sub$education_5_num, rep(NA, max_length - nrow(wvs5_sub))),
                      income_5_num = c(wvs5_sub$income_5_num, rep(NA, max_length - nrow(wvs5_sub))),
                      country_5 = c(wvs5_sub$country_5, rep(NA, max_length - nrow(wvs5_sub))) |> factor(labels = unique_vals)
)


wvs4_sub = wvs4_sub |> 
  dplyr::mutate(env_4_num = env_4 |> as.numeric() |> as.factor(),
                sex_4_num = sex_4 |> as.numeric(),
                age_4_num = age_4 |> as.numeric(),
                education_4_num = education_4 |> as.numeric(),
                income_4_num = income_4 |> as.numeric())

wvs4_new = data.frame(env_4_num = c(wvs4_sub$env_4_num, rep(NA, max_length - nrow(wvs4_sub))),
                      sex_4_num = c(wvs4_sub$sex_4_num, rep(NA, max_length - nrow(wvs4_sub))),
                      age_4_num = c(wvs4_sub$age_4_num, rep(NA, max_length - nrow(wvs4_sub))),
                      education_4_num = c(wvs4_sub$education_4_num, rep(NA, max_length - nrow(wvs4_sub))),
                      income_4_num = c(wvs4_sub$income_4_num, rep(NA, max_length - nrow(wvs4_sub))),
                      country_4 = c(wvs4_sub$country_4, rep(NA, max_length - nrow(wvs4_sub)))
)


# sanity check that levels match numeric values
wvs7_sub$age_7 |> unique()
wvs7_sub$age_7_num |> unique()
wvs6_sub$age_6 |> unique()
wvs6_sub$age_6_num |> unique()
wvs5_sub$age_5 |> unique()
wvs5_sub$age_5_num |> unique()
wvs4_sub$age_4 |> unique()
wvs4_sub$age_4_num |> unique()

# let's join all wvs waves into one

wvs = cbind(wvs4_new, wvs5_new, wvs6_new, wvs7_new)
# let's save data
# write.csv(wvs,
#           "wvs.csv")

# ===== OSM data ====

# import csv file containing OSM data
osm = readxl::read_excel("data/osm.xlsx")

# let's extract year from the date
osm_clean = osm |> 
  dplyr::mutate(year = lubridate::year(timestamp)
  ) |> 
  dplyr::select(-1)

# save our new osm dataset
# write.csv(osm_clean,
#           "osm_clean.csv")

