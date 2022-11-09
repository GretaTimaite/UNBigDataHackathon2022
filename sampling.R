# AIM: sampling using kmeans

# CO2 emissions data
co2emissions = readxl::read_excel("data/CO2emissionsbycountry.xlsx",
                              skip = 2)

# GDP per capita data
gdp <- readxl::read_excel("data/gdp/gdp.xls", skip = 2)

# our world :)
world = spData::world

# Select columns that will be used for kmeans clustering
gdp_2019 = gdp |> dplyr::select(`Country Code`, `2019`)
colnames(gdp_2019) = c("gdp_country_name", "gdp_2019") # rename to avoid duplication of column names

co2emissions2019 = co2emissions |> dplyr::select(`Country Code`, `2019`)

# join both subsetted datasets
gdp_co2 = cbind(gdp_2019, co2emissions2019) |> tidyr::drop_na()


# let's find out which countries in world are also in gdp_land
gdp_co2_logical = gdp_co2$`Country Code` %in% world$is
gdp_co2_logical |> table()
gdp_co2_sub = gdp_co2[gdp_co2_logical,] |> tidyr::drop_na()


# join datasets 
world_joined = dplyr::left_join(gdp_co2_sub, world,
                            by = c("Country Name" = "name_long" ))

# standardise 
gdp_co2_scaled1 = scale(world_joined$gdp_2019)
gdp_co2_scaled2 = scale(world_joined$`2019`)
world_joined$gdp_2019_scale = gdp_co2_scaled1
world_joined$landtemp_2019_scale = gdp_co2_scaled2

# determine optimal cluster number
factoextra::fviz_nbclust((world_joined |> as.data.frame())[,c("gdp_2019_scale", "landtemp_2019_scale")],
                         FUNcluster = kmeans)
# 3 clusters are recommended

# run kmeans
clusters = stats::kmeans((world_joined |> as.data.frame())[,c("gdp_2019_scale", "landtemp_2019_scale")], centers = 3, iter.max = 20)
clusters$cluster |> table()

world_joined$cluster = clusters$cluster
world_joined |> dplyr::filter(cluster == 3)
world_joined = sf::st_as_sf(world_joined, 
                        sf_column_name = "geom")

# ploting!
tmap::tm_shape(world_joined)+tmap::tm_polygons(col = "cluster")

# random sample
dplyr::sample_n(world_joined |> dplyr::filter(cluster == 1), 1)
dplyr::sample_n(world_joined |> dplyr::filter(cluster == 2), 1)
dplyr::sample_n(world_joined |> dplyr::filter(cluster == 3), 1)
