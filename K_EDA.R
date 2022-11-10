# read excel file
CO2emis = readxl::read_excel("data/CO2emissionsbycountry.xlsx",
                              skip = 2)

# check if read correctly
CO2emis[1:5,1:10]

# count NA values in each column
colSums(is.na(CO2emis))

# which years have 266 NAs(full column is NA)?
names(CO2emis[,colSums(is.na(CO2emis))==266])
# NOTE: only 1990-2019 is viable
# which ones have more than 30 NAs?
names(CO2emis[,colSums(is.na(CO2emis))!=266 & colSums(is.na(CO2emis)) > 30])
# suggests 1992-2019 is best choice


