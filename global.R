# convert matrix to dataframe
library(dygraphs)
library(data.table)
library(ggplot2)
library(readr)
library(plotly)
library(stringi)
library(DT)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(data.table)
library(DT)
library(shiny)
library(googleVis)
library(ggthemes)
library(leaflet)


nycrollingsales = fread('nyc-rolling-sales.csv', stringsAsFactors = F, na.strings = c('', '-'))
nycoccupancy = read.csv('DOB_Certificate_Of_Occupancy.csv', stringsAsFactors = F, na.strings = c('NA'))


nycoccupancy = nycoccupancy %>%
  mutate(., LOT = as.integer(LOT)) %>%
  mutate(., BOROUGH = toupper(BOROUGH))


  
nycoccupancy$BOROUGH[nycoccupancy$BOROUGH == 'STATEN ISLAND'] = 'STATEN_ISLAND'




nycsales = nycrollingsales %>% 
  filter(., SALE_PRICE != 'NA') %>%
  mutate(., SALE_PRICE = as.numeric(SALE_PRICE)) %>%
  mutate(., BOROUGH = as.character(BOROUGH)) %>%
  filter(., SALE_PRICE > 100000) %>%
  filter(., ZIP_CODE != 0) %>%
  mutate(., SALE_DATE = gsub('0:00', '', SALE_DATE)) %>%
  mutate(., ADDRESS = stri_trans_totitle(ADDRESS)) %>%
  mutate(., BUILDING_CLASS_CATEGORY = substr(BUILDING_CLASS_CATEGORY,4, 100))
  

nycsales = nycsales[,-1]


nycsales$SALE_DATE = as.Date(nycsales$SALE_DATE, '%m/%d/%y')
nycsales$SALE_DATE = format(nycsales$SALE_DATE, "%y/%m")



boroswitch = function(x){
  switch(x, '1' = 'MANHATTAN', '2' = 'BRONX', '3' = 'BROOKLYN', '4' = 'QUEENS', '5' = 'STATEN_ISLAND')
}



nycsales$BOROUGH = sapply(nycsales$BOROUGH, boroswitch)


excludebuildingclass = c('29 COMMERCIAL GARAGES', '37 RELIGIOUS FACILITIES', '38 ASYLUMS AND HOMES', '44 CONDO PARKING', '28 COMMERCIAL CONDOS', '47 CONDO NON-BUSINESS STORAGE', '31 COMMERCIAL VACANT LAND', '41 TAX CLASS 4 - OTHER', '34 THEATRES', 
                         '36 OUTDOOR RECREATIONAL FACILITIES', '32 HOSPITAL AND HEALTH FACILITIES', "42 CONDO CULTURAL/MEDICAL/EDUCATIONAL/ETC","33 EDUCATIONAL FACILITIES", "05 TAX CLASS 1 VACANT LAND", '06 TAX CLASS 1 - OTHER', '46 CONDO STORE BUILDINGS',
                         "35 INDOOR PUBLIC AND CULTURAL FACILITIES","40 SELECTED GOVERNMENTAL FACILITIES", "48 CONDO TERRACES/GARDENS/CABANAS", "11 SPECIAL CONDO BILLING LOTS", "39 TRANSPORTATION FACILITIES", '17 CONDO COOPS', '22 STORE BUILDINGS', '23 LOFT BUILDINGS', '11A CONDO-RENTALS')


filteredbuildingclass = which(!(nycsales$BUILDING_CLASS_CATEGORY %in% excludebuildingclass))
nycsales = nycsales[filteredbuildingclass, ]


rentalclass = c("07 RENTALS - WALKUP APARTMENTS", "08 RENTALS - ELEVATOR APARTMENTS", '14 RENTALS - 4-10 UNIT')
condoclass = c('12 CONDOS - WALKUP APARTMENTS', "13 CONDOS - ELEVATOR APARTMENTS", "15 CONDOS - 2-10 UNIT RESIDENTIAL", "16 CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT", '04 TAX CLASS 1 CONDOS')
coopclass = c("09 COOPS - WALKUP APARTMENTS", "10 COOPS - ELEVATOR APARTMENTS" )
singlefamilyclass = c('01 ONE FAMILY DWELLINGS')
multifamilyclass = c('02 TWO FAMILY DWELLINGS', '03 THREE FAMILY DWELLINGS')
officeclass = c('21 OFFICE BUILDINGS', '43 CONDO OFFICE BUILDINGS')
hotelclass = c('25 LUXURY HOTELS', '26 OTHER HOTELS', '45 CONDO HOTELS')
industrialclass = c('30 WAREHOUSES', '27 FACTORIES')



takeaddress = function(x){
  for (i in x) {
    return (i[1])
  }
}

convertaddress = function(x){
  switch(x,
         MANHATTAN = ', New York, NY',
         BRONX = ', BRONX, NY',
         STATEN_ISLAND = ', STATEN ISLAND, NY',
         QUEENS = ', QUEENS, NY',
         BROOKLYN = ', BROOKLYN, NY')
}


fulladdress = sapply(nycsales$BOROUGH, convertaddress)


nycsales = nycsales %>% 
  mutate(ADDRESS = sapply(strsplit(nycsales$ADDRESS, ',', fixed = TRUE), takeaddress)) %>%
  mutate(ADDRESS = paste(ADDRESS, fulladdress, ZIP_CODE))


nyctotal = inner_join(nycsales, nycoccupancy, by = c('BLOCK' = 'BLOCK', 'LOT' = 'LOT', 'BOROUGH' = 'BOROUGH'))
nyctotal = nyctotal %>% filter(!is.na(SALE_PRICE))
nyctotal = nyctotal[!is.na(as.numeric(nyctotal$LATITUDE)), ]
nyctotal = nyctotal[!is.na(as.numeric(nyctotal$LONGITUDE)), ]
nyctotal = unique(nyctotal)



BOROUGH = unique(nycsales$BOROUGH)
NEIGHBORHOOD = unique(nycsales$NEIGHBORHOOD)
BUILDING_CLASS_CATEGORY = unique(nycsales$BUILDING_CLASS_CATEGORY)


borough = unique(nycsales$BOROUGH)
neighborhood = unique(nycsales$NEIGHBORHOOD)
buildingclasscategory = unique(nycsales$BUILDING_CLASS_CATEGORY)




                            



