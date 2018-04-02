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

#read in the files
nycrollingsales = fread('nyc-rolling-sales.csv', stringsAsFactors = F, na.strings = c('', '-'))
nycoccupancy = read.csv('DOB_Certificate_Of_Occupancy.csv', stringsAsFactors = F, na.strings = c('NA'))


#change data types and switch borough names to upper case
nycoccupancy = nycoccupancy %>%
  mutate(., LOT = as.integer(LOT)) %>%
  mutate(., BOROUGH = toupper(BOROUGH))


#change the format for staten island
nycoccupancy$BOROUGH[nycoccupancy$BOROUGH == 'STATEN ISLAND'] = 'STATEN_ISLAND'



#more data cleaning
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

#change the format of dates
nycsales$SALE_DATE = as.Date(nycsales$SALE_DATE, '%m/%d/%y')
nycsales$SALE_DATE = format(nycsales$SALE_DATE, "%y/%m")


#formatting the boroughs 
boroswitch = function(x){
  switch(x, '1' = 'MANHATTAN', '2' = 'BRONX', '3' = 'BROOKLYN', '4' = 'QUEENS', '5' = 'STATEN_ISLAND')
}



nycsales$BOROUGH = sapply(nycsales$BOROUGH, boroswitch)

#excluding certain classes of properties
excludebuildingclass = c('COMMERCIAL GARAGES', 'RELIGIOUS FACILITIES', 'ASYLUMS AND HOMES', 'CONDO PARKING', 'COMMERCIAL CONDOS', 'CONDO NON-BUSINESS STORAGE', 'COMMERCIAL VACANT LAND', 'TAX CLASS 4 - OTHER', 'THEATRES', 
                         'OUTDOOR RECREATIONAL FACILITIES', 'HOSPITAL AND HEALTH FACILITIES', "CONDO CULTURAL/MEDICAL/EDUCATIONAL/ETC","EDUCATIONAL FACILITIES", "TAX CLASS 1 VACANT LAND", 'TAX CLASS 1 - OTHER', 'CONDO STORE BUILDINGS',
                         "INDOOR PUBLIC AND CULTURAL FACILITIES","SELECTED GOVERNMENTAL FACILITIES", "CONDO TERRACES/GARDENS/CABANAS", "SPECIAL CONDO BILLING LOTS", "TRANSPORTATION FACILITIES", 'CONDO COOPS', 'STORE BUILDINGS', 'LOFT BUILDINGS', ' CONDO-RENTALS')


filteredbuildingclass = which(!(nycsales$BUILDING_CLASS_CATEGORY %in% excludebuildingclass))


filteredbuildingclass


nycsales = nycsales[filteredbuildingclass, ]

#categorizing the remaining classes of the properties 
rentalclass = c("RENTALS - WALKUP APARTMENTS", "RENTALS - ELEVATOR APARTMENTS", 'RENTALS - 4-10 UNIT')
condoclass = c('CONDOS - WALKUP APARTMENTS', "CONDOS - ELEVATOR APARTMENTS", "CONDOS - 2-10 UNIT RESIDENTIAL", "CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT", 'TAX CLASS 1 CONDOS')
coopclass = c("COOPS - WALKUP APARTMENTS", "COOPS - ELEVATOR APARTMENTS" )
singlefamilyclass = c('ONE FAMILY DWELLINGS')
multifamilyclass = c('TWO FAMILY DWELLINGS', 'THREE FAMILY DWELLINGS')
officeclass = c('OFFICE BUILDINGS', 'CONDO OFFICE BUILDINGS')
hotelclass = c('LUXURY HOTELS', 'OTHER HOTELS', 'CONDO HOTELS')
industrialclass = c('WAREHOUSES', 'FACTORIES')


nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% rentalclass] = 'RENTAL'
nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% condoclass] = 'CONDO'
nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% coopclass] = 'CO-OP'
nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% singlefamilyclass] = 'SINGLE-FAMILY'
nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% multifamilyclass] = 'MULTI-FAMILY'
nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% officeclass] = 'OFFICE'
nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% hotelclass] = 'HOTEL'
nycsales$BUILDING_CLASS_CATEGORY[nycsales$BUILDING_CLASS_CATEGORY %in% industrialclass] = 'INDUSTRIAL'


unique(nycsales$BUILDING_CLASS_CATEGORY)




#formatting the addresses
convertaddress = function(x){
  switch(x,
         MANHATTAN = ', New York, NY',
         BRONX = ', BRONX, NY',
         STATEN_ISLAND = ', STATEN ISLAND, NY',
         QUEENS = ', QUEENS, NY',
         BROOKLYN = ', BROOKLYN, NY')
}


fulladdress = sapply(nycsales$BOROUGH, convertaddress)

#formatting addresses
nycsales = nycsales %>% 
  mutate(ADDRESS = sapply(strsplit(nycsales$ADDRESS, ',', fixed = TRUE), takeaddress)) %>%
  mutate(ADDRESS = paste(ADDRESS, fulladdress, ZIP_CODE))


#join nycsales and nycoccupancy by block, lot and borough
nyctotal = inner_join(nycsales, nycoccupancy, by = c('BLOCK' = 'BLOCK', 'LOT' = 'LOT', 'BOROUGH' = 'BOROUGH'))

#filter out the rows with nans
nyctotal = nyctotal %>% filter(!is.na(SALE_PRICE))
nyctotal = nyctotal[!is.na(as.numeric(nyctotal$LATITUDE)), ]
nyctotal = nyctotal[!is.na(as.numeric(nyctotal$LONGITUDE)), ]
nyctotal = unique(nyctotal)


#creating variables for server input
BOROUGH = unique(nycsales$BOROUGH)
NEIGHBORHOOD = unique(nycsales$NEIGHBORHOOD)
BUILDING_CLASS_CATEGORY = unique(nycsales$BUILDING_CLASS_CATEGORY)


borough = unique(nycsales$BOROUGH)
neighborhood = unique(nycsales$NEIGHBORHOOD)
buildingclasscategory = unique(nycsales$BUILDING_CLASS_CATEGORY)










