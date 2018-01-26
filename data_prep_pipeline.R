#
# Analysis pipeline to analyse ABS Census 2016 data 
# 
rm(list = ls(all = TRUE))
#
#
dataPath <- 'Data\\ABS_census2016\\'

# for the ABS census 2016 data, I have extracted (via Census Table Builder)  postalCode vs weekly_income vs OTHER_DIMENSION
ageCSV <- paste0(dataPath, 'postalCode_vs_age-income.csv')
householdCompCSV <- paste0(dataPath, 'postalCode_vs_income-householdComp.csv')
industryCSV <- paste0(dataPath, 'postalCode_vs_income-industry.csv')
mortgageRepayCSV <- paste0(dataPath, 'postalCode_vs_income-mortagerepay.csv')
rentCSV <- paste0(dataPath, 'postalCode_vs_income-rent.csv')
latLongCSV <- paste0(dataPath, 'Australian_Post_Codes_Lat_Lon.csv')

# packages
source('fileIO.R')
source('utilities.R')
#source('visualisations.R')

## Read data
# read ABS census 2016 CSVs
age <- readTwoHeaderRows2table(ageCSV, referenceDimension = 2)  # has POA and individual weekly income
householdComp <- readTwoHeaderRows2table(householdCompCSV, referenceDimension = 1) # has POA and household weekly income
industry <- readTwoHeaderRows2table(industryCSV, referenceDimension = 1) # has POA and individual weekly income
mortgageRepay <- readTwoHeaderRows2table(mortgageRepayCSV, referenceDimension = 1) # has POA and household weekly income
rent <- readTwoHeaderRows2table(rentCSV, referenceDimension = 1) # has POA and household weekly income
# read lat long data
latLong <- as.data.table(read.csv(file = latLongCSV, header = TRUE))
latLong <- latLong[type == "Delivery Area                                ", ]   # extra long spaces a must


# merge data
individual <- merge(age, industry, by = c('POA', 'INCP Total Personal Income (weekly)'))
household <- merge(householdComp, mortgageRepay, by = c('POA', 'HIND Total Household Income (weekly)'))
household <- merge(household, rent, by = c('POA', 'HIND Total Household Income (weekly)'))

# prep data
individual <- prepIndividual(individual)
household <- prepHousehold(household)

# collapse all suburbs with same postcode to a list, and estimate the median latitude and longitude
latLong$suburb <- as.character(latLong$suburb)  # convert to character because retaining as factor blows up the memory-size of datatable
postCodeLatLong <- latLong[, lapply(.SD, unrollList2String), .SDcols = 'suburb', by = c('postcode', 'state')]
tmp <- latLong[, lapply(.SD, mean), .SDcols = c('lat', 'lon'), by = c('postcode', 'state')]
postCodeLatLong <- merge(postCodeLatLong, tmp, by = c('postcode', 'state'))

# merge postcodes to household data
#individual <- merge(individual, postCodeLatLong, by = 'postcode', all = TRUE)
household <- merge(household, postCodeLatLong, by = 'postcode', all = TRUE)

# segregate data
mortRepay <- prepMortgageRepay(household)
rentPay <- prepRentPay(household)
hhCompo <- prepHHComposition(household)
household <- list(alldata = household,
                  hhCompo = hhCompo,
                  mortRepay = mortRepay,
                  rentPay = rentPay)

# merge latLong information to data.tables - this is a many - one join hence using pylr::join function
individual <- merge(individual, postCodeLatLong, by = 'postcode', all = TRUE)
#household$alldata <- merge(household$alldata, postCodeLatLong, by = 'postcode', all = TRUE)
household$hhCompo <- merge(household$hhCompo, postCodeLatLong, by = 'postcode', all = TRUE)
household$mortRepay <- merge(household$mortRepay, postCodeLatLong, by = 'postcode', all = TRUE)
household$rentPay <- merge(household$rentPay, postCodeLatLong, by = 'postcode', all = TRUE)

# if lat-longs are NA remove those rows
individual <- individual[!(is.na(lat) | is.na(lon)),]
household$hhCompo <- household$hhCompo[!(is.na(lat) | is.na(lon)),]
household$mortRepay <- household$mortRepay[!(is.na(lat) | is.na(lon)),]
household$rentPay <- household$rentPay[!(is.na(lat) | is.na(lon)),]



#save
saveRDS(individual, file = paste0(dataPath, 'ABS_census2016_individualIncome.RDS'))
saveRDS(household, file = paste0(dataPath, 'ABS_census2016_householdIncome.RDS'))

