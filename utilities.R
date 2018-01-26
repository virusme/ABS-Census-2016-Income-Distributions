## Data preparation utilties
#

# 
prepIndividual <- function(individual) {
    # data cleansing
    individual <- individual[POA != '(c) Commonwealth of Australia 2017',]
    individual <- individual[POA != 'INFO',]
    tmpnames <- names(individual)
    names(individual)[tmpnames == "INCP Total Personal Income (weekly)"] = 'weeklyIncome'
    #  split POA column into two '2000, NSW' into postcode and state
    tmp <- colsplit(individual$POA, split = "\\, ", names = c('postcode', 'state'))
    individual[['postcode']] <- tmp$postcode
    individual[['state']] <- NULL # will get 'state' back when we merge with latLong
    individual[['POA']] <- NULL # first column is POA, drop this column
    # convert postcode to numeric
    individual[['postcode']] <- as.numeric(as.character(individual[['postcode']]))
    # force all columns except weekly income to numeric
    xx <- individual[, setdiff(names(individual), 'weeklyIncome') := lapply(.SD, function(x)(as.numeric(x))), .SDcols = setdiff(names(individual), 'weeklyIncome')]
    #
    return(individual)
}

#
prepHousehold <- function(household) {
    ##  Data cleansing
    #
    tmpnames <- names(household)
    names(household)[tmpnames == 'HIND Total Household Income (weekly)'] = 'weeklyIncome'

    #  split POA column into two '2000, NSW' into postcode and state
    tmp <- colsplit(household$POA, split = "\\, ", names = c('postcode', 'state'))
    household[['postcode']] <- tmp$postcode
    household[['state']] <- NULL
    household[['POA']] <- NULL # first column is POA drop this column
    # convert postcode to numeric
    household[['postcode']] <- as.numeric(as.character(household[['postcode']]))
    # force all columns except weekly income to numeric
    xx <- household[, setdiff(names(household), 'weeklyIncome') := lapply(.SD, function(x)(as.numeric(x))), .SDcols = setdiff(names(household), 'weeklyIncome')]
    # 
    return(household)
}

# Household composition
prepHHComposition <- function(dt) {
    # first 2:7 contains [HH income bands, HH composition...]
    mydt <- dt[, 1:7]
    # remove 'Not applicable.y' column (related to hhCompo)
    mydt[['Not applicable.x']] <- NULL
    return(setDT(mydt))
}

# Mortgage Repay
prepMortgageRepay <- function(dt) {
    ## Data manipulation: Need to flip the data from weeklyIncome x mortrepay TO mortRepay x weeklyIncome
    # first 2, 8:28 contains [postcode, HH income bands, mortgage repayments...]
    mydt1 <- cbind(dt[, 1:2], dt[, 8:28])
    # remove 'Not applicable.y' column (related to mortPay)
    mydt1[['Not applicable.y']] <- NULL
    # melt with postcode and weeklyIncome as reference
    mydt.melt <- melt(mydt1, id.vars = c('postcode', 'weeklyIncome'))
    # reshape back with postcode and 'variable'(mortRepays) as reference, use fun.aggregate to aggregate data
    mydt <- dcast(mydt.melt,
                  postcode + variable ~ weeklyIncome,
                  value.var = 'value',
                  fun.agg = function(x)(sum(as.numeric(x))))
    # rename the 2nd column as mortRepay
    names(mydt)[2] <- 'mortRepay'
    # there could a column with string 'NA' name
    mydt[[which(names(mydt) == 'NA')]] <- NULL
    # now the datatable will have [ mortRepay, HH income bands..]
    return(setDT(mydt))
}

# Rent Pay
prepRentPay <- function(dt) {
    ## Data manipulation: Need to flip the data from weeklyIncome x rent TO rent x weeklyIncome
    # first 2, 29:53 contains [postcode, HH income bands, rent payments...]
    mydt1 <- cbind(dt[, 1:2], dt[, 29:53])
    # remove 'Not Applicable' column (related to rentPay)
    mydt1[['Not applicable']] <- NULL
    # melt with postcode and weeklyIncome as reference
    mydt.melt <- melt(mydt1, id.vars = c('postcode', 'weeklyIncome'))
    # reshape back with postcode and 'variable'(rents) as reference, use fun.aggregate to aggregate data
    mydt <- dcast(mydt.melt,
                  postcode + variable ~ weeklyIncome,
                  value.var = 'value',
                  fun.agg = function(x)(sum(as.numeric(x))))
    # rename the 2nd column as mortRepay
    names(mydt)[2] <- 'rent'
    # there could a column with string 'NA' name
    mydt[[which(names(mydt) == 'NA')]] <- NULL
    # now the datatable will have [ rent, HH income bands..]
    return(setDT(mydt))
}
