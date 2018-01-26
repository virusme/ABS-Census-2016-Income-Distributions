#
# This is the data server for shiny app
#

# load data
dataPath <- 'Data\\ABS_census2016\\'
individual <- readRDS(paste0(dataPath, 'ABS_census2016_individualIncome.RDS'))
household <- readRDS(paste0(dataPath, 'ABS_census2016_householdIncome.RDS'))



server <- function(input, output) {


    # leaflet
    output$myMap <- renderLeaflet({ initLeaflet() })
    output$myMap2 <- renderLeaflet({ initLeaflet() })
   
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    # for Individual
    zipsInBounds_Individual <- reactive({

        if (is.null(input$myMap_bounds)) {
            return(individual = individual[FALSE,])
        }

        bounds <- input$myMap_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)

        # individual
        individual_subset <- subset(individual,
                                     lat >= latRng[1] & lat <= latRng[2] &
                                     lon >= lngRng[1] & lon <= lngRng[2])
        # exclude lat = NA and lon = NA
        individual_subset <- individual_subset[!(is.na(lat) | is.na(lon)), ]
        #
       
        return(individual = individual_subset)
    })

   # for Household
   zipsInBounds_Household <- reactive({

        if (is.null(input$myMap2_bounds)) {
        return(list(hhCompo = household$hhCompo[FALSE, ],
                    mortRepay = household$mortRepay[FALSE,],
                    rentPay = household$rentPay[FALSE,]))
        }

        bounds <- input$myMap2_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)

       # HH composition - household
        hhCompo <- subset(household$hhCompo,
                                        lat >= latRng[1] & lat <= latRng[2] &
                                        lon >= lngRng[1] & lon <= lngRng[2])

 
        # mortgage repay - household
        mortRepay <- subset(household$mortRepay,
                                        lat >= latRng[1] & lat <= latRng[2] &
                                        lon >= lngRng[1] & lon <= lngRng[2])
        # rent pay - household
        rentPay <- subset(household$rentPay,
                                        lat >= latRng[1] & lat <= latRng[2] &
                                        lon >= lngRng[1] & lon <= lngRng[2])

        # exclude lat = NA and lon = NA
        hhCompo <- hhCompo[!(is.na(lat) | is.na(lon)),]
        mortRepay <- mortRepay[!(is.na(lat) | is.na(lon)),]
        rentPay <- rentPay[!(is.na(lat) | is.na(lon)),]
        #

        return(list(hhCompo = hhCompo, mortRepay = mortRepay, rentPay = rentPay))
     })

    
    ##debugging
    #zipsInBounds_1 <- reactive({
    #    if (is.null(input$myMap_bounds)) {
    #        return('I am inside')
    #    }
    #
    #    return('i am outside')
    #})


    # plot income vs age
    output$plotIncomeVsAge <- renderPlot({
        # If no zipcodes are in view, don't plot
        if (nrow(zipsInBounds_Individual()) == 0) {
            return(NULL)
        }

        return(plotIncomeVsAge(zipsInBounds_Individual()))
        
    })

    # plot income vs industry
    output$plotIncomeVsIndustry <- renderPlot({
    # If no zipcodes are in view, don't plot
        if (nrow(zipsInBounds_Individual()) == 0) {
            return(NULL)
        }

        return(plotIncomeVsIndustry(zipsInBounds_Individual()))
    })

    # plot HH income vs composition
    output$plotHHIncomeVsComposition <- renderPlot({
        # If no zipcodes are in view, don't plot
        if (nrow(zipsInBounds_Household()$hhCompo) == 0) {
            return(NULL)
        }

        return(plotHHIncomeVsComposition(zipsInBounds_Household()$hhCompo))
    })

    # plot HH income vs mortgage repayments
    output$plotHHIncomeVsMortRepay <- renderPlot({
        # If no zipcodes are in view, don't plot
        if (nrow(zipsInBounds_Household()$mortRepay) == 0) {
            return(NULL)
        }

            return(plotHHIncomeVsMortRepay(zipsInBounds_Household()$mortRepay))
    })

    # plot HH income vs composition
    output$plotHHIncomeVsRent <- renderPlot({
        # If no zipcodes are in view, don't plot
        if (nrow(zipsInBounds_Household()$rentPay) == 0) {
            return(NULL)
        }

            return(plotHHIncomeVsRent(zipsInBounds_Household()$rentPay))
    })
    
    # event loop for display buttong
    #displayNow <- eventReactive(input$displayButton, { TRUE })
    # for debugging
    #output$nText <- renderText({ displayNow() })
    #

    # display clusters and available points on the map, display postcode popups when clicked.
    observe({
              # extract lat/longs for postcodes, remove duplciates
              sites <- zipsInBounds_Individual()[, lapply(.SD, function(x)(x[1])),
                                                    .SDcols = c('lat', 'lon', 'suburb', 'state'),
                                                    by = c('postcode')]
              # html content for popup
              htmlContent <- paste0('Postcode: <br/>',
                                     '<b>', sites$postcode, ', ', sites$state, '</b> <br/>')#,
                                     #'Suburbs:', sites$suburb)
              #
              leafletProxy("myMap") %>% clearMarkers() %>%
              addMarkers(lng = sites$lon,
                         lat = sites$lat,
                         popup = htmlContent,
                         clusterOptions = markerClusterOptions())
    })

    # display clusters and available points on the map, display postcode popups when clicked.
    observe({
        # extract lat/longs for postcodes, remove duplciates
        sites <- zipsInBounds_Household()$rentPay[, lapply(.SD, function(x)(x[1])),
                                                        .SDcols = c('lat', 'lon', 'suburb', 'state'),
                                                        by = c('postcode')]
        # html content for popup
        htmlContent <- paste0('Postcode: <br/>',
                                         '<b>', sites$postcode, ', ', sites$state, '</b> <br/>') #,
        #'Suburbs:', sites$suburb)
        #
        leafletProxy("myMap2") %>% clearMarkers() %>%
                  addMarkers(lng = sites$lon,
                             lat = sites$lat,
                             popup = htmlContent,
                             clusterOptions = markerClusterOptions())
    })

}


