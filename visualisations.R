# packages
library(ggplot2)
library(scales)
library(leaflet)
library(shiny)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
library(reshape)
library(markdown)


#
source('shinyUI.R')
source('shinyServer.R')

# Plot income vs age
plotIncomeVsAge <- function(dt) {
    # first 2:23 contains [income bands, age...]
    mydt <- dt[, 2:23]
    # normalise age columns (exclude weeklyIncome)  , add 'delta' to avoid div by zero error
    xx <- mydt[, names(mydt)[2:22] := lapply(.SD,
                                            function(x)(as.numeric(x) / (sum(as.numeric(x), na.rm = TRUE) + 0.0000001))),
                                            .SDcols = 2:22]
    ## find concetrations of population in each income category
    # perform colSums for each income category vs each age category
    concents <- mydt[, lapply(.SD, function(x)(sum(x, na.rm = TRUE))), by = 'weeklyIncome', .SDcols = 2:22]
    # perform rowSums for each income category
    tots <- rowSums(concents[, 2:22], na.rm = TRUE)
    # convert to percent
    pcnts <- (tots / sum(tots, na.rm = TRUE)) * 100
    tmpdt <- data.table(weeklyIncome = unique(mydt$weeklyIncome), tots = tots, pcnts = pcnts)
    #
    mydt.melt <- melt(mydt, id.vars = 'weeklyIncome')
    # plot bar chart
    plt1 <- ggplot(data = mydt.melt,
                   aes_string(x = 'weeklyIncome', y = 'value', fill = 'variable')) +
                   geom_bar(stat = 'identity', width = 0.5) +
                   scale_fill_manual(values = colorRampPalette(brewer.pal(8, 'Accent'))(length(unique(mydt.melt$variable)))) +
                   scale_y_continuous(name = '% of Total Individuals') +
                   scale_x_discrete(name = 'Individual weekly income') +
                   guides(fill = guide_legend(title = 'Age',
                                              nrow = length(unique(mydt.melt$variable)))) +
                   geom_text(data = tmpdt, aes(x = weeklyIncome,
                                               y = tots,
                                               label = paste(format(pcnts, digits = 2), '%')),
                                          size = 3,
                                          vjust = -1,
                                          inherit.aes = FALSE) +
                   theme(axis.text.x = element_text(angle = 60, hjust = 1),
                         axis.text.y = element_blank())
                   #geom_text(aes(y = cumsum), vjust = -0.5, position = position_dodge(0.9))   # add percentages on top of bars

    show(plt1)
}

# Plot income vs age
plotIncomeVsIndustry <- function(dt) {
    # first 2:23 contains [income bands, industry...]
    mydt <- cbind(dt[, 2], dt[, 25:46])
    # normalise age columns (exclude weeklyIncome)  , add 'delta' to avoid div by zero error
    xx <- mydt[, names(mydt)[2:23] := lapply(.SD,
                                            function(x)(as.numeric(x) / (sum(as.numeric(x), na.rm = TRUE) + 0.0000001))),
                                            .SDcols = 2:23]

    ## find concetrations of population in each income category
    # perform colSums for each income category vs each industry category
    concents <- mydt[, lapply(.SD, function(x)(sum(x, na.rm = TRUE))), by = 'weeklyIncome', .SDcols = 2:23]
    # perform rowSums for each income category
    tots <- rowSums(concents[, 2:23], na.rm = TRUE)
    # convert to percent
    pcnts <- (tots / sum(tots, na.rm = TRUE)) * 100
    #
    tmpdt <- data.table(weeklyIncome = unique(mydt$weeklyIncome), tots = tots, pcnts = pcnts)

    #
    mydt.melt <- melt(mydt, id.vars = 'weeklyIncome')
    # plot bar chart
    plt1 <- ggplot(data = mydt.melt,
                   aes_string(x = 'weeklyIncome', y = 'value', fill = 'variable')) +
                   geom_bar(stat = 'identity', width = 0.5) +
                   scale_fill_manual(values = colorRampPalette(brewer.pal(8, 'Accent'))(length(unique(mydt.melt$variable)))) +
                   scale_y_continuous(name = '% of Total Individuals') +
                   scale_x_discrete(name = 'Individual weekly income') +
                   guides(fill = guide_legend(title = 'Industry',
                                              nrow = length(unique(mydt.melt$variable)))) +
                   geom_text(data = tmpdt, aes(x = weeklyIncome,
                                               y = tots,
                                               label = paste(format(pcnts, digits = 2), '%')),
                                              size = 3,
                                              vjust = -1,
                                              inherit.aes = FALSE) +
                   theme(axis.text.x = element_text(angle = 60, hjust = 1),
                         axis.text.y = element_blank())
    #geom_text(aes(y = cumsum), vjust = -0.5, position = position_dodge(0.9))   # add percentages on top of bars

    show(plt1)
}


# Plot household income vs household composition
plotHHIncomeVsComposition <- function(dt) {
    # exclude postcode column
    excludeList <- c('postcode', 'state', 'suburb', 'lat', 'lon')
    mydt <- dt[, -c(excludeList), with = FALSE]
    # normalise age columns (exclude weeklyIncome)  , add 'delta' to avoid div by zero error
    xx <- mydt[, names(mydt)[2:5] := lapply(.SD,
                                            function(x)(as.numeric(x) / (sum(as.numeric(x), na.rm = TRUE) + 0.0000001))),
                                            .SDcols = 2:5]

    ## find concetrations of population in each income category
    # perform colSums for each income category vs each industry category
    concents <- mydt[, lapply(.SD, function(x)(sum(x, na.rm = TRUE))), by = 'weeklyIncome', .SDcols = 2:5]
    # perform rowSums for each income category
    tots <- rowSums(concents[, 2:5], na.rm = TRUE)
    # convert to percent
    pcnts <- (tots / sum(tots, na.rm = TRUE)) * 100
    #
    tmpdt <- data.table(weeklyIncome = unique(mydt$weeklyIncome), tots = tots, pcnts = pcnts)

    #
    mydt.melt <- melt(mydt, id.vars = 'weeklyIncome')
    # plot bar chart
    plt1 <- ggplot(data = mydt.melt,
                   aes_string(x = 'weeklyIncome', y = 'value', fill = 'variable')) +
                   geom_bar(stat = 'identity', width = 0.5) +
                   scale_fill_manual(values = colorRampPalette(brewer.pal(8, 'Accent'))(length(unique(mydt.melt$variable)))) +
                   scale_y_continuous(name = '% of Total Households') +
                   scale_x_discrete(name = 'Household weekly income') +
                   guides(fill = guide_legend(title = 'Household Composition',
                                              nrow = length(unique(mydt.melt$variable)))) +
                   geom_text(data = tmpdt, aes(x = weeklyIncome,
                                               y = tots,
                                               label = paste(format(pcnts, digits = 2), '%')),
                                              size = 3,
                                              vjust = -1,
                                              inherit.aes = FALSE) +
                   theme(axis.text.x = element_text(angle = 60, hjust = 1),
                         axis.text.y = element_blank())
    #geom_text(aes(y = cumsum), vjust = -0.5, position = position_dodge(0.9))   # add percentages on top of bars

    show(plt1)
}


# Plot household income vs mortgage repayments
plotHHIncomeVsMortRepay <- function(dt) {
    # exclude postcode column
    excludeList <- c('postcode', 'state', 'suburb', 'lat', 'lon')
    mydt <- dt[, - c(excludeList), with = FALSE]
    # now the datatable will have [ mortRepay, HH income bands..]
    ##
    # normalise weeklyIncome columns (exclude mortRepay)  , add 'delta' to avoid div by zero error
    xx <- setDT(mydt)[, names(mydt)[2:26] := lapply(.SD,
                                            function(x)(as.numeric(x) / (sum(as.numeric(x), na.rm = TRUE) + 0.0000001))),
                                            .SDcols = 2:26]

    ## find concetrations of population in each mortRepay category
    # perform colSums for each mortRepaycategory vs each income category
    concents <- mydt[, lapply(.SD, function(x)(sum(x, na.rm = TRUE))), by = 'mortRepay', .SDcols = 2:26]
    # perform rowSums for each mortRepay category
    tots <- rowSums(concents[, 2:26], na.rm = TRUE)
    # convert to percent
    pcnts <- (tots / sum(tots, na.rm = TRUE)) * 100
    #
    tmpdt <- data.table(mortRepay = unique(mydt$mortRepay), tots = tots, pcnts = pcnts)

    #
    mydt.melt <- melt(mydt, id.vars = 'mortRepay')
    # plot bar chart
    plt1 <- ggplot(data = mydt.melt,
                   aes_string(x = 'mortRepay', y = 'value', fill = 'variable')) +
                   geom_bar(stat = 'identity', width = 0.5) +
                   scale_fill_manual(values = colorRampPalette(brewer.pal(8, 'Accent'))(length(unique(mydt.melt$variable)))) +
                   scale_y_continuous(name = '% of Total Households') +
                   scale_x_discrete(name = 'Mortgage Repayments') +
                   guides(fill = guide_legend(title = 'Household Incomes',
                                              nrow = length(unique(mydt.melt$variable)))) +
                   geom_text(data = tmpdt, aes(x = mortRepay,
                                               y = tots,
                                               label = paste(format(pcnts, digits = 2), '%')),
                                              size = 3,
                                              vjust = -1,
                                              inherit.aes = FALSE) +
                   theme(axis.text.x = element_text(angle = 60, hjust = 1),
                         axis.text.y = element_blank())
    #geom_text(aes(y = cumsum), vjust = -0.5, position = position_dodge(0.9))   # add percentages on top of bars

    show(plt1)
}


# Plot household income vs rent payments
plotHHIncomeVsRent <- function(dt) {
    # exclude postcode column
    excludeList <- c('postcode', 'state', 'suburb', 'lat', 'lon')
    mydt <- dt[, - c(excludeList), with = FALSE]
    # now the datatable will have [ rent, HH income bands..]
    ##
    # normalise age columns (exclude weeklyIncome)  , add 'delta' to avoid div by zero error
    xx <- setDT(mydt)[, names(mydt)[2:26] := lapply(.SD,
                                            function(x)(as.numeric(x) / (sum(as.numeric(x), na.rm = TRUE) + 0.0000001))),
                                            .SDcols = 2:26]

    ## find concetrations of population in each rent category
    # perform colSums for each rent category vs each income category
    concents <- mydt[, lapply(.SD, function(x)(sum(x, na.rm = TRUE))), by = 'rent', .SDcols = 2:26]
    # perform rowSums for each rent category
    tots <- rowSums(concents[, 2:26], na.rm = TRUE)
    # convert to percent
    pcnts <- (tots / sum(tots, na.rm = TRUE)) * 100
    #
    tmpdt <- data.table(rent = unique(mydt$rent), tots = tots, pcnts = pcnts)

    #
    mydt.melt <- melt(mydt, id.vars = 'rent')
    # plot bar chart
    plt1 <- ggplot(data = mydt.melt,
                   aes_string(x = 'rent', y = 'value', fill = 'variable')) +
                   geom_bar(stat = 'identity', width = 0.5) +
                   scale_fill_manual(values = colorRampPalette(brewer.pal(8, 'Accent'))(length(unique(mydt.melt$variable)))) +
                   scale_y_continuous(name = '% of Total Households') +
                   scale_x_discrete(name = 'Rent Payments') +
                   guides(fill = guide_legend(title = 'Household Incomes',
                                              nrow = length(unique(mydt.melt$variable)))) +
                   geom_text(data = tmpdt, aes(x = rent,
                                               y = tots,
                                               label = paste(format(pcnts, digits = 2), '%')),
                                              size = 3,
                                              vjust = -1,
                                              inherit.aes = FALSE) +
                   theme(axis.text.x = element_text(angle = 60, hjust = 1),
                         axis.text.y = element_blank())
    #geom_text(aes(y = cumsum), vjust = -0.5, position = position_dodge(0.9))   # add percentages on top of bars

    show(plt1)
}



# create a leaflet container and set it to Sydney
#
# var sydney = new L.LatLng(-33.91, 151.08);
# ref: https://github.com/henrythasler/Leaflet.Geodesic/blob/master/example/simple.html
#
initLeaflet <- function() {
    return(leaflet() %>%
           addTiles() %>%
           setView(lng = 151.08,
                   lat = -33.91,
                   zoom = 11))
}


# ShinyApp
#shinyApp(ui = fluidPage(titlePanel("So Simple"),
#                        sliderInput("slider", "Turn It Up", min = 0, max = 11, value = 5),
#                        mainPanel(textOutput("MainPanel"))),
#         server = function(input, output) {
#             output$MainPanel = renderText(input$slider)
#         }
#           )

shinyApp(
  ui = ui,
  server = server,
  options = list(height = 600)
)

