#
#  This is the UI for shiny app
#



ui <- function() {
    

    navbarPage("ABS Census 2016", id = "nav",

                # About
                tabPanel('About',
                         div(class = 'outer',
                            tags$head(
                                #  Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              absolutePanel(id='controlsText', class = "panel panel-default", fixed = TRUE,
                                           draggable = FALSE, top = 60, left = 200, right = 200, bottom = 200,
                                           width = 1200, height = 750,
                                            includeMarkdown('aboutText.md')
                              ),

                            tags$div(id = "cite",
                                 'Data compiled from ', tags$em('Australian Bureau of Statistics, Census 2016'), ' by VirusMe and The Portfolio Trader.'
                             ))
                ),

                # Individual incomes
                tabPanel("Individual incomes",
                         div(class = "outer",
                             tags$head(
                               #  Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                             # If not using custom CSS, set height of leafletOutput to a number instead of percent
                             leafletOutput("myMap", width = "100%", height = "100%"),

                             # Shiny versions prior to 0.11 should use class = "modal" instead.
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 1200, height = "auto",
                                            # Individual incomes
                                           navbarPage('Individual Weekly Incomes vs', id = 'innerNav',
                                                       tabPanel("Age",
                                                                 div(class = 'inner',
                                                                    tags$head(
                                                                                #  Include our custom CSS
                                                                               includeCSS("styles.css")
                                                                              ),
                                                                     plotOutput("plotIncomeVsAge", height = 650)
                                                                     )
                                                               ),
                                                       tabPanel("Industry",
                                                                 div(class = 'inner',
                                                                    tags$head(
                                                                                #  Include our custom CSS
                                                                               includeCSS("styles.css")
                                                                              ),
                                                                     plotOutput("plotIncomeVsIndustry", height = 650)
                                                                     )
                                                                ),
                                                        conditionalPanel("false", icon("crosshair"))
                                           )
                                           
                             ),

                             tags$div(id = "cite",
                                 'Data compiled from ', tags$em('Australian Bureau of Statistics, Census 2016'), ' by VirusMe and The Portfolio Trader.'
                        )
                       )
                     ),

                # Household Incomes
                tabPanel("Household incomes",
                         div(class = "outer",
                             tags$head(
                                #  Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                             # If not using custom CSS, set height of leafletOutput to a number instead of percent
                             leafletOutput("myMap2", width = "100%", height = "100%"),

                            # Shiny versions prior to 0.11 should use class = "modal" instead.
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 1200, height = "auto",
                                            navbarPage('Household incomes vs', id = 'innerNav',
                                                       tabPanel("Household Composition",
                                                                 div(class = 'inner',
                                                                    tags$head(
                                                                                #  Include our custom CSS
                                                                               includeCSS("styles.css")
                                                                              ),
                                                                     plotOutput("plotHHIncomeVsComposition", height = 650)
                                                                     )
                                                               ),

                                                       tabPanel("Mortgage Repayments",
                                                                 div(class = 'inner',
                                                                    tags$head(
                                                                               #  Include our custom CSS
                                                                               includeCSS("styles.css")
                                                                              ),
                                                                    plotOutput("plotHHIncomeVsMortRepay", height = 650)
                                                                     )
                                                               ),
                                                       tabPanel("Rent Payments",
                                                                 div(class = 'inner',
                                                                    tags$head(
                                                                             #  Include our custom CSS
                                                                               includeCSS("styles.css")
                                                                              ),
                                                                    plotOutput("plotHHIncomeVsRent", height = 650)
                                                                     )
                                                               ),
                                                     
                                                        conditionalPanel("false", icon("crosshair"))
                                            )

                             ),

                             tags$div(id = "cite",
                                 'Data compiled from ', tags$em('Australian Bureau of Statistics, Census 2016'), ' by VirusMe and The Portfolio Trader.'
                        )
                       )
                     ),

               conditionalPanel("false", icon("crosshair"))
          )


}
