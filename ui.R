

source("packages.R")


ui <- fluidPage(
    theme = shinytheme("cosmo"),

    navbarPage(title = "Predicting NBA results with Machine Learning",
               header = tagList(
                                    useShinydashboard(),
                                    useSweetAlert(),
                                    tags$head(tags$style("
                                                         .jhr{
                                                         display: inline;
                                                         vertical-align: middle;
                                                         padding-left: 10px;}"))),


               tabPanel("Introduction", column(10, offset = 1,
                                               HTML("<p style='text-align: center;'><span style='font-size: 46px;'>WELCOME!</span></p>"),
                                               tags$img(src = "https://i.pinimg.com/originals/d3/1a/ee/d31aeeee6fa31d3209f48cdc9e557c31.gif",
                                                        style="display: block; margin-left: auto; margin-right: auto;",
                                                        height = "30%", width = "30%"), br(),
                                               includeMarkdown("intro.md"))


                        ),

               tabPanel("Daily Schedule",
                        sidebarLayout(
                            sidebarPanel(dateInput("date", "Select a date", value = "2021-03-31", min = min(schedule$Date), max = max(schedule$Date)), width = 3),
                            mainPanel(h1("Daily NBA schedule"),
                                      uiOutput("game1"),
                                      uiOutput("game2"),
                                      uiOutput("game3"),
                                      uiOutput("game4"),
                                      uiOutput("game5"),
                                      uiOutput("game6"),
                                      uiOutput("game7"),
                                      uiOutput("game8"),
                                      uiOutput("game9"),
                                      uiOutput("game10"),
                                      uiOutput("game11"),
                                      uiOutput("game12"),
                                      uiOutput("game13"),
                                      uiOutput("game14"),
                                      uiOutput("game15"),
                                      width = 9)
                        )


                        ),

               tabPanel("Predictions",

                        tabsetPanel(tabPanel("Model 1 (LogReg)",
                                             uiOutput("textfardate"),
                                             uiOutput("game1predLR"),
                                             uiOutput("game2predLR"),
                                             uiOutput("game3predLR"),
                                             uiOutput("game4predLR"),
                                             uiOutput("game5predLR"),
                                             uiOutput("game6predLR"),
                                             uiOutput("game7predLR"),
                                             uiOutput("game8predLR"),
                                             uiOutput("game9predLR"),
                                             uiOutput("game10predLR"),
                                             uiOutput("game11predLR"),
                                             uiOutput("game12predLR"),
                                             uiOutput("game13predLR"),
                                             uiOutput("game14predLR"),
                                             uiOutput("game15predLR")


                                            ),

                                   tabPanel("Model 2 (SVM)",
                                            uiOutput("textfardate2"),
                                             uiOutput("game1predSVM"),
                                             uiOutput("game2predSVM"),
                                             uiOutput("game3predSVM"),
                                             uiOutput("game4predSVM"),
                                             uiOutput("game5predSVM"),
                                             uiOutput("game6predSVM"),
                                             uiOutput("game7predSVM"),
                                             uiOutput("game8predSVM"),
                                             uiOutput("game9predSVM"),
                                             uiOutput("game10predSVM"),
                                             uiOutput("game11predSVM"),
                                             uiOutput("game12predSVM"),
                                             uiOutput("game13predSVM"),
                                             uiOutput("game14predSVM"),
                                             uiOutput("game15predSVM")


                                            )

                                   )),

               tabPanel("Beat the Odds",
                        tabsetPanel(tabPanel("Table", dataTableOutput("tableodds")),
                                    tabPanel("Plots", br(), column(6, plotlyOutput("plot1")),
                                             column(6, plotlyOutput("plot2")))



                        )

               )


))
