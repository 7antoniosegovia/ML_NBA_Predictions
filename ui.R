

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
                                             withLoader(uiOutput("game1predLR"), type = "image", loader = "loading.gif"),
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
                                             withLoader(uiOutput("game1predSVM"), type = "image", loader = "loading.gif"),
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
                                    tabPanel("Plots", br(), column(6, plotlyOutput("plot1"), column(7, offset = 3, br(), valueBoxOutput("vb1", width = 12))),
                                             column(6, plotlyOutput("plot2"), column(7, offset = 3, br(), valueBoxOutput("vb2", width = 12))))



                        )

               ),

               tabPanel("Results",
                        tabsetPanel(
                            tabPanel("Recap", br(),
                                     HTML('<p style="text-align: justify;">The regular season has come to an end, which means that the testing of our models is over! And surprisingly, we have obtained very good results. As we mentioned in the introduction, the best models in this field have achieved an accuracy of at most 70%. Our Logistic Regression model had an accuracy of <strong>68.57%</strong> and we achieved a stunning <strong>72.47%</strong> with the Support Vector Machine.</p>
<p style="text-align: justify;">Speaking about earnings, the results are quite positive as well. Both models had difficulties to get going, and 100 games into the test, both fell below the starting point of 10.000 betting points. From that point on, the trend was overall positive, and the short negative streaks were fastly overcome by great runs. Curiously, the model with lower accuracy, the Logistic Regression, was the one that obtained best betting results, with a net gain of <strong>4721 points</strong>, while the SVM only obtained <strong>3786 points</strong>, which highlights the importance of a <strong>good betting strategy</strong>.&nbsp;</p>
<p style="text-align: justify;">In my opinion, the main reason behind these better results is the low number of non-important games at the end of the season. There were lots of games with seeding implications both for the first seeds and for the play-in tournament, which translated in less teams resting their main players and a lower upset ratio. We can see in the league standings, how there are quite a few teams with similar records.&nbsp;</p>'),

fluidRow(column(6, img(src = "este.PNG", align = "right")),
         column(6, img(src = "oeste.PNG", align = "center"))), br(),

HTML('<p style="text-align: justify;">The upset ratio is the proportion of games won by the underdog. Usually, it takes values of up to 32%, but the ratio for these games was just 29.35%, which made the games somewhat easier to predict. Even though, keeping in mind that it was just my first approach to working with these model and this problem, I am very happy with the results obtained. You will find in the next tabs of this page a short analysis of the results, finding out which model was more risk tolerant, or which team was easier to predict. Make sure to take a look at it!&nbsp;</p>
<p style="text-align: justify;">Finally, I just wanted to add that this has been a nice journey, and I was overwhelmed by the support this received when I first uploaded it to LinkedIn. If you get to this point, thank you for your support! I hope you enjoyed it!</p>')

                                             ),
tabPanel("Stats", br(),
         box(title = "Summary: Accuracies, earnings, ratios",
             status = "danger",
             solidHeader = T,
             collapsible = T,
             width = 12,
             column(4, valueBoxOutput("vboxresults1", width = 12),
                    valueBoxOutput("vboxresults2", width = 12)),
             column(4, valueBoxOutput("vboxresults3", width = 12),
                    valueBoxOutput("vboxresults4", width = 12)),
             column(4, valueBoxOutput("vboxresults5", width = 12),
                    valueBoxOutput("vboxresults6", width = 12))), br(),

         box(title = "Daily stats - Logistic regression",
             status = "info",
             solidHeader = T,
             collapsible = T,
             width = 12,
             column(6, plotlyOutput("dailyaccslr"), br(),
                    column(6, valueBoxOutput("dailyvboxLR1", width = 12)),
                    column(6, valueBoxOutput("dailyvboxLR2", width = 12))),
             column(6, plotlyOutput("dailyearningslr"), br(),
                    column(6, valueBoxOutput("dailyvboxLR3", width = 12)),
                    column(6, valueBoxOutput("dailyvboxLR4", width = 12)))),

         box(title = "Daily stats - Support Vector Machine",
             status = "primary",
             solidHeader = T,
             collapsible = T,
             width = 12,
             column(6, plotlyOutput("dailyaccssvm"), br(),
                    column(6, valueBoxOutput("dailyvboxSVM1", width = 12)),
                    column(6, valueBoxOutput("dailyvboxSVM2", width = 12))),
             column(6, plotlyOutput("dailyearningssvm"), br(),
                    column(6, valueBoxOutput("dailyvboxSVM3", width = 12)),
                    column(6, valueBoxOutput("dailyvboxSVM4", width = 12))))

),

tabPanel("Betting", br(),
         fluidRow(box(title = "Logistic regression betting strategy",
             status = "primary",
             solidHeader = T,
             collapsible = T,
             width = 9,
             column(6, plotlyOutput("betstrat1")),
             column(6, plotlyOutput("betstrat2"))),

             column(3,
                box(title = "Best odds hit",
             status = "danger",
             solidHeader = T,
             collapsible = T,
             width = NULL,
             valueBoxOutput("betvbox13", width = 12),
             valueBoxOutput("betvbox14", width = 12)))),

         column(6,
                box(title = "Betting on the underdog",
             status = "primary",
             solidHeader = T,
             collapsible = T,
             width = NULL,
             column(6, valueBoxOutput("betvbox1", width = 12), valueBoxOutput("betvbox2", width = 12)),
             column(6, valueBoxOutput("betvbox3", width = 12), valueBoxOutput("betvbox4", width = 12)))),
         column(6,
                box(title = "Odds > 2.5",
             status = "danger",
             solidHeader = T,
             collapsible = T,
             width = NULL,
             column(6, valueBoxOutput("betvbox5", width = 12), valueBoxOutput("betvbox6", width = 12)),
             column(6, valueBoxOutput("betvbox7", width = 12), valueBoxOutput("betvbox8", width = 12)))),
         column(6,
                box(title = "Odds > 3",
             status = "danger",
             solidHeader = T,
             collapsible = T,
             collapsed = T,
             width = NULL,
             column(6, valueBoxOutput("betvbox9", width = 12), valueBoxOutput("betvbox10", width = 12)),
             column(6, valueBoxOutput("betvbox11", width = 12), valueBoxOutput("betvbox12", width = 12)))),

             column(6,
                box(title = "Odds > 4",
             status = "primary",
             solidHeader = T,
             collapsible = T,
             collapsed = T,
             width = NULL,
             column(6, valueBoxOutput("betvbox15", width = 12), valueBoxOutput("betvbox16", width = 12)),
             column(6, valueBoxOutput("betvbox17", width = 12), valueBoxOutput("betvbox18", width = 12))))),

tabPanel("Teams",

         box(title = "Logistic regression",
             status = "primary",
             solidHeader = T,
             collapsible = T,
             width = 12,
             column(6, plotlyOutput("teamsplot1")),
             column(6, plotlyOutput("teamsplot2"))),


         box(title = "Support Vector Machine",
             status = "danger",
             solidHeader = T,
             collapsible = T,
             width = 12,
             column(6, plotlyOutput("teamsplot3")),
             column(6, plotlyOutput("teamsplot4"))))))



))
