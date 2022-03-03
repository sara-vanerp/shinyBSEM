## UI definition Shiny BSEM 

ui <- navbarPage("Bayesian SEM",
                 tabPanel("Estimate",
                          sidebarLayout(
                            sidebarPanel(
                              # own data or example
                              radioButtons("example", 
                                           p("Would you like to estimate your own model or work with the", a("Political Democracy", href = "https://rdrr.io/cran/lavaan/man/PoliticalDemocracy.html", "example?")),
                                           choices = c("Example data",
                                                       "Own data"),
                                           selected = "Example data"),
                              
                              uiOutput("datachoice"),
                              tags$hr(),
                              
                              numericInput("burnin",
                                           "Specify the number of iterations to take as burnin",
                                           value = 500),
                              numericInput("iter", 
                                           "Specify the number of iterations to take after burnin",
                                           value = 1000), 
                              numericInput("chains", 
                                           "Specify the number of desired MCMC chains",
                                           4),
                              tags$hr(),
                              actionButton("estMod", "Estimate the model"),
                              uiOutput("download")
                            ),
                            mainPanel(
                              # path diagram
                              plotlyOutput("mod.plot"),
                              br(),
                              # print warning if the "estimate" button is clicked but not all priors are specified
                              textOutput("estWarn"),
                              br(),
                              # print warning if defaults are used
                              htmlOutput("defaultWarn"),
                              br(),
                              # plot of the specified prior
                              uiOutput("priorOutput"),
                              br(),
                              # show message when done
                              textOutput("fitCompl"),
                              br(),
                              # chosen prior settings
                              dataTableOutput("priorVals")
                            )
                          )
                          ),
                 navbarMenu("Output",
                            tabPanel("Convergence",
                                     textOutput("convWarn1"),
                                     dataTableOutput("convInfo")),
                            tabPanel("Summary",
                                     textOutput("convWarn2"),
                                     textOutput("ppp"),
                                     dataTableOutput("fitSummary")),
                            tabPanel("Visualisation",
                                     plotlyOutput("mod.plot.est"))),
                 #tabPanel("Prior sensitivity checks"),
                 navbarMenu("More",
                            tabPanel("References"),
                            tabPanel("About"))
                 )

