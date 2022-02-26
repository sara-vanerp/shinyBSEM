## UI definition Shiny BSEM 

ui <- navbarPage("Bayesian SEM",
                 tabPanel("Estimate",
                          sidebarLayout(
                            sidebarPanel(
                              # upload data
                              fileInput("file", "Upload your data file (CSV), including variable names.",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                              ),
                              radioButtons("sep", "Which character separates different fields?",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),
                              
                              tags$hr(),
                              # specify model
                              textAreaInput("model", p("Specify the model using", a("lavaan syntax.", href = "https://lavaan.ugent.be/index.html")),
                                            value = "# latent variable definitions
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8 ",
                                            rows = 15),
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
                              # print warning if defaults are used
                              textOutput("defaultWarn"),
                              # use blavaan default priors
                              actionButton("defaultPriors", 
                                           "Use the blavaan default priors"),
                              # print warning if the "estimate" button is clicked but not all priors are specified
                              textOutput("estWarn"),
                              # show message when done
                              textOutput("fitCompl"),
                              # plot of the specified prior
                              uiOutput("priorOutput"),
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

