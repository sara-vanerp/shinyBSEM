## UI definition Shiny BSEM 

ui <- fluidPage(
  
  titlePanel(HTML(paste(h1("Bayesian SEM"), h5("Developed by:", a("Sara van Erp", href = "https://saravanerp.com") )))),
  
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
      
      numericInput("iter", 
                   "Specify the number of iterations to take after burnin",
                   value = 1000), 
      
      numericInput("chains", 
                   "Specify the number of desired MCMC chains",
                   4),
      
      actionButton("estMod", "Estimate the model"),
      
      uiOutput("download")
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Priors", 
                           
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
                           tableOutput("priorVals")
                           
                  ),
                  
                  tabPanel("Output",
                           
                           # summary
                           tableOutput("fitSummary")
                           
                  ),
                  tabPanel("Prior sensitivity checks")
      )
      
      
    )
  )
)