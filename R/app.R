##### Shiny Bayesian SEM #####
## Author: Sara van Erp

library(shiny)
library(lavaan)
library(tidySEM)
library(plotly)

source("./functions_shinyBSEM.R")

ui <- fluidPage(

    titlePanel("Bayesian SEM"),
    
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
                          value = "
                          # latent variable definitions
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
        ),
            
        mainPanel(
            plotlyOutput("mod.plot"),
                
            verbatimTextOutput("clicks")
        )
    )
)

server <- function(input, output) {
    
    # read data file
    dat <- reactive({
        req(input$file)
        
        read.csv(input$file$datapath,
                 sep = input$sep)
    })
    
    # add labels to the model and fit using lavaan
    fit <- reactive({
        req(input$model)
        
        mod.lbl <- label_syntax_fun(toString(input$model))
        #mod.fit <- sem(mod.lbl, data = dat())
        mod.fit <- sem(mod.lbl, data = PoliticalDemocracy) #TODO: use input data instead of placeholder
    })
    
    # plot the model
    output$mod.plot <- renderPlotly({
        ggplotly(plot_fun(fit()), tooltip = "text")
    })
    
    click_data <- reactive({
        click <- event_data("plotly_click")$customdata
    })
    
    output$clicks <- renderPrint({
        data.frame(click_data())
    })
}

shinyApp(ui = ui, server = server)
