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
            # path diagram
            plotlyOutput("mod.plot"),
            
            # plot of the specified prior
            uiOutput("priorOutput")
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
    
    # set prior hyperparameters and plot
    # see http://ecmerkle.github.io/blavaan/articles/prior.html for the defaults
    
    output$priorOutput <- renderUI({
        if(is.null(click_data())){
            print("Please select a parameter in the model by clicking on one of the labels")
        } else if(grepl("l", click_data()) == TRUE){ # loadings
            tagList(
                numericInput("priorMeanInputLoad", 
                             paste0("Mean for the normal prior on the loading ", click_data()),
                             value = 0, min = -100, max = 100, step = 1),
                numericInput("priorScaleInputLoad", 
                             paste0("Standard deviation for the normal prior on the loading ", click_data()),
                             value = 10, min = 0, max = 100, step = 1),
                
                actionButton("fixLoadPrior", "Set this prior"),
                
                renderPlot({
                    ggplot(data = data.frame(x = c(input$priorMeanInputLoad - 5*input$priorScaleInputLoad, input$priorMeanInputLoad + 5*input$priorScaleInputLoad)), aes(x)) + 
                        stat_function(fun = dnorm, n = 101, args = list(mean = input$priorMeanInputLoad, sd = input$priorScaleInputLoad)) + 
                        ylab("") + xlab(paste0("Prior for the loading ", click_data())) +
                        scale_y_continuous(breaks = NULL) + theme_bw() + 
                        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                              text = element_text(size = 20))
                })
            )
        } else if(grepl("v", click_data()) == TRUE){ # variances
            tagList(
                selectInput("priorTypeInputVar", 
                            paste0("Specify the gamma prior for the variable ", click_data(), " on the:"),
                            c("standard deviation", "variance", "precision"), selected = "standard deviation"),
                numericInput("priorShapeInputVar", "Shape parameter for the gamma prior",
                             value = 1, min = 0, max = 100, step = 1),
                numericInput("priorRateInputVar", "Rate parameter for the gamma prior",
                             value = 0.5, min = 0, max = 100, step = 1),
                actionButton("fixVarPrior", "Set this prior"),
                
                renderPlot({
                    ggplot(data = data.frame(x = c(0, input$priorShapeInputVar/input$priorRateInputVar + 5*(input$priorShapeInputVar/input$priorRateInputVar^2))), aes(x)) + 
                        stat_function(fun = dgamma, n = 101, args = list(shape = input$priorShapeInputVar, rate = input$priorRateInputVar)) + 
                        ylab("") + xlab(paste0("Prior for the ", input$priorTypeInputVar, " ", click_data())) +
                        scale_y_continuous(breaks = NULL) + theme_bw() + 
                        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                              text = element_text(size = 20))
                })
            )
        } else if(grepl("r", click_data()) == TRUE){ # correlations
            tagList(
                numericInput("priorShapeInputCorr1", 
                             paste0("Shape parameter 1 for the beta prior on the correlation ", click_data()),
                             value = 1, min = 0, max = 100, step = 1),
                numericInput("priorShapeInputCorr2", 
                             paste0("Shape parameter 2 for the beta prior on the correlation ", click_data()),
                             value = 1, min = 0, max = 100, step = 1),
                actionButton("fixCorrPrior", "Set this prior"),
                
                renderPlot({
                    ggplot(data = data.frame(x = c(0, 1)), aes(x)) + 
                        stat_function(fun = dbeta, n = 101, args = list(shape1 = input$priorShapeInputCorr1, shape2 = input$priorShapeInputCorr2)) + 
                        ylab("") + xlab(paste0("Prior for the correlation ", click_data())) +
                        scale_y_continuous(breaks = NULL) + theme_bw() + 
                        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                              text = element_text(size = 20))
                })
            )
        } else if(grepl("b", click_data()) == TRUE){ # structural regression parameters
            tagList(
                numericInput("priorMeanInputRegr", 
                             paste0("Mean for the normal prior on the regression parameter ", click_data()),
                             value = 0, min = -100, max = 100, step = 1),
                numericInput("priorScaleInputRegr", 
                             paste0("Standard deviation for the normal prior on the regression parameter ", click_data()),
                             value = 10, min = 0, max = 100, step = 1),
                actionButton("fixRegrPrior", "Set this prior"),
                
                renderPlot({
                    ggplot(data = data.frame(x = c(input$priorMeanInputRegr - 5*input$priorScaleInputRegr, input$priorMeanInputRegr + 5*input$priorScaleInputRegr)), aes(x)) + 
                        stat_function(fun = dnorm, n = 101, args = list(mean = input$priorMeanInputRegr, sd = input$priorScaleInputRegr)) + 
                        ylab("") + xlab(paste0("Prior for the structural regression parameter ", click_data())) +
                        scale_y_continuous(breaks = NULL) + theme_bw() + 
                        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                              text = element_text(size = 20))
                })
            )
        }
    })
    
}

shinyApp(ui = ui, server = server)
