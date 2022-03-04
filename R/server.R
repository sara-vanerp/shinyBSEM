## Server definition Shiny BSEM 

server <- function(input, output) {
  
  output$datachoice <- renderUI({
    
    if(input$example == "Own data"){
      
      tagList(
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
                      value = "",
                      rows = 15),
        # needed to avoid label_syntax_fun trying to parse empty lavaan model (although this works in shinySEM app)
        actionButton("plotModel", 
                     "Plot the model")
        
      )
    } else if(input$example == "Example data"){
      
      tagList(
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
        
        # needed to avoid label_syntax_fun trying to parse empty lavaan model (although this works in shinySEM app)
        actionButton("plotModel", 
                     "Plot the model")
        
      )
    }
    
  })
  
  # read data file
  dat <- reactive({
    req(input$file)
    
    read.csv(input$file$datapath,
             sep = input$sep)
  })
  
  # add labels to the model and fit using lavaan
  fit <- reactive({
    req(input$plotModel)
    mod.lbl <- label_syntax_fun(toString(input$model), meanstructure = FALSE)
    
    if(input$example == "Own data"){
      mod.fit <- sem(mod.lbl, data = dat())
    } else if(input$example == "Example data"){
      mod.fit <- sem(mod.lbl, data = PoliticalDemocracy)
    }
    
    outFit <- list(modLbl = mod.lbl, modFit = mod.fit)
    return(outFit)
  })
  
  # plot the model
  output$mod.plot <- renderPlotly({
    req(input$plotModel)
    ggplotly(plot_fun(fit()$modFit, est = FALSE), tooltip = "text")
  })
  
  click_data <- reactive({
    click <- event_data("plotly_click")$customdata
  })
  
  output$clicks <- renderPrint({
    data.frame(click_data())
  })
  
  # set prior hyperparameters and plot
  # see http://ecmerkle.github.io/blavaan/articles/prior.html for the defaults
  
  # return specified priors
  priors <- reactiveValues()
  
  observeEvent(input$plotModel, {
    priors$df <- do.call(rbind, lapply(fit()$modLbl$label, init_df))
  })
  
  output$priorOutput <- renderUI({
    req(input$plotModel)
    if(is.null(click_data())){
      tagList(
        renderUI(HTML(paste(em("Please select a parameter in the model by clicking on one of the labels or use the blavaan default priors.")))),
        # renderText("Please select a parameter in the model by clicking on one of the labels or use the blavaan default priors."),
        br(),
      # use blavaan default priors
      actionButton("defaultPriors", 
                   "Use the blavaan default priors"),
      )
    } else if(grepl("l", click_data()) == TRUE){ # loadings
      tagList(
        numericInput("priorMeanInputLoad", 
                     paste0("Mean for the normal prior on the loading ", click_data()),
                     value = 0, min = -100, max = 100, step = 1),
        numericInput("priorScaleInputLoad", 
                     paste0("Standard deviation for the normal prior on the loading ", click_data()),
                     value = 10, min = 0, max = 100, step = 1),
        
        actionButton("fixLoadPrior", "Set this prior"),
        
        br(),
        
        renderText({
          paste0("With this prior approximately 95% prior probability is assigned in the range [", 
                 round(qnorm(p = .025, mean = input$priorMeanInputLoad, sd = input$priorScaleInputLoad), 2), " ; ",
                 round(qnorm(p = .975, mean = input$priorMeanInputLoad, sd = input$priorScaleInputLoad), 2), "]")
        }),
        
        br(),
        
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
        
        br(),
        
        renderText({
          paste0("With this prior approximately 95% prior probability is assigned in the range [", 
                 round(qgamma(p = .025, shape = input$priorShapeInputVar, rate = input$priorRateInputVar), 2), " ; ",
                 round(qgamma(p = .975, shape = input$priorShapeInputVar, rate = input$priorRateInputVar), 2), "]")
        }),
        
        br(),
        
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
        
        br(),
        
        renderText({
          paste0("With this prior approximately 95% prior probability is assigned in the range [", 
                 round(qbeta(p = .025, shape1 = input$priorShapeInputCorr1, shape2 = input$priorShapeInputCorr2), 2), " ; ",
                 round(qbeta(p = .975, shape1 = input$priorShapeInputCorr1, shape2 = input$priorShapeInputCorr2), 2), "]")
        }),
        
        br(),
        
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
        
        br(),
        
        renderText({
          paste0("With this prior approximately 95% prior probability is assigned in the range [", 
                 round(qnorm(p = .025, mean = input$priorMeanInputRegr, sd = input$priorScaleInputRegr), 2), " ; ",
                 round(qnorm(p = .975, mean = input$priorMeanInputRegr, sd = input$priorScaleInputRegr), 2), "]")
        }),
        
        br(),
        
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
   
  addPrior <- observeEvent(input$fixLoadPrior, {
    r <- which(priors$df$Parameter == click_data())
    priors$df[r, "Hyperparameter 1"] <- input$priorMeanInputLoad
    priors$df[r, "Hyperparameter 2"] <- input$priorScaleInputLoad
  })
  
  addPrior2 <- observeEvent(input$fixVarPrior, {
    r <- which(priors$df$Parameter == click_data())
    priors$df[r, "Hyperparameter 1"] <- input$priorShapeInputVar
    priors$df[r, "Hyperparameter 2"] <- input$priorRateInputVar
  })
  
  addPrior3 <- observeEvent(input$fixCorrPrior, {
    r <- which(priors$df$Parameter == click_data())
    priors$df[r, "Hyperparameter 1"] <- input$priorShapeInputCorr1
    priors$df[r, "Hyperparameter 2"] <- input$priorShapeInputCorr2
  })
  
  addPrior4 <- observeEvent(input$fixRegrPrior, {
    r <- which(priors$df$Parameter == click_data())
    priors$df[r, "Hyperparameter 1"] <- input$priorMeanInputRegr
    priors$df[r, "Hyperparameter 2"] <- input$priorScaleInputRegr
  })
  
  # Set blavaan default priors if requested
  addDefPrior <- observeEvent(input$defaultPriors, { 
    # default priors loadings
    sel1 <- grep("l", priors$df$Parameter)
    priors$df[sel1, "Hyperparameter 1"] <- 0
    priors$df[sel1, "Hyperparameter 2"] <- 10
    # default priors variances
    sel2 <- grep("v", priors$df$Parameter)
    priors$df[sel2, "Hyperparameter 1"] <- 1
    priors$df[sel2, "Hyperparameter 2"] <- 0.5
    # default priors correlations
    sel3 <- grep("r", priors$df$Parameter)
    priors$df[sel3, "Hyperparameter 1"] <- 1
    priors$df[sel3, "Hyperparameter 2"] <- 1
    # default priors structural regression coefficients
    sel4 <- grep("b", priors$df$Parameter)
    priors$df[sel4, "Hyperparameter 1"] <- 0
    priors$df[sel4, "Hyperparameter 2"] <- 10
  })
  
  defaultWarn <- eventReactive(input$defaultPriors, {
    paste("<b>Warning</b>: carefully consider the prior distribution for each parameter. 
          The default settings from blavaan might not be suitable for the application at hand. 
          Always conduct a prior sensitivity analysis, even when default priors are being used.")
  })
  
  output$defaultWarn <- renderText({
    defaultWarn()
  })
  
  output$priorVals <- renderDataTable({ 
    priors$df[, 1:4] # do not show the prior specification
  })
  
  # show warning if estimate button is clicked before all priors are specified
  estWarn <- eventReactive(input$estMod, {
    indNA <- complete.cases(priors$df)
    if(all(indNA) == TRUE){ 
      "All priors are specified"
    }
    else({
      c("Please specify the priors for the following parameters first:", paste(priors$df[!indNA, "Parameter"]))
    })
  })

  output$estWarn <- renderText({
    estWarn()
  })
  
  # estimate the model 
  fitBlav <- eventReactive(input$estMod, {
    indNA <- complete.cases(priors$df)
    if(all(indNA) == TRUE){ # if all priors are specified: start model estimation
      priors$df$priorspec <- apply(priors$df, 1, priorspec) # add priors in correct form for blavaan to priors$df
      df.full <- merge(fit()$modLbl, priors$df, by.x = "label", by.y = "Parameter") # combine prior specification with model labels
      # create model syntax with priors
      df.full$modspec <- paste(df.full$lhs, df.full$op, df.full$rhs, sep = " ")
      df.full$spec <- paste(df.full$priorspec, df.full$modspec, sep ="")
      modprior <- paste(df.full$spec, collapse = " \n ")
      if(input$example == "Own data"){
        fit.blav <- bsem(modprior, data = dat(), 
                         n.chains = input$chains, burnin = input$burnin, sample = input$iter)
      } else if(input$example == "Example data"){
        fit.blav <- bsem(modprior, data = PoliticalDemocracy,
                         n.chains = input$chains,  burnin = input$burnin, sample = input$iter)
      }
    }
  })
  
  output$fitCompl <- renderText({
    req(input$estMod)
    if(!is.null(fitBlav())){
      "Estimation is complete. Either download the blavaan fitobject or continue to the tab 'output' tab"
    }
  })
  
  output$download <- renderUI({
    req(fitBlav())
    downloadButton("downloadFit", "Download the Stan fitobject")
  })
  
  output$downloadFit <- downloadHandler(
    filename = "stanfit_bsem.RData",
    content = function(file) {
      saveRDS(fitBlav(), file)
    }
  )
  
  summ <- reactiveValues()
  
  observe({
    req(fitBlav())
    pt <- parTable(fitBlav())[, c("lhs", "op", "rhs", "se", "prior")]
    pt$Parameter <- paste0(pt$lhs, pt$op, pt$rhs)
    out1 <- as.data.frame(pt[, -c(1:3)])
    colnames(out1) <- c("Posterior SD", "Prior", "Parameter")
    
    means <- blavInspect(fitBlav(), what = "postmean")
    medians <- blavInspect(fitBlav(), what = "postmedian")
    hpd <- blavInspect(fitBlav(), what = "hpd", level = 0.95) # does not correspond with pi from summary?
    out2 <- cbind.data.frame(means, medians, hpd)
    out2$Parameter <- rownames(out2)
    colnames(out2) <- c("Posterior mean", "Posterior median", 
                        "Lower bound 95% CI", "Upper bound 95% CI", "Parameter")
    
    dt <- merge(out1, out2, by = "Parameter")
    summ$dt <- dt[, c("Parameter", "Posterior mean", "Posterior median", "Posterior SD",
                      "Lower bound 95% CI", "Upper bound 95% CI", "Prior")]
  })
  
  output$fitSummary <- renderDataTable({
    return(datatable(summ$dt) %>%
             formatRound("Posterior mean", digits = 2) %>%
             formatRound("Posterior median", digits = 2) %>%
             formatRound("Posterior SD", digits = 2) %>%
             formatRound("Lower bound 95% CI", digits = 2) %>%
             formatRound("Upper bound 95% CI", digits = 2)
    )
  })

  convOut <- reactive({
    req(fitBlav())
    convfun(fitBlav(), lbls = fit()$modLbl, totalN = input$iter*input$chains)
  })
  
  output$convInfo <- renderDataTable({
    return(datatable(convOut()$df) %>%
             formatRound("Potential scale reduction statistic (Rhat)", digits = 2) %>%
             formatStyle("Potential scale reduction statistic (Rhat)", Color = styleInterval(convOut()$rhatC, c("black", "red"))) %>%
             formatRound("Effective N", digits = 1) %>%
             formatStyle("Effective N", Color = styleInterval(convOut()$neffC, c("red", "black"))) %>%
             formatRound("Ratio effective to total N", digits = 1) %>%
             formatStyle("Ratio effective to total N", Color = styleInterval(convOut()$neff_ratioC, c("red", "black")))
           )
  })
  
  convWarning <- reactive({
    req(convOut())
    if(convOut()$conv == "yes"){
      "The model has converged according to the following criteria: all parameters have: <br>
      1) a value for the potential scale reduction statistic (Rhat) smaller than or equal to 1.1; <br>
      2) an effective number of iterations of at least 100; and <br>
      3) a ratio of effective number of iterations to the total number of iterations of at least 0.1. <br>
      Please note that these criteria are based on heuristics and this is therefore no guarantee that the model has indeed converged.
      To further assess convergence, it is possible to download the blavaan fitobject and further process it in R. 
      Please see the references for more resources on how to assess convergence."
    } else{
      "The model has <b>not</b> converged. At least one parameter shows one or more of the following: <br>
      1) a value for the potential scale reduction statistic (Rhat) greater than 1.1; <br>
      2) an effective number of iterations less than 100; <br>
      3) a ratio of effective number of iterations to the total number of iterations smaller than 0.1.<br>
      Please increase the number of iterations or reconsider your model. The current results should not be trusted."
    }
  })
  
  output$convWarn1 <- output$convWarn2 <- renderText({
    convWarning()
  })
  
  output$ppp <- renderText({
    req(fitBlav())
    paste0("The posterior predictive p-value equals ", fitBlav()@test[[2]]$stat, ". Values close to 0.5 indicate a model that fits the observed data.")
  })
  
  
  # Visualization estimated model
  # first add posterior mean estimates to lavaan fitobject
  fitLavEst <- reactive({
    req(input$model, fitBlav())
    # this is inefficient, because the lavaan model is estimated twice now, but the app breaks when meanstructure = T is added to the previous instance
    mod.lbl <- label_syntax_fun(input$model, meanstructure = TRUE)
    #fitLav <- sem(mod.lbl, data = dat(), meanstructure = TRUE)
    fitLav <- sem(mod.lbl, data = PoliticalDemocracy, meanstructure = TRUE)
    ptb <- fitBlav()@ParTable
    
    # remove intercepts from fitLav@ParTable to avoid them being plotted
    sel <- which(fitLav@ParTable$op == "~1")
    fitLav@ParTable <- lapply(fitLav@ParTable, function(x) x[-sel])
    
    # make sure ptb and fitLav@ParTable are ordered similarly before adding Bayesian estimates to lavaan ParTable
    n <- length(ptb$id)
    if(sum(ptb$lhs == fitLav@ParTable$lhs) == n &
       sum(ptb$op == fitLav@ParTable$op) == n &
       sum(ptb$rhs == fitLav@ParTable$rhs) == n){
      fitLav@ParTable$est <- ptb$est
    } else{
      ord <- paste0(fitLav@ParTable$lhs, fitLav@ParTable$op, fitLav@ParTable$rhs)
      ptb.df <- data.frame("id" = ptb$id, 
                           "par" = paste0(ptb$lhs, ptb$op, ptb$rhs))
      ids <- ptb.df[match(ord, ptb.df$par), "id"]
      fitLav@ParTable$est <- ptb$est[ids]
    }
    
    return(fitLav)
  })

  # plot the model with estimates
  output$mod.plot.est <- renderPlotly({
    ggplotly(plot_fun(fitLavEst(), est = TRUE), tooltip = "text")
  })
 
  output$references <- renderText({
  "<b>Blavaan</b>
  <br>
  Merkle, E. C., & Rosseel, Y. (2018). blavaan: Bayesian Structural Equation Models via Parameter Expansion. <i>Journal of Statistical Software, 85,</i> 1-30.
  <br>
  Merkle, E. C., Fitzsimmons, E., Uanhoro, J., & Goodrich, B. (2021). Efficient Bayesian Structural Equation Modeling in Stan. <i>Journal of Statistical Software, 100,</i> 1-22.
  <br>
  <br>
  <b>Fit indices in BSEM</b>
  <br>
  Garnier-Villarreal, M., & Jorgensen, T. D. (2020). Adapting fit indices for Bayesian structural equation modeling: Comparison to maximum likelihood. <i>Psychological Methods, 25</i>(1), 46.
  <br>
  <br>
  <b>Prior sensitivity in BSEM</b>
  <br>
  Van Erp, S., Mulder, J., & Oberski, D. L. (2018). Prior Sensitivity Analysis in Default Bayesian Structural Equation Modeling. <i>Psychological Methods, 23</i>(2), 363-388."
  })
  
  output$about <- renderText({
    "Developed by: <a href = https://saravanerp.com>Sara van Erp</a>
    <br>
    <br>
   This Shiny app enables researchers to visualize structural equation models (SEMs) and 
   use this visualization to easily specify prior distributions and subsequently run a Bayesian analysis using <a href = https://github.com/ecmerkle/blavaan>blavaan</a>. 
    Currently, this app is still under development, so please send bugs or comments to <a href=mailto:sara.vanerp@gmail.com>Sara van Erp</a>. 
    The source code can be found <a href = https://github.com/sara-vanerp/shinyBSEM>here</a>.
    <br>
    <br>
    <i>Acknowledgements</i>
    <br>
    Thanks to Caspar van Lissa for adapting the <a href = https://cjvanlissa.github.io/tidySEM/>tidySEM package</a> for use in this app."
  })
  
}