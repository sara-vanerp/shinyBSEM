##### Functions ShinyBSEM #####
## Author: Sara van Erp

## Function to automatically add intuitive labels to the lavaan syntax
label_syntax_fun <- function(model){
  parTab <- lavaanify(model, meanstructure = FALSE, int.ov.free = TRUE, int.lv.free = FALSE,
                      auto.fix.first = TRUE, auto.fix.single = TRUE, auto.var = TRUE,
                      auto.cov.lv.x = TRUE, auto.efa = TRUE, auto.th = TRUE,
                      auto.delta = TRUE, auto.cov.y = TRUE)
  
  load <- parTab[parTab$op == "=~", ]
  if(nrow(load) > 0){
    load[, "label"] <- paste0("l", 1:nrow(load))
  }
  
  vars <- parTab[parTab$op == "~~" & parTab$lhs == parTab$rhs, ]
  if(nrow(vars) > 0){
    vars[, "label"] <- paste0("v", 1:nrow(vars))
  }
  
  corr <- parTab[parTab$op == "~~" & parTab$lhs != parTab$rhs, ]
  if(nrow(corr) > 0){
    corr[, "label"] <- paste0("r", 1:nrow(corr))
  }
  
  regr <- parTab[parTab$op == "~", ]
  if(nrow(regr) > 0){
    regr[, "label"] <- paste0("b", 1:nrow(regr))
  }
  
  parTabLab <- rbind.data.frame(load, vars, corr, regr)
  return(parTabLab)
}

## Function to plot the model using tidySEM
plot_fun <- function(fit, layout = "layout_as_tree"){
  prep <- prepare_graph(fit, layout_algorithm = layout)
  
  prep$edges$geom_text <- TRUE # to be able to record and use the labels
  prep$nodes$geom_text <- TRUE # to be able to record and use the labels
  
  # show the labels instead of the classical estimates
  # TODO: avoid estimating the classical model at all
  prep$edges$label <- prep$edges$lavaan_label
  pmod <- plot(prep)
  return(pmod)
}

## Funtion to initialize a data frame to store the priors in
init_df <- function(pars){
  if(grepl("l", pars) == TRUE){
    cbind.data.frame(Parameter = pars, Prior = "Normal", 'Hyperparameter 1' = NA, 'Hyperparameter 2' = NA)
  } else if(grepl("v", pars) == TRUE){
    cbind.data.frame(Parameter = pars, Prior = "Gamma", 'Hyperparameter 1' = NA, 'Hyperparameter 2' = NA)
  } else if(grepl("r", pars) == TRUE){
    cbind.data.frame(Parameter = pars, Prior = "Beta", 'Hyperparameter 1' = NA, 'Hyperparameter 2' = NA)
  } else if(grepl("b", pars) == TRUE){
    cbind.data.frame(Parameter = pars, Prior = "Normal", 'Hyperparameter 1' = NA, 'Hyperparameter 2' = NA)
  }
}

## Function to set all priors to the blavaan default
def_prior <- function(df){
  load <- grep("l", df$pars)
  df[load, "Hyperparameter 1"] <- 0
  df[load, "Hyperparameter 2"] <- 10
  
  vars <- grep("v", df$pars)
  df[vars, "Hyperparameter 1"] <- 1
  df[vars, "Hyperparameter 2"] <- .5
  
  cors <- grep("r", df$pars)
  df[cors, "Hyperparameter 1"] <- 1
  df[cors, "Hyperparameter 2"] <- 1
  
  regs <- grep("b", df$pars)
  df[regs, "Hyperparameter 1"] <- 0
  df[regs, "Hyperparameter 2"] <- 10
  
  return(df)
}

## Function to add priors in correct form to the prior dataframe
priorspec <- function(df){
  if(df["Prior"] == "Normal"){
    spec <- paste0("prior(\"dnorm(", df["Hyperparameter 1"], ", ", df["Hyperparameter 2"], ")\")* ")
  }
  if(df["Prior"] == "Gamma"){
    spec <- paste0("prior(\"dgamma(", df["Hyperparameter 1"], ", ", df["Hyperparameter 2"], ")\")* ")
  }
  if(df["Prior"] == "Beta"){
    spec <- paste0("prior(\"dbeta(", df["Hyperparameter 1"], ", ", df["Hyperparameter 2"], ")\")* ")
  }
  return(spec)
}

## Function to combine convergence diagnostics
# Use specific cutoffs for convergence; these are hardcoded only here
convfun <- function(fit, lbls, totalN, rhatC = 1.1, neffC = 100, neff_ratioC = 0.1){
  rhat <- blavInspect(fit, what = "psrf")
  neff <- blavInspect(fit, what = "neff")
  neff_ratio <- neff/totalN
  convdf <- cbind.data.frame("Potential scale reduction statistic (Rhat)" = rhat, 
                             "Effective N"= neff, 
                             "Ratio effective to total N" = neff_ratio)
  convdf$`Parameter` <- rownames(convdf)
  # add original labels
  lbls$`Parameter` <- paste(lbls$lhs, lbls$op, lbls$rhs, sep = "")
  lbls.sel <- lbls[, c("Parameter", "label")]
  colnames(lbls.sel) <- c("Parameter", "Label")
  df <- merge(lbls.sel, convdf, by = "Parameter")
  out <- list(df = df, rhatC = rhatC, neffC = neffC, neff_ratioC = neff_ratioC)
  return(out)
}


