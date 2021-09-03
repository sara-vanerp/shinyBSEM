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