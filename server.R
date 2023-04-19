library(shiny)
library(shinyapps)
library(MASS)
library(dplyr)
library(ggvis)
library(stringr)
library(xtable)

d <- read.csv("pred_data.csv")
d$aoa <- d$aoa / 30.3 # days to months
d$small.cat <- factor(d$small.cat, 
                      levels = c("Nouns","Verbs","Adjectives",
                                 "Closed class","Other"),
                      labels = c("Nouns","Verbs","Adjectives",
                                 "Closed Class","Other"))

basic_vars <- c("word", "small.cat", "cdi.cat", "aoa", "aoa.pred")

##### SHINY SERVER START #######
shinyServer(function(input, output, session) {
  
  # cut down to category of interest
  d.subset <- reactive({
    if (input$cats == "All") {
      d 
    } else {      
      d %>% filter(small.cat %in% input$cats) 
    }
  })
    
  # fit model to cat of interest
  mod <- reactive({
    
    if (input$robust == TRUE & length(input$pred_vars) > 0) {
      rlm(as.formula(paste("aoa ~ ",str_c(input$pred_vars,collapse=" + ")," + 1", sep="")),
          data=d.subset(), na.action = na.exclude)
    } else {
      lm(as.formula(paste("aoa ~ ",str_c(input$pred_vars,collapse=" + ")," + 1", sep="")),
         data=d.subset(), na.action = na.exclude)      
    }
  })
  
  # filter out NAs and add model predictions
  d.plot <- reactive({      
    d.plot <- d.subset()
    d.plot$aoa.pred <- predict(mod())     
    d.plot %>% filter(!is.na(aoa.pred)) 
  })
  
  # round for data table output
  d.out <- reactive({
    d.out <- d.plot()
    d.out[c(2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)] <- round(d.out[c(2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)], 
                                                         digits = 2) 
    d.out <- d.out %>% 
      dplyr::select(one_of(basic_vars), 
                    one_of(input$pred_vars))
    d.out
  })
  
  # add word labels?
  labels <- reactive({input$labels})
  
  # use robust regression?
  rmodel <- reactive({
    if (input$robust == TRUE & length(input$pred_vars) > 0) { 
      "rlm" 
    } else { 
      "lm" 
    }
  })
  
  # major plot
  reactive({        
    if (labels()) {
      d.plot %>%
        ggvis(x = ~aoa.pred, y = ~aoa) %>% 
        layer_text(text := ~word, fill = ~small.cat,  
                   align := "center", baseline := "middle") %>%
        layer_model_predictions(model=rmodel(), strokeDash := 10, se=TRUE) %>%
        group_by(small.cat) %>%
        layer_model_predictions(model=rmodel(), stroke = ~small.cat, strokeDash := 3) %>%
        scale_numeric("x", domain = c(13, 23), nice = TRUE, clamp = FALSE) %>% # domain = c(12, 23), 
        scale_numeric("y", domain = c(9, 24), nice = TRUE, clamp = FALSE) %>% # domain = c(9, 24),
        add_legend(c("stroke", "fill"), title = "Word category") %>%
        add_axis("x", title = "Model Predicted Age of First Production (months)") %>% # values = seq(14, 23, 1)
        add_axis("y", title = "Chid's Age of First Production (months)") # values = seq(9, 24, 1)
    } else {
      d.plot %>%
        ggvis(x = ~aoa.pred, y = ~aoa) %>% 
        layer_points(fill = ~small.cat, fillOpacity := .3) %>%   
        layer_model_predictions(model=rmodel(), strokeDash := 10, se=TRUE) %>%
        group_by(small.cat) %>%
        layer_model_predictions(model=rmodel(), stroke = ~small.cat, strokeDash := 3) %>%
        scale_numeric("x", domain = c(13, 23), nice = TRUE, clamp = TRUE) %>% #  n
        scale_numeric("y", domain = c(9, 24), nice = TRUE, clamp = TRUE) %>% # 
        add_legend(c("stroke", "fill"), title = "Word category") %>%
        add_axis("x", title = "Model Predicted Age of First Production (months)") %>% # values = seq(12, 23, 1)
        add_axis("y", title = "Chid's Age of First Production (months)") # values = seq(9, 24, 1) 
    }
  }) %>% bind_shiny("scatter") 
  
  # regression model output
  # note - this is harder to get formatted nicely than I want, see below for 
  # attempt to use MathJax
  output$summary <- renderPrint({  
      summary(mod())
  })

  # data table  
  output$datatable <- renderDataTable(d.out())
  
  # download button
  output$downloadData <- downloadHandler(filename = "roy_et_al_data.csv",  
                                     content = function (file) {
                                       write.csv(d, file, row.names = FALSE)
                                     }, 
                                     contentType = "text/csv")
})

#   output$summary <- renderUI({
#     mod <- summary(lm(as.formula(paste("aoa ~ ",str_c(input$pred_vars,collapse=" + ")," + 1", sep="")),
#                data=d,
#                na.action = na.exclude))
# #     print(mod)
# #     print(xtable(mod))
#     M <- print(xtable(mod),
#                floating=FALSE, tabular.environment="array", 
#                comment=FALSE, print.results=FALSE)
# #     print(M)
#     html <- paste0(M)
# 
#     list(
#       tags$script(src = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', 
#                   type = 'text/javascript'),
#       tags$script(src = "MathJax.Hub.Config({tex2jax: {inlineMath: [['$*$','$*$']]}});", 
#            type = "text/x-mathjax-config"),
#       HTML(html)
#     )
#   })