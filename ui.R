library(shiny)
library(shinythemes)
library(ggvis)

pred_names <- list("Log frequency (prior to AoFP)" = "sln.freq.pre", 
                   "# of phonemes" = "s.cmu.phon", 
                   "Mean utterance length (prior to AoFP)" = "s.uttlen.pre",
                   "MRC Concreteness" = "s.mrc.conc",
                   "MRC Imageability" = "s.mrc.imag",
                   "MRC Familiarity" = "s.mrc.fam",
                   "Spatial distinctiveness" = "srl.sp.KL",
                   "Temporal distinctiveness" = "srl.temp.KL", 
                   "Linguistic distinctiveness" = "srl.topic.KL")

cat_names <- list("All", "Nouns", "Verbs", "Adjectives", "Closed Class", "Other")

shinyUI(
  fluidPage(
    
    theme = shinytheme("spacelab"),
    
    titlePanel("Word Birth Browser"),
    #     br(),
    p("Our analysis explores the words a single child had produced 
      by age two, displayed by their age of first production (AoFP). The 
      interactive visualation allows users to explore regression models 
      predicting AoFP on the basis of different aspects of the child's language 
      input. See Main Text and Supplemental Information for more information about 
      models and variable coding."),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput("pred_vars", "Predictors:",
                           pred_names, selected = pred_names[1:3]),
        radioButtons("labels", "Labels on individual words:",
                     selected = FALSE,                   
                     c("off" = FALSE,
                       "on" = TRUE),
                     inline = TRUE),
        radioButtons("robust", "Robust regression:",
                     selected = FALSE,                   
                     c("off" = FALSE,
                       "on" = TRUE),
                     inline = TRUE),
        selectizeInput("cats", "Word Categories:", 
                       cat_names, multiple = FALSE)),     
      mainPanel(
        width = 9,
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        tabsetPanel(
          tabPanel("Predictions Plot",
                   br(),
                   h4("Child age of first production plotted by model predictions"),
                   div(style = "height: 300px;",                   
                       ggvisOutput("scatter")
                   )),  
          tabPanel("Model Summary",
                   br(), 
                   h4("R Regression model output"),                  
                   verbatimTextOutput("summary")),
          tabPanel("Raw Data",    
                   br(),
                   h4("Download full dataset:"),                                     
                   downloadButton('downloadData', 'download'),
                   br(),                   
                   br(),
                   h4("Browse the data specified in the current plot and model:"),
                   dataTableOutput(outputId = "datatable"))      
        )
      )
    )
  )
)