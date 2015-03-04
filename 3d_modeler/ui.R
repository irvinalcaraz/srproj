# ------------------------------------------------
#  App Title: 3D Model Viewer
#     Author: Irvin Alcaraz
# ------------------------------------------------


if (!require("devtools")){install.packages("devtools")}
if (!require("shiny")){install.packages("shiny")}
# if (!require("shinysky")) devtools::install_github("ShinySky","AnalytixWare")
if (!require("shinyRGL")){install.packages("shinyRGL")}
# if (!require("shinyBS")){install.packages("shinyBS")}

# library(shinysky)
# library(shinyBS)
library(shinyRGL)
library(shiny)
library(rgl)

shinyUI(fluidPage(
  
  
  h2("3D Model Viewer"),
  p("When creating a model, it can be very helpful to visualize both the data and the model.
      Often we wish to create a prediction model for a response variable on more than one predictors. 
      In the case of a single response and two predictors, we must use a third dimension to visualize the 
      the data and model."),
  p("In this app, you will be able to  visualize the data and explore the effectiveness of different models
    for a numerical response variable. "),
 
  sidebarLayout(

    
    sidebarPanel(
    
      tags$title("3D Model Viewer"),
#       helpText("The response variable is numerical."),
#       helpText("Explanatory Variable Types to Include"),
#       helpText("Note: Qualitative = Qual, Quantitative = Quant"),
      selectInput("dataset",label = "Select a dataset", choices = c("Iris"= "iris", "Cars" = "mtcars",
                                                                    "US" = "state.x77")),
      conditionalPanel(condition = "input.dataset == 'iris'",
                       radioButtons("expTypes1", label = "Available Models ", 
                         choices = list("Sepal Length, Sepal Width" = 1, "Sepal Length, Sepal Width, Interaction" = 2,
                                        "Sepal Length, Sepal Width, Species" = 3, "None" = 4),
                         selected = 4)),
      conditionalPanel(condition = "input.dataset == 'mtcars'",
                       radioButtons("expTypes2", label = "Available Models", 
                          choices = list("Qual,Quant" = 1, "Qual,Quant,Interaction" = 2,
                                         "Quant,Quant" = 3, "Quant,Quant,Interaction" = 4),
                          selected = 3)),
      conditionalPanel(condition = "input.dataset == 'state.x77'",
                       radioButtons("expTypes3", label = "Available Models", 
                          choices = list("Qual,Quant" = 1, "Qual,Quant,Interaction" = 2,
                                         "Quant,Quant" = 3, "Quant,Quant,Interaction" = 4),
                          selected = 3))
#       checkboxInput("surfaceOrNah","Toggle Surface")
#       actionButton("go","Submit")
      
    ),
    mainPanel(
    
      tabsetPanel(
        tabPanel("Plot",webGLOutput("troisPlot",width="600px",height="600px")),
        tabPanel("Model Info",      
                 withMathJax(),
                 helpText("The Response variable is:"),
                 verbatimTextOutput("responseVar"),
                 br(),
                 helpText("The current model is:"),
                 verbatimTextOutput("modelEQ"),
                 br(),
                 helpText("The corresponding \\(R^2-adjusted\\) is:"),
                 verbatimTextOutput("modelRsq"))
          
        
      )
    )
    
  )
))