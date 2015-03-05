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
  
  
  h2("Multiple Regression Visualization"),
  p("When creating a model, it can be very helpful to visualize both the data and the model.
      Often we wish to create a prediction model for a response variable on more than one predictors. 
      In the case of a single response and two predictors, we must use a third dimension to visualize the 
      the data and model."),
  p("In this app, you will be able to  visualize the data and explore the effectiveness of different models
    for a numerical response variable. "),
 
  sidebarLayout(

    
    sidebarPanel(
    
      tags$title("3D Model Viewer"),

      selectInput("dataset",label = "Select a dataset", choices = c("Iris"= "iris", "Cars" = "mtcars",
                                                                    "U.S." = "state.x77")),
      conditionalPanel(condition = "input.dataset == 'iris'",
                       radioButtons("expTypes1", label = "Available Models: Sepal Area = ", 
                         choices = list("Sepal Length + Sepal Width" = 1, "Sepal Length + Sepal Width + Interaction" = 2,
                                        "Sepal Length + Sepal Width, Species (2D)" = 3,
                                        "Sepal Length + Sepal Width + Species (3D)" = 4,"None" = 5),
                         selected = 5`)),
      conditionalPanel(condition = "input.dataset == 'mtcars'",
                       radioButtons("expTypes2", label = "Available Models: MPG =", 
                          choices = list("Horsepower + Weight" = 1, "Horsepower + Weight + Interaction" = 2,
                                         "Horsepower + Weight, Transmission (2D)" = 3,
                                         "Horsepower + Weight + Transmission (3D)" = 4, "None" = 5),
                          selected = 5)),
      conditionalPanel(condition = "input.dataset == 'state.x77'",
                       radioButtons("expTypes3", label = "Available Models: Life Expectancy =", 
                          choices = list("Murder Rate + HS Graduation Rate" = 1, 
                                         "Murder + HSGrad + Interaction" = 2,
                                         "Murder + HSGrad, Region (2D)" = 3,
                                         "Murder + HSGrad + Region (3D)" = 4, "None" = 5),
                         selected = 5))
      
    ),
    mainPanel(
      
      conditionalPanel(condition = "input.expTypes1 != 3 || input.expTypes2 != 3 || input.expTypes3 != 3",
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
                                  verbatimTextOutput("modelRsq")
                                  )
                                                           
                        )
                       ),
      conditionalPanel(condition = "input.expTypes1 == 3 || input.expTypes2 == 3 || input.expTypes3 == 3",
                     tabsetPanel(
                       tabPanel("Plot",plotOutput("cat2d")),
                       tabPanel("Model Info",      
                                withMathJax(),
                                helpText("The Response variable is:"),
                                verbatimTextOutput("catResp"),
                                br(),
                                helpText("The current model is:"),
                                verbatimTextOutput("catModel"),
                                br(),
                                helpText("The corresponding \\(R^2-adjusted\\) is:"),
                                verbatimTextOutput("catRsq")
                       )
                     )
                     
                     )
    

    )
    
  )
))