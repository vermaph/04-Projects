library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
                  # Application title
                  titlePanel("Data Science Specialization Capstone Project: Predict the next word")
                  ,
                  sidebarLayout(
                    sidebarPanel(
                      textInput("incoming","Enter your word here and press submit:")
                    ),
                    mainPanel(
                      h6("Your input:"),
                      textOutput("original"),
                      br(),
                      h6("Our prediction:"),
                      div(textOutput("prediction"),style = "color:red")
                    )
                  )
                  )
        ) # End of ShinyUI

