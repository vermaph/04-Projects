# Shiny app: Prediction of mileage based on car specifications
# This is the user interface for the app

library("shiny")
shinyUI(fluidPage(
  
  
  titlePanel("Car Mileage Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("cyl1","Number of cylinders in your car",min=4,max=8,step=2,4),
      sliderInput("wt1","Your car's weight in 1000 lbs",min=1,max=6,step=0.01,3)
    ),
    
    
    mainPanel(
      h4("Estimated mileage (in Miles/(US) gallon):"),
      tabsetPanel(
        tabPanel("Documentation","User inputs number of cylinders and weight of their car and server uses a linear regression model to predict car mileage"),
        tabPanel("Estimation",textOutput("mpg_pred"))
      )
      
    )
  )
))

