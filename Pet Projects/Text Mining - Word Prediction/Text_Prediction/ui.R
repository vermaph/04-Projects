library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("superhero")   # Giving Shiny a theme
                  ,
                  fluidRow(br()
                           ,
                           br()
                           ,
                           column(8, align="center", offset = 2
                                   ,
                                   titlePanel("Data Science Specialization Capstone Project: Predict the next word")
                                   ,
                                   br()
                                   ,
                                   br()
                                   ,
                                   br()
                                   ,
                                   tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")

                                ) 
                          )  # End of 1st fluidRow
                  ,
                  # Application title
                  fluidRow(
                          column(8, align="center", offset = 2
                                  ,
                                  textInput("incoming","Enter your word here and see live predictions:")
                                  ,
                                  tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                                )        
                          ), # End of 2nd fluidRow
                  fluidRow(
                          column(8, align="center", offset = 2
                                 ,
                                 h3("Your input:"),
                                 textOutput("text1"),  # Displays Input
                                 br(),
                                 div(textOutput("display"),style = "color:green"),  # Displays which gram was used for prediction
                                 br(),
                                 h3("Our prediction:"),
                                 div(textOutput("prediction"),style = "color:red")  # Displays the prediction 
                                 ,
                                 tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                                 ,
                                 br(),br(),br(),
                                 h3("My LinkedIn !", a("Piyush Verma", href="https://www.linkedin.com/in/vermaph/"))
                                 ,
                                 h3("Github location !", a("Predict the next word R code", href="https://github.com/grammilo/R-codes/tree/master/Pet%20Projects/Text%20Mining%20-%20Word%20Prediction/Text_Prediction"))
                                 ,
                                 h3("You can find my other pet projects here !", a("Tableau Public,", href="https://public.tableau.com/profile/piyush.verma#!/"), a("R Pubs", href="https://rpubs.com/vermaph"))
                                )
                          ) # End of 3rd fluidRow
                 ) # End of fluidPage
)
                  
