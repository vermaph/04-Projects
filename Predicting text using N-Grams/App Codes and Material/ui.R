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
                                   tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")

                                ) 
                          )  # End of 1st fluidRow
                  ,
                  fluidRow(br()
                           
                           ,
                           column(8, align="left", offset = 2
                                  ,
                                  br()
                                  ,
                                  tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                                  ,
                                  textOutput("text3")
                           ) 
                  )  # End of 2nd fluidRow
                  ,
                  
                  # Application title
                  fluidRow(
                          column(8, align="center", offset = 2
                                  ,
                                  br(),
                                  textInput("incoming","Enter your word here and see live predictions:")
                                  ,
                                  tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                                )        
                          ), # End of 2nd fluidRow
                  fluidRow(
                          column(8, align="center", offset = 2
                                 ,
                                 #h3("Your input:"),
                                 #textOutput("text1"),  # Displays Input
                                 br(),
                                 div(textOutput("display"),style = "color:green"),  # Displays which gram was used for prediction
                                 br(),
                                 h3("My prediction:"),
                                 div(textOutput("prediction"),style = "color:red")  # Displays the prediction 
                                 ,
                                 tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 60px; display: block;}")
                                 ,
                                 h3("My LinkedIn !", a("Piyush Verma", href="https://www.linkedin.com/in/vermaph/"))
                                 ,
                                 h3("Github location !", a("Predict the next word R code", href="https://github.com/grammilo/Codes/tree/master/Pet%20Projects/Text%20Mining%20-%20Word%20Prediction"))
                                 ,
                                 h3("You can find my other pet projects here !", a("Tableau Public,", href="https://public.tableau.com/profile/piyush.verma#!/"), a("R Pubs", href="https://rpubs.com/vermaph"))
                                )
                          ) # End of 3rd fluidRow
                 ) # End of fluidPage
)
                  
