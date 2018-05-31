# Shiny app: Prediction of mileage based on car specifications
# This is the server side of the shiny app

library(shiny)
shinyServer(function(input, output) {
   
  output$mpg_pred <- renderText({
    model<-lm(mpg~cyl+wt,data=mtcars)
    pred<-predict(model,data.frame(cyl=input$cyl1,wt=input$wt1))
    round(pred,2)
  })
  
})
