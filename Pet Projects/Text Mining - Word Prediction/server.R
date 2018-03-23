## R Shiny App Location: https://vermaph.shinyapps.io/Text_Prediction/

library(tm)
library(knitr)
library(stringi)
library(RWeka)
library(ggplot2)



unigram <- readRDS("./unigram.RData")
bigram <- readRDS("./bigram.RData")
trigram <- readRDS("./triigram.RData")
quadgram <- readRDS("./quadgram.RData")
msg<-""


## Prediction Version 2.0 (N-Back off algorithm -- Better Accuracy)
## Building the predict function
Predict <- function(x){
  
  x<-strsplit(as.character(x), split = "\\s+")[[1]]   # "\\s+" is for space
  x<-removeNumbers(removePunctuation(tolower(x))) # Preparation of the user input: Splitting and Cleaning
  x
  
  ## Back off algorithm
  if (length(x) >=3) {
    x<-tail(x,3)
    if (identical(character(0),head(quadgram[quadgram$first == x[1] & quadgram$second == x[2] & quadgram$third == x[3],"fourth"],1)))
    {
      Predict(paste(x[2],x[3],sep = " "))
    }
    else
    {msg<<-"Predicting word using the most popular four letter sentence";head(quadgram[quadgram$first == x[1] & quadgram$second == x[2] & quadgram$third == x[3],"fourth"],1)}
  }
  
  
  else if (length(x) ==2) {
    x<-tail(x,2)
    if (identical(character(0),head(trigram[trigram$first == x[1] & trigram$second == x[2],"third"],1)))
    {
      Predict(x[3])
    }
    else
    {msg<<-"Predicting word using the most popular three letter sentence";head(trigram[trigram$first == x[1] & trigram$second == x[2],"third"],1)}
  }
  
  
  else if (length(x) ==1) {
    x<-tail(x,1)
    if (identical(character(0),head(bigram[bigram$first == x[1],"second"],1)))
    {msg<<-"No match found !!! Please type some other word"}
    else
    {msg<<-"Predicting word using the most popular two letter sentence";head(bigram[bigram$first == x[1],"second"],1)}
  }
  
}



shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    result <- Predict(input$incoming)
    output$display <- renderText({msg})
    result
  });
  
  output$text1 <- renderText({
    input$incoming});
  
  output$text3 <- renderText({
    paste("This app can be used as a POC to build more evolved products. For example, a customer types spicy and the app should produce the recipes for all the spicy cuisines filtered according to the customer's historic orders (European-Asian-Mediterranean etc). Or it can be used to predict a service complain even before a customer completes typing his complaint: possibly cutting down the critical customer service time")});
}
)
