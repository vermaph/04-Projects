suppressWarnings(library(stringr))
suppressWarnings(library(shiny))


ngram <- readRDS("C:/0000/05 Github/R-Codes/R-codes/Pet Projects/Text Mining - Word Prediction/Text_Prediction/Ngram_data.RData")

Predict<-function(x){
  test<-removeNumbers(removePunctuation(tolower(x)))

  matches<-ngram[grep(test,ngram$Word_1),]
  matches<-matches[order(matches$Freq,decreasing = TRUE),]
  matches<-head(matches[,1],1)
  matches
}

shinyServer(function(input, output) 
{
# Generating the user input  
output$original<-renderText({
      output_original<-input$incoming
      return(output_original)
})
# Generating the prediction
output$prediction<-renderText({
  output_prediction<-Predict(input$incoming)
  return(output_prediction)
})

}
)
