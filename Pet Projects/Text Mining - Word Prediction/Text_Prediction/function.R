####################################################################
# Building the predict function
# Backoff model
####################################################################
Predict <- function(x){
  
  # "\\s+" is for space
  x<-strsplit(as.character(x), split = "\\s+")[[1]]
  # Preparation of the user input: Splitting and Cleaning
  x<-removeNumbers(removePunctuation(tolower(x))) 

    ## Back off algorithm
    if (length(x) >=3) {
      x<-tail(x,3)
      if (identical(character(0),head(quadgram[quadgram$first == x[1] & quadgram$second == x[2] & quadgram$third == x[3],"fourth"],1)))
      {
        Predict(paste(x[2],x[3],sep = " "))
      }
      else
      {
        display<-"Predicting word using the most popular four letter sentence"
        head(quadgram[quadgram$first == x[1] & quadgram$second == x[2] & quadgram$third == x[3],"fourth"],1)
      }
    }
  
  
  else if (length(x) ==2) {
    x<-tail(x,2)
    if (identical(character(0),head(trigram[trigram$first == x[1] & trigram$second == x[2],"third"],1)))
    {
      Predict(x[3])
    }
    else
    {
      display<-"Predicting word using the most popular three letter sentence"
      head(trigram[trigram$first == x[1] & trigram$second == x[2],"third"],1)
    }
  }
  
  
  else if (length(x) ==1) {
    x<-tail(x,1)
    if (identical(character(0),head(bigram[bigram$first == x[1],"second"],1)))
    {
      display<-"No match found !!! Please type some other word"
    }
    else
    {
      display<-"Predicting word using the most popular two letter sentence"
      head(bigram[bigram$first == x[1],"second"],1)
    }
  }
  
}

test<-"I will"
Predict(test)