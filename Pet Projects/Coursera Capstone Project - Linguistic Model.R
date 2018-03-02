###################################################################################################################
## Course Name: Coursera Data Science Specialization 
## Objective: Capstone Project using NLP in building a predictive model
## Author: Piyush Verma
## Started: 02/22/2018 
## Ended: 
################################################################################################################

#+++++++++++++++++++++++++#
library(ggplot2)
library(tm)
library(RWeka)
library(wordcloud)
library(knitr)
library(caTools)
library(googleVis)
#+++++++++++++++++++++++++#

## QUiz 1

median<-c()
avg<-c()
max<-c()
maxchar<-c()
counts<-c()
files<-list.files()
for(i in 1:length(files)){
  con<-file(files[i],open = "r")   # opens the connection and saves file in the memory
  lines<-suppressWarnings(readLines(con))  # read filelines in 1 chunk
  nchars<-lapply(lines,nchar)   # counts number of characters line by line
  max[i]<-max(as.numeric(nchars))   #gets the max of the previous words count for that file
  maxchar[i]<-which.max(nchars) #locates the line number having the maximum number of words
  
  median[i]<-median(as.numeric(nchars)) # median number of words in a line
  avg[i]<-mean(as.numeric(nchars)) # avg number of words in a line 
  counts[i]<-length(nchars)   # number of lines in text
  close(con)
}
dt<-data.frame(cbind(file=files, 
                 max_word_line = maxchar,
                 lines_count=counts,
                 med_words_len=median,
                 avg_words_len=round(avg,0),
                 max_words_len=max
                 )
           ) # gives a more complete picture 
kable(dt)
con<-file("en_US.twitter.txt", open = "r")
twitter<-suppressWarnings(readLines(con))
close(con)

k<-length(grep("love",twitter))/length(grep("hate",twitter))   # counts the lines where love/hate occur in twitter file
k

grep("*biostats*",twitter, value = TRUE)
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing",twitter))


## QUiz 2

#++++++++++++++++++++++++++++++++++++++++ Looking into words per line +++++++++++++++++++++++++++++++++++++++++++

#+++++++ Histograms  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
for(i in 1:3){
word_counts<-lapply(suppressWarnings(readLines(file(files[i],open="r"))),nchar) # counts words in each line
if(i==1)
  title<-"blogs"
if(i==2)
  title<-"news"
if(i==3)
  title<-"twitter"

print(
  ggplot(data=data.frame(as.numeric(word_counts)),aes(x=as.numeric(word_counts)))
  +geom_histogram(binwidth=10)
  +scale_x_continuous(limits = c(0, 1000))  #choosing 1000 because blogs have ome lines which have huge count of words
  +ylab("Word frequency")+xlab("words per line")+ggtitle(title)+theme(plot.title = element_text(hjust=0.5))
  )
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++ Profanity File  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
profanityWords <- read.table("./profanity_filter.txt", header = FALSE)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++ All files ++++++++++++++++++++++++++++++++++++++++++++++
#Merging all files, taking 10% of all bulk for making a training set in next steps and cleaning appropriately 
set.seed(12396911)
all.data<-NULL
for(i in 1:3){
all.data<-append(all.data,readLines(file(files[i],open="r"))) # all.data is a character vector
} 
length(all.data)/10**6 # 3 million lines
sum(as.numeric(nchar(all.data))) / 10**6 # 386 million words
index<-sample.split(all.data,SplitRatio= .001, group=NULL)
all.data.sample<-all.data[index]
all_corpora <- VCorpus(VectorSource(all.data.sample), readerControl = list(language="en"))
all_corpora <- tm_map(all_corpora, removeNumbers)
all_corpora <- tm_map(all_corpora, removePunctuation)
all_corpora <- tm_map(all_corpora, removeWords, c(stopwords('english')))
all_corpora <- tm_map(all_corpora, stripWhitespace)
all_corpora <- tm_map(all_corpora, content_transformer(tolower))
all_corpora <- tm_map(all_corpora, removeWords, profanityWords$V1) #Profanity Filter
all_corpora <- tm_map(all_corpora, stemDocument, language='english')
all_corpora <- tm_map(all_corpora, PlainTextDocument)
all_corpora <- tm_map(all_corpora, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=" ")))
all_corpora <- tm_map(all_corpora, content_transformer(removeMostPunctuation))
tdm <- TermDocumentMatrix(all_corpora)
m <- as.matrix(tdm)
freq <- sort(rowSums(m), decreasing = TRUE)
wordcloud(words = names(freq), freq = freq, min.freq = 2, random.order = FALSE,col=terrain.colors(20), scale=c(8,.2), max.words=300, rot.per=.15)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++ WordCloud Blogs: START ++++++++++++++++++++++++++++++++++++++++++++++
set.seed(12396911)
blogsEN<-NULL
blogsEN<-append(blogsEN,suppressWarnings(readLines(file(files[1],open="r")))) # all.data is a character vector
index<-sample.split(blogsEN,SplitRatio= .001, group=NULL)
blogsEN.sample<-blogsEN[index]
blog_corpora <- VCorpus(VectorSource(blogsEN.sample), readerControl = list(language="en"))
blog_corpora <- tm_map(blog_corpora, removeNumbers)
blog_corpora <- tm_map(blog_corpora, removePunctuation)
blog_corpora <- tm_map(blog_corpora, removeWords, c(stopwords('english')))
blog_corpora <- tm_map(blog_corpora, stripWhitespace)
blog_corpora <- tm_map(blog_corpora, content_transformer(tolower))
blog_corpora <- tm_map(blog_corpora, removeWords, profanityWords$V1)  #Profanity Filter
blog_corpora <- tm_map(blog_corpora, stemDocument, language='english')
blog_corpora <- tm_map(blog_corpora, PlainTextDocument)
blog_corpora <- tm_map(blog_corpora, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=" ")))
tdm <- TermDocumentMatrix(blog_corpora)
m <- as.matrix(tdm)
freq <- sort(rowSums(m), decreasing = TRUE)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Blogs", col = "blue", cex = 1.5)
wordcloud(words = names(freq), freq = freq, min.freq = 2, random.order = FALSE,col=terrain.colors(20), scale=c(8,.2), max.words=300, rot.per=.15)
#+++++++ WordCloud Blogs: END ++++++++++++++++++++++++++++++++++++++++++++++



#+++++++ WordCloud News: START ++++++++++++++++++++++++++++++++++++++++++++++
set.seed(12396911)
newsEN<-NULL
newsEN<-append(newsEN,suppressWarnings(readLines(file(files[2],open="r")))) # all.data is a character vector
index<-sample.split(newsEN,SplitRatio= .001, group=NULL)
newsEN.sample<-newsEN[index]
news_corpora <- VCorpus(VectorSource(newsEN.sample), readerControl = list(language="en"))
news_corpora <- tm_map(news_corpora, removeNumbers)
news_corpora <- tm_map(news_corpora, removePunctuation)
news_corpora <- tm_map(news_corpora, removeWords, c(stopwords('english')))
news_corpora <- tm_map(news_corpora, stripWhitespace)
news_corpora <- tm_map(news_corpora, content_transformer(tolower))
news_corpora <- tm_map(news_corpora, removeWords, profanityWords$V1)  #Profanity Filter
news_corpora <- tm_map(news_corpora, stemDocument, language='english')
news_corpora <- tm_map(news_corpora, PlainTextDocument)
news_corpora <- tm_map(news_corpora, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=" ")))
tdm <- TermDocumentMatrix(news_corpora)
m <- as.matrix(tdm)
freq <- sort(rowSums(m), decreasing = TRUE)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "News", col = "blue", cex = 1.5)
wordcloud(words = names(freq), freq = freq, min.freq = 2, random.order = FALSE,col=terrain.colors(20), scale=c(8,.2), max.words=300, rot.per=.15)
#+++++++ WordCloud News: END ++++++++++++++++++++++++++++++++++++++++++++++




#+++++++ WordCloud Twitter: START ++++++++++++++++++++++++++++++++++++++++++++++
set.seed(12396911)
twitterEN<-NULL
twitterEN<-append(twitterEN,suppressWarnings(readLines(file(files[3],open="r")))) # all.data is a character vector
index<-sample.split(twitterEN,SplitRatio= .001, group=NULL)
twitterEN.sample<-newsEN[index]
twitter_corpora <- VCorpus(VectorSource(twitterEN.sample), readerControl = list(language="en"))
twitter_corpora <- tm_map(twitter_corpora, removeNumbers)
twitter_corpora <- tm_map(twitter_corpora, removePunctuation)
twitter_corpora <- tm_map(twitter_corpora, removeWords, c(stopwords('english')))
twitter_corpora <- tm_map(twitter_corpora, stripWhitespace)
twitter_corpora <- tm_map(twitter_corpora, content_transformer(tolower))
twitter_corpora <- tm_map(twitter_corpora, removeWords, profanityWords$V1)  #Profanity Filter
twitter_corpora <- tm_map(twitter_corpora, stemDocument, language='english')
twitter_corpora <- tm_map(twitter_corpora, PlainTextDocument)
twitter_corpora <- tm_map(twitter_corpora, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=" ")))
tdm <- TermDocumentMatrix(twitter_corpora)
m <- as.matrix(tdm)
freq <- sort(rowSums(m), decreasing = TRUE)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Twitter", col = "blue", cex = 1.5)
wordcloud(words = names(freq), freq = freq, min.freq = 2, random.order = FALSE,col=terrain.colors(20), scale=c(8,.2), max.words=300, rot.per=.15)
#+++++++ WordCloud Twitter: END ++++++++++++++++++++++++++++++++++++++++++++++


#+++++++ Making tokenizer function
ngramTokenizer <- function(theCorpus, ngramCount) {
  ngramFunction <- NGramTokenizer(theCorpus,Weka_control(min = ngramCount, max = ngramCount,delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngramFunction <- data.frame(table(ngramFunction))
  ngramFunction <- ngramFunction[order(ngramFunction$Freq,decreasing = TRUE),][1:10,]
  colnames(ngramFunction) <- c("String","Count")
  ngramFunction
}


#++++++++++++++++++++ Making n-grams
unigram <- ngramTokenizer(all_corpora, 1)
bigram <- ngramTokenizer(all_corpora, 2)
trigram <- ngramTokenizer(all_corpora, 3)


unigram_Plot <- gvisColumnChart(unigram, "String", "Count",options=list(legend="none"))
plot(unigram_Plot)
