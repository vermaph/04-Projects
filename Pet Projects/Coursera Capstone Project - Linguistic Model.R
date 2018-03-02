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

k<-length(grep("love",twitter))/length(grep("hate",twitter))   # counts the lines where love/hate occur in twitter file
k

grep("*biostats*",twitter, value = TRUE)
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing",twitter))


## QUiz 2

#++++++++++++++++++++++++++++++++++++++++ Looking into words per line +++++++++++++++++++++++++++++++++++++++++++#

#++++++++ Historgrams
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


#++++++++ Merging all files, taking 10% of all bulk for making a training set i nnext steps and cleaning appropriately 
set.seed(12396911)
all.data<-NULL
for(i in 1:3){
all.data<-append(all.data,readLines(file(files[i],open="r"))) # all.data is a character vector
} 
length(all.data)/10**6 # 3 million lines
sum(as.numeric(nchar(all.data))) / 10**6 # 386 million words
index<-sample.split(all.data,SplitRatio= .05, group=NULL)
all.data.sample<-all.data[index]


sample_corpora <- VCorpus(VectorSource(all.data.sample), readerControl = list(language="en"))
sample_corpora <- tm_map(sample_corpora, removeNumbers)
sample_corpora <- tm_map(sample_corpora, removePunctuation)
sample_corpora <- tm_map(sample_corpora, removeWords, c(stopwords('english')))
sample_corpora <- tm_map(sample_corpora, stripWhitespace)
sample_corpora <- tm_map(sample_corpora, content_transformer(tolower))
#sample_corpora <- tm_map(sample_corpora, removeWords, badWordsList)
sample_corpora <- tm_map(sample_corpora, stemDocument, language='english')
sample_corpora <- tm_map(sample_corpora, PlainTextDocument)
sample_corpora <- tm_map(sample_corpora, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=" ")))


