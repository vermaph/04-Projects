#########################################################
#                                                       #
#                                                       #
#                                                       #
#                                                       #
#                                                       #
#########################################################



#################
library(tm)
library(knitr)
library(stringi)
library(RWeka)
library(ggplot2)
#################




############################################## Reading files ###########################################
t_line<-c()
t_words<-c()
max_words<-c()

files<-list.files()
for(i in 1:3){
  con<-file(files[i],open = "r")
  text<-suppressWarnings(readLines(con))
  t_line[i]<-length(text) # Counting total number of lines
  t_words[i]<-sum(stri_count_words(text)) # Counting total number of words  
  max_words[i]<-max(stri_count_words(text)) # Counting max number of words in 1 line
  close(con)
}
tab<-data.frame(Files = files[1:3], Total_Lines = t_line, Total_Words = t_words, Max_Word = max_words)
kable(tab[order(tab$Total_Lines,decreasing = TRUE),]) # Output high level info for all the files
########################################################################################################




############################### Cleaning text files ###################################################
final_text_combined<-NULL
for(i in 1:3){
  final_text_combined<-append(final_text_combined,suppressWarnings(readLines(file(files[i],open="r"))))
}
all<-length(final_text_combined)
index<-sample(1:all,0.001*all)
combined_sample<-final_text_combined[index]


# This step removes 16% of bad characters like 
dat2 <- unlist(strsplit(combined_sample, split=", ")) # convert string to vector of words
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2")) # Find indices of words with non-ASCII characters
dat4 <- dat2[-dat3] # Subset original vector of words to exclude words with non-ASCII char
combined_sample <- paste(dat4, collapse = ", ") # Convert vector back to a string, 



profanityWords<-read.table("./profanity_filter.txt", header = FALSE)
removeURL<-function(x) gsub("http[[:alnum:]]*", "", x)
########################################################################################################




############################# Corpora Preparation ######################################################
EN_corpora<-Corpus(VectorSource(combined_sample), readerControl = list(language="en"))

EN_corpora<-tm_map(EN_corpora, removeWords, stopwords('en'))
EN_corpora<-tm_map(EN_corpora, removeWords, stopwords('SMART'))
EN_corpora<-tm_map(EN_corpora, removeWords, stopwords('german'))
EN_corpora<-tm_map(EN_corpora, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
EN_corpora<-tm_map(EN_corpora, content_transformer(tolower))
EN_corpora<-tm_map(EN_corpora, content_transformer(removePunctuation), preserve_intra_word_dashes=TRUE)
EN_corpora<-tm_map(EN_corpora, removeWords, profanityWords$V1) #Profanity Filter
EN_corpora<-tm_map(EN_corpora, content_transformer(removeNumbers))
EN_corpora<-tm_map(EN_corpora, content_transformer(removeURL))
EN_corpora<-tm_map(EN_corpora, stemDocument, language='english')
EN_corpora<-tm_map(EN_corpora, stripWhitespace)
EN_corpora<-tm_map(EN_corpora, PlainTextDocument)


############################ Word Frequnecy Barplots ###################################################
unigram<-NGramTokenizer(EN_corpora, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
unigram<-data.frame(table(unigram))
unigram<-unigram[order(unigram$Freq,decreasing = TRUE),]
names(unigram)<-c("Word_1", "Freq")
unigram$Word_1<-as.character(unigram$Word_1)
g<-ggplot(data=unigram[1:15,], aes(x = reorder(Word_1,Freq), y = Freq))
g<-g + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequent Words")
g<-g + geom_text(data = unigram[1:15,], aes(x = Word_1, y = Freq, label = Freq), hjust=-1, position = "identity")
g<-g + labs(x="Frequency",y="Words")
g
########################################################################################################
