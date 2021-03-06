###### Music Recommendation Code ######################################
setwd('C:/0000/Cincinnati Projects/Recommender System/')
library("dplyr")
library("tidyr")
library("ggplot2")
library("recommenderlab")
library("readr")
artists <- suppressMessages(read_delim("artists.dat", "\t", escape_double = FALSE, trim_ws = TRUE))
artists<-artists %>%
  mutate(name = gsub("[^[:alnum:][:space:]]","",name)) %>%
  arrange(desc(name)) %>%
  filter(row_number(name)>=637)
valid_artist<-artists$id

tags <- suppressMessages(read_delim("tags.dat", "\t", escape_double = FALSE,trim_ws = TRUE))

user_artists <- suppressMessages(read_delim("user_artists.dat","\t", escape_double = FALSE, trim_ws = TRUE))
user_artists<-user_artists[user_artists$artistID %in% valid_artist,]

user_friends <- suppressMessages(read_delim("user_friends.dat","\t", escape_double = FALSE, trim_ws = TRUE))
user_taggedartists <- suppressMessages(read_delim("user_taggedartists.dat","\t", escape_double = FALSE, trim_ws = TRUE))
user_taggedartists<-user_taggedartists[user_taggedartists$artistID %in% valid_artist,]

user_taggedartists_timestamps <- suppressMessages(read_delim("user_taggedartists-timestamps.dat","\t", escape_double = FALSE, trim_ws = TRUE))
user_taggedartists_timestamps<-user_taggedartists_timestamps[user_taggedartists_timestamps$artistID %in% valid_artist,]



###### EDA ############################################################
# Famous Artists By Play Counts
user_artists %>%
  inner_join(artists, by = c("artistID" = "id")) %>%
  select(userID,name,weight) %>%
  group_by(name) %>%
  summarise(play_counts = round((sum(weight)/1000000),2)) %>%
  arrange(desc(play_counts)) %>%
  top_n(15) %>%
  ggplot(aes(x = reorder(factor(name),-play_counts), y = play_counts, fill = -play_counts)) + geom_bar(stat = "identity") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 0.5)) + ggtitle("Famous Artists by Play Counts") +
  xlab("Artist Names") + ylab("Played in Millions") + guides(fill=FALSE)

# Famous Artists By User Counts
user_artists %>%
  inner_join(artists, by = c("artistID" = "id")) %>%
  select(userID,name) %>%
  group_by(name) %>%
  summarise(user_counts = n()) %>%
  arrange(desc(user_counts)) %>%
  top_n(15) %>%
  ggplot(aes(x = reorder(factor(name),-user_counts), y = user_counts, fill = -user_counts)) + geom_bar(stat = "identity") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 0.5)) + ggtitle("Famous Artists by User Counts") +
  xlab("Artist Names") + ylab("User Counts") + guides(fill=FALSE)

# most artists are listened less than 10 times
user_artists %>%
  select(userID,artistID) %>%
  group_by(artistID) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=count)) + 
  geom_histogram(binwidth = 5, fill = "steelblue") + 
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,4000)) + ggtitle("Artists and their User counts") + theme(plot.title = element_text(hjust=0.5))

# most users listen 50 artists 
user_artists %>%
  select(userID,artistID) %>%
  group_by(userID) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=count)) + 
  geom_histogram(fill = "steelblue") + 
  ggtitle("User and their Artists counts") + 
  theme(plot.title = element_text(hjust=0.5))

# Very few artists who were played more than 5000 times
user_artists %>%
  select(artistID,weight) %>%
  group_by(artistID) %>%
  summarize(play_counts = sum(weight)) %>%
  ggplot(aes(x=play_counts)) + 
  geom_histogram(binwidth = 5, fill = "LightCoral") +
  scale_x_continuous(limits = c(0,500)) + 
  xlab("Number of times played") + ylab("Count of Artists") +
  ggtitle("Total number of times an artist was played by all users \n (Showing for 9827 Less Played Artists)") + theme(plot.title = element_text(hjust=0.5))

user_artists %>%
  select(artistID,weight) %>%
  group_by(artistID) %>%
  summarize(play_counts = sum(weight)) %>%
  ggplot(aes(x=play_counts)) + 
  geom_histogram(binwidth = 5, fill = "LightCoral") +
  scale_x_continuous(limits = c(500,12000)) + 
  scale_y_continuous(limits = c(0,50)) + 
  xlab("Number of times played") + ylab("Count of Artists") +
  ggtitle("Total number of times an artist was played by all users \n (Showing for 6374 Medium Played Artists)") + theme(plot.title = element_text(hjust=0.5))


artists$id<-paste0("A",artists$id)
tags$tagID<-paste0("T",tags$tagID)
user_artists$userID<-paste0("U",user_artists$userID)
user_artists$artistID<-paste0("A",user_artists$artistID)
user_taggedartists$userID<-paste0("U",user_taggedartists$userID)
user_taggedartists$artistID<-paste0("A",user_taggedartists$artistID)
user_taggedartists$tagID<-paste0("T",user_taggedartists$tagID)
  
master<-user_artists %>%
  select(userID,artistID,weight) %>%
  inner_join(user_taggedartists,by=c("userID" = "userID","artistID" = "artistID")) %>%
  inner_join(artists,by = c("artistID"="id")) %>%
  inner_join(tags,by = c("tagID" = "tagID")) %>%
  select(userID,artistID,name,tagID,tagValue,weight)
sub_master<-master[,c(1:2,6)] %>% distinct(userID,artistID,weight)




# Non Binary Version: How many times an user would listen to an artist (on scale of 1-5; 5 being the most and 1 least)
# Creating new rating
quantile(as.vector(sub_master$weight),na.rm = TRUE, probs = seq(0,1,0.2))
sub_master$new_rating<-ifelse(sub_master$weight<139.0,1,  # This equally distributes the 1-5 rating scale across final matrix
                              ifelse(sub_master$weight<=297.0,2,
                                     ifelse(sub_master$weight<=570.0,3,
                                            ifelse(sub_master$weight<=1238.8,4,5))))
# Creating sparse matrix
sub_master_wide_NR<-sub_master %>% # very sparse matrix
  select(userID,artistID,new_rating) %>%
  spread(artistID,new_rating)
sub_master_wide_NR<-sub_master_wide_NR[,colSums(!is.na(sub_master_wide_NR),na.rm = TRUE)>=10] # Artists who were played atleast 10 times by all users
sub_master_wide_NR<-sub_master_wide_NR[rowSums(!is.na(sub_master_wide_NR),na.rm = TRUE)>=6,] # Users who played atleast 6 overall artists 
# By doing these, some users who played some 50 artists, some of those artists were played only by them and hence are removed 
1-(sum(!is.na(sub_master_wide_NR))/sum(is.na(sub_master_wide_NR)))
# Matrix Sparsity = 0.9644405
# Matrix Density = 0.03555949
sub_master_mat_NR<-as.matrix(sub_master_wide_NR[,-1])
sub_rRM<-as(sub_master_mat_NR,"realRatingMatrix")
set.seed(12396911)
eval_non_bin<-evaluationScheme(sub_rRM, method ="split", train=0.8, given = 4, goodRating = 5)
algorithms <- list("random items" = list(name="RANDOM", param=NULL),
                   "popular items" = list(name="POPULAR", param=NULL),
                   "user-based CF" = list(name="UBCF", param=list(nn=32)),   # Calculate within 32 similar users (5% of all users in final matrix)
                   "item-based CF" = list(name="IBCF", param=list(k=20)),    # Calculate within 20 similar items (5% of all artists in final matrix)
                   "SVD approximation" = list(name="SVD", param=list(k = 50))
)
results_non_bin <- evaluate(eval_non_bin, algorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))

# ROC Curves
plot(results_non_bin, annotate=c(1:4), lwd = 2, legend = "topleft")
title(main = "Non-Binary: Comparison of ROC curves for 5 recommender methods")
results<-evaluate(eval_non_bin, algorithms, type = "ratings")
plot(results, ylim = c(0,3.5))
title(main = "Errors in different algorithms: Non-Binary Version")
r1 <- Recommender(getData(eval_non_bin, "train"), "UBCF")    # learned using 511 users
r2 <- Recommender(getData(eval_non_bin, "train"), "POPULAR") # learned using 511 users
r3 <- Recommender(getData(eval_non_bin, "train"), "SVD")     # learned using 511 users
r4 <- Recommender(getData(eval_non_bin, "train"), "IBCF")    # learned using 511 users
r5 <- Recommender(getData(eval_non_bin, "train"), "RANDOM")  # learned using 511 users

p1 <- predict(r1, getData(eval_non_bin, "known"), type="ratings")
p2 <- predict(r2, getData(eval_non_bin, "known"), type="ratings")
p3 <- predict(r3, getData(eval_non_bin, "known"), type="ratings")
p4 <- predict(r4, getData(eval_non_bin, "known"), type="ratings")
p5 <- predict(r5, getData(eval_non_bin, "known"), type="ratings")

# Error between the prediction and the unknown part of the test data
error <- rbind(UBCF = calcPredictionAccuracy(p1, getData(eval_non_bin, "unknown")),
               POPULAR = calcPredictionAccuracy(p2, getData(eval_non_bin, "unknown")),
               SVD = calcPredictionAccuracy(p3, getData(eval_non_bin, "unknown")),
               IBCF = calcPredictionAccuracy(p4, getData(eval_non_bin, "unknown")),
               RANDOM = calcPredictionAccuracy(p5, getData(eval_non_bin, "unknown"))
)
error
# Conclusion: User-Based performs better than popularity-based CF and outperforms rest of all other algorithms




# Binary Version: Whether an user would listen an artist or not
# Creating sparse matrix
sub_master_wide<-sub_master %>% # sub_master_wide: 1st column is userID,  # very sparse matrix
  spread(artistID,weight)
quantile(sub_master$weight, probs = seq(0,1,0.1))
# 0%      10%      20%      30%      40%      50%      60%      70%      80%      90%     100% 
# 1.0     73.6    139.0    211.0    297.0    411.0    570.0    821.0   1238.8   2361.0 352698.0 
nrow(sub_master_wide) # 1820
sum(!is.na(sub_master_wide)) # 12,064,443
sum(is.na(sub_master_wide))
sub_master_wide[,][sub_master_wide[,]<211]<-NA

sub_master_wide<-sub_master_wide[,colSums(!is.na(sub_master_wide),na.rm = TRUE)>=10] # Artists who were played atleast 10 times by all users
sub_master_wide<-sub_master_wide[rowSums(!is.na(sub_master_wide),na.rm = TRUE)>=6,] # Users who played atleast 6 overall artists 
# By doing these, some users who played some 50 artists, some of those artists were played only by them and hence are removed 
1-(sum(!is.na(sub_master_wide))/sum(is.na(sub_master_wide)))
# Matrix Sparsity = 0.9547748
# Matrix Density = 0.04522523

sub_bin<-sub_master_wide
sub_bin<-as.matrix(sub_bin[,-1])
sub_bin[,][is.na(sub_bin[,])]<-0  
sub_bin[,][sub_bin[,]>0]<-1 
sub_bRM<-as(sub_bin,"binaryRatingMatrix")
set.seed(12396911)
eval_bin<-evaluationScheme(sub_bRM, method ="split", train=0.8, given = 4, goodRating = 1) # Give 4 rating to recommender and test 2 for error evaluation
algorithms <- list("random items" = list(name="RANDOM", param=NULL),
                   "popular items" = list(name="POPULAR", param=NULL),
                   "user-based CF" = list(name="UBCF", param=list(nn=32)),   # Calculate within 32 similar users (5 % of all users in final matrix)
                   "item-based CF" = list(name="IBCF", param=list(k=20))     # Calculate within 20 similar items (5% of all artists in final matrix)
                   #,"SVD approximation" = list(name="SVD", param=list(k = 50))
                   # SVD does not implement a method for binary data
                   )
results_bin <- evaluate(eval_bin, algorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))


# ROC Curves
plot(results_bin, annotate=c(1:4), lwd = 2, legend = "topleft")
title(main = "Binary: Comparison of ROC curves for 4 recommender methods")
# Precision Recall Curves
plot(results_bin, "prec/rec", annotate=c(1:4), legend="topleft", lwd = 2)
title(main = "Binary: Comparison of Precision Recall curves for 4 recommender methods")
# Conclusion: User-Based CF outperforms all other algorithms


# Between Binary and Non-Binary
bin_results <- evaluate(eval_bin, method = "UBCF",n = c(2, 3, 5, 7, 10)) # Top 10 recommendations in Binary UBCF
non_bin_results <- evaluate(eval_non_bin, method = "UBCF",n = c(2, 3, 5, 7, 10)) # Top 10 recommendations  in Non-Binary UBCF
# Extract binary confusion matrix metrics
b_conf <- getConfusionMatrix(bin_results)[[1]]
b_conf <- as.data.frame(b_conf)
# Extract listen count confusion matrix metrics
c_conf <- getConfusionMatrix(non_bin_results)[[1]]
c_conf <- as.data.frame(c_conf)


# ROC Comparison: Binary and Non-Binary
# ROC 
plot(y = c_conf$TPR, x = c_conf$FPR, type = "o", col = "blue", xlab = "FPR", ylab = "TPR", xlim=c(0,0.03), ylim=c(0,0.35), lwd = 2)
lines(y = b_conf$TPR, x = b_conf$FPR, col = "red", type = "o", lwd = 2)
# Add a legend
legend(0.020, 0.1, legend=c("Non-Binary", "Binary"),col=c("blue", "red"), lty=1:2, cex=0.8)
title("ROC Comparison: Binary UBCF and Non-Binary UBCF")


############## PREDICTIONS ########################################
###################################################################
###################################################################

# Predictions: Binary and Non-Binary
# Learning Matrix
learningM<-as(as.matrix(sub_master_wide_NR[3:nrow(sub_master_wide_NR),-1]),"realRatingMatrix")
testingM<-as(as.matrix(sub_master_wide_NR[1:2,-1]),"realRatingMatrix")
rec_sys<-Recommender(learningM, method = "UBCF")
pre <- predict(rec_sys, testingM, n=3)
non_bin_list<-as(pre,"list")
# "U10" "U1003"
# [[1]]
# [1] "A1400" "A227"  "A230" 
# 
# [[2]]
# [1] "A89" "A72"


non_bin_list<-non_bin_list[[1]]
new_user<-sub_master_wide_NR[1:2,] %>%
  gather(artistID,new_rating,names(sub_master_wide_NR)[2:381]) %>%
  filter(!is.na(new_rating)) %>%
  inner_join(artists, by = c("artistID" = "id")) %>%
  inner_join(user_taggedartists, by = c("userID" = "userID", "artistID" = "artistID")) %>%
  inner_join(tags, by = c("tagID" = "tagID")) %>%
  select(userID,artistID,name,tagID,tagValue,new_rating) %>%
  arrange(userID,name,tagValue)

new_user_data<-new_user %>%
  distinct(userID,name,tagValue) %>%
  group_by(userID,tagValue) %>%
  summarize(tag_count = n()) %>%
  arrange(userID,desc(tag_count))

# U10: First user was more into indie, alternative rock music and other indie and alternative artists were recommended
# U1003: Second user  was more into Synthpop and electronic and artists like Lady Gaga and Depeche Mode were recommended