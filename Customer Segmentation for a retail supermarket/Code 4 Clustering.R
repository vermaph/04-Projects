##############################
## Segmentation
##############################
library("rattle")   # For visualizing the random forest tree
library("ggplot2")  # For visualizations
library("cluster")   
library("dplyr")
library("Rtsne")    # For visualizing the clustering in 2-D
library("RODBC")    # For connecting SQL RFM view with R

dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;Server=SCOTT\\SQLEXPRESS; Database=SUPERMARKET;Uid=; Pwd=; trusted_connection=yes")
Customer_Data <- sqlQuery(dbconnection,paste("select * from RFM;"))
odbcClose(dbconnection)

############################## Clustering of the Customers ################################################ 
## Calculate Gower Distance
gower_dist <- daisy(Customer_Data[,-1],metric = "gower", type = list(logratio = c(8:13))) 
# Log transformation for positively skewed variables: FAMILY_TOT_SALES, FAMILY_TOT_VISITS


## Calculate optimal number of clusters
sil_width <- c(NA)
for(i in 2:20){
  pam_fit<-pam(gower_dist, diss = TRUE,k = i)  # PAM: Partitioning Around Medoids 
  sil_width[i]<-pam_fit$silinfo$avg.width
}
tab<-data.frame(x=1:20,sil_width=sil_width)
ggplot(data=tab,aes(x = x,y = sil_width))+geom_point(cex=3,col="red")+geom_line()+ggtitle("Silhoutte Width Vs Number of clusters")+theme(plot.title = element_text(hjust=0.5))+xlab("Number of clusters")


## Creating clusters
pam_fit<-pam(gower_dist, diss=TRUE, k = 7)
Customer_Data<-cbind(Customer_Data, Group = pam_fit$clustering)

## Visualizing the clusters
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = Customer_Data$H_KEY)

ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster)) + ggtitle("Customer Segments") + theme(plot.title = element_text(hjust = 0.5))

result<-Customer_Data %>% 
  group_by(Group) %>% 
  summarize(Avg_sales = mean(ANNUAL_SALES), 
            Avg_visits = mean(ANNUAL_VISITS), 
            Avg_basket_value = mean(ANNUAL_BASKET_VALUE),
            Avg_Recency = mean(RECENCY),
            Avg_Frequency = mean(FREQUENCY),
            Avg_MOnetary = mean(MONETARY)
            ) %>%
  arrange(desc(Avg_sales),desc(Avg_visits))