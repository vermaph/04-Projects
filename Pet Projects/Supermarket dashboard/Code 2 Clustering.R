##############################
## Clustering of the Customers
##############################

library("ggplot2")
library("dplyr")
library("cluster") 
library("Rtsne")
library("tibble")

## Calculate Gower Distance
glimpse(Customer_Data)
#names(Customer_Data)
#[1] "H_KEY"               "AGE_DESC"            "MARITAL_STATUS_CODE" "INCOME_DESC"         "HOMEOWNER_DESC"     
#[6] "HH_COMP_DESC"        "HOUSEHOLD_SIZE_DESC" "KID_CATEGORY_DESC"   "FAMILY_TOT_SALES"    "FAMILY_TOT_VISITS" 

names<-c(2:8)
Customer_Data[,names]<-lapply(Customer_Data[,names],factor)
gower_dist <- daisy(Customer_Data[,-1],metric = "gower", type = list(logratio = c(8,9,10))) # Log transformation for positively skewed variables: FAMILY_TOT_SALES, FAMILY_TOT_VISITS



## Calculate optimal number of clusters
sil_width <- c(NA)
for(i in 2:20){
pam_fit<-pam(gower_dist, diss = TRUE,k = i)  # PAM: Partitioning Around Medoids 
sil_width[i]<-pam_fit$silinfo$avg.width
}
plot(1:20, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:20, sil_width)

## Creating clusters
pam_fit<-pam(gower_dist, diss=TRUE, k = 8)
Customer_Data<-cbind(Customer_Data, Group = pam_fit$clustering)

## Visualizing the clusters
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = Customer_Data$H_KEY)

ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster)) + ggtitle("Customer Segments") + theme(plot.title = element_text(hjust = 0.5))


result2<-Customer_Data %>% 
  group_by(Group) %>% 
  summarize(Avg_sales = mean(FAMILY_TOT_SALES), Avg_visits = mean(FAMILY_TOT_VISITS)) %>%
  arrange(desc(Avg_sales),desc(Avg_visits))



